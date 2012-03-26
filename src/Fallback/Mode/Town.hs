{- ============================================================================
| Copyright 2011 Matthew D. Steele <mdsteele@alum.mit.edu>                    |
|                                                                             |
| This file is part of Fallback.                                              |
|                                                                             |
| Fallback is free software: you can redistribute it and/or modify it under   |
| the terms of the GNU General Public License as published by the Free        |
| Software Foundation, either version 3 of the License, or (at your option)   |
| any later version.                                                          |
|                                                                             |
| Fallback is distributed in the hope that it will be useful, but WITHOUT     |
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or       |
| FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for   |
| more details.                                                               |
|                                                                             |
| You should have received a copy of the GNU General Public License along     |
| with Fallback.  If not, see <http://www.gnu.org/licenses/>.                 |
============================================================================ -}

{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Fallback.Mode.Town
  (newTownMode)
where

import Control.Applicative ((<$), (<$>))
import Control.Monad (forM_, guard, when)
import Data.List (delete, partition)
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.IORef
import qualified Data.Set as Set

import Fallback.Constants
  (baseFramesPerActionPoint, combatArenaCols, combatArenaRows,
   momentsPerActionPoint)
import Fallback.Control.Script
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
  (Point(Point), Position, half, makeRect, plusDir, pSqDist, pSub)
import qualified Fallback.Data.SparseMap as SM
import Fallback.Data.TotalMap (tmGet, unfoldTotalMap)
import Fallback.Draw (handleScreen, paintScreen)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog (newTextEntryDialogMode)
import Fallback.Mode.Error (popupIfErrors)
import Fallback.Mode.LoadGame (newLoadGameMode)
import Fallback.Mode.MultiChoice (newMultiChoiceMode)
import Fallback.Mode.Narrate (newNarrateMode)
import Fallback.Scenario.Abilities (getAbility)
import Fallback.Scenario.Areas (enterPartyIntoArea)
import Fallback.Scenario.MonsterAI (monsterTownStep)
import Fallback.Scenario.Potions (runPotionAction)
import Fallback.Scenario.Script
  (addToCharacterAdrenaline, alsoWith, concurrentAny, grantExperience,
   inflictAllPeriodicDamage, partyWalkTo, setMessage, teleport)
import Fallback.Sound (playSound)
import Fallback.State.Action
import Fallback.State.Area
import Fallback.State.Combat
  (CombatCharState(..), CombatState(..), CombatPhase(WaitingPhase))
import Fallback.State.Creature (CreatureAnim(..))
import Fallback.State.Item (getPotionAction)
import Fallback.State.Party
import Fallback.State.Region (RegionState(..))
import Fallback.State.Resources
  (Resources, SoundTag(SndCombatStart), rsrcSound)
import Fallback.State.Simple (CastingCost, deltaFaceDir)
import Fallback.State.Tags (AreaTag, ItemTag(PotionItemTag))
import Fallback.State.Town
import Fallback.View (fromAction, viewHandler, viewPaint)
import Fallback.View.Abilities (AbilitiesAction(..))
import Fallback.View.Inventory (InventoryAction(..))
import Fallback.View.Sidebar (SidebarAction(..))
import Fallback.View.Town
import Fallback.View.Upgrade (UpgradeAction(..))

-------------------------------------------------------------------------------

newTownMode :: Resources -> Modes -> TownState -> IO Mode
newTownMode resources modes initState = do
  view <- newTownView resources
  stateRef <- newIORef initState
  let

    mode EvQuit = return DoQuit -- TODO offer to save game
    mode (EvKeyDown KeyO [KeyModCmd] _) = do
      ts <- readIORef stateRef
      ChangeMode <$> newLoadGameMode resources modes mode view ts
    mode event = do
      mbInterrupt <- if (event /= EvTick) then return Nothing else do
        (ts', mbInt) <- readIORef stateRef >>= doTick
        mbInt <$ writeIORef stateRef ts'
      ts <- readIORef stateRef
      action <- handleScreen $ viewHandler view ts event
      when (event == EvTick) $ paintScreen (viewPaint view ts)
      case mbInterrupt of
        Just (DoExit area) -> do
          let party = arsParty ts
          let restore char = char { chrAdrenaline = 0,
                                    chrHealth = chrMaxHealth party char,
                                    chrMana = chrMaxMana party char }
          let party' = party { partyCharacters =
                                 restore <$> partyCharacters party }
          ChangeMode <$> newRegionMode' modes RegionState
            { rsClock = arsClock ts,
              rsParty = party',
              rsPreviousArea = partyCurrentArea party',
              rsRegion = partyCurrentRegion party',
              rsSelectedArea = area }
        Just DoGameOver -> ChangeMode <$> newGameOverMode' modes
        Just (DoMultiChoice text choices cancel sfn) -> do
          fmap ChangeMode $ newMultiChoiceMode resources view ts text choices
                                               cancel $ \choice -> do
            modifyIORef stateRef $
              \ts' -> ts' { tsPhase = ScriptPhase (sfn choice) }
            return mode
        Just (DoNarrate text script) -> do
          fmap ChangeMode $ newNarrateMode resources view ts text $ do
            modifyIORef stateRef $ \ts' -> ts' { tsPhase = ScriptPhase script }
            return mode
        Just DoStartCombat -> doStartCombat ts
        Just (DoTeleport tag pos) -> do
          popupIfErrors resources view ts (return mode)
                        (enterPartyIntoArea resources (arsParty ts) tag pos) $
                        \ts' -> do
            writeIORef stateRef ts'
            handleAction action
        Just (DoWait script) -> do
          writeIORef stateRef $
            (tickTownAnimations ts) { tsPhase = ScriptPhase script }
          handleAction action
        Nothing -> handleAction action

    handleAction action = do
      ts <- readIORef stateRef
      case fromAction action of
        Just (TownSidebar EnterCheatCode) -> do
          case tsPhase ts of
            WalkingPhase -> do
              ChangeMode <$> newTextEntryDialogMode resources
                "What do you want?" "" (const True) (return mode)
                (tryCheating ts) view ts
            _ -> ignore
        Just (TownSidebar (MakeCharacterActive charNum)) -> do
          changeState ts { tsActiveCharacter = charNum }
        Just (TownSidebar ToggleAbilities) -> do
          case tsPhase ts of
            WalkingPhase ->
              changeState ts { tsPhase = ChooseAbilityPhase }
            ChooseAbilityPhase ->
              changeState ts { tsPhase = WalkingPhase }
            InventoryPhase Nothing ->
              changeState ts { tsPhase = ChooseAbilityPhase }
            InventoryPhase (Just _) -> ignore
            UpgradePhase _ _ -> ignore
            TargetingPhase _ -> ignore
            ScriptPhase _ -> ignore
        Just (TownSidebar ToggleInventory) -> do
          case tsPhase ts of
            WalkingPhase ->
              changeState ts { tsPhase = InventoryPhase Nothing }
            ChooseAbilityPhase ->
              changeState ts { tsPhase = InventoryPhase Nothing }
            InventoryPhase Nothing ->
              changeState ts { tsPhase = WalkingPhase }
            InventoryPhase (Just _) -> ignore
            UpgradePhase _ _ -> ignore
            TargetingPhase _ -> ignore
            ScriptPhase _ -> ignore
        Just (TownSidebar TryToggleCombat) -> do
          case tsPhase ts of
            WalkingPhase -> tryToManuallyStartCombat ts
            _ -> ignore
--         Just (TownSidebar (UseAbility abilNum)) -> do
--           let party = tsParty ts
--           let charNum = tsActiveCharacter ts
--           let char = partyGetCharacter party charNum
--           (SameMode <$) $ fromMaybe (return ()) $ do
--             guard $ case tsPhase ts of
--                       { ChooseAbilityPhase -> True; _ -> False }
--             level <- tmGet abilNum (chrAbilities char)
--             let ability = getAbility (chrClass char) abilNum level
--             case abKind ability of
--               ActiveAbility cost effect -> do
--                 guard (partyCanAffordCastingCost charNum cost party)
--                 case effect of
--                   GeneralAbility target sfn -> Just $ writeIORef stateRef $
--                     case target of
--                       AllyTarget r -> setTargeting (TargetingAlly r)
--                       AreaTarget r f -> setTargeting (TargetingArea r f)
--                       AutoTarget -> ts { tsPhase = ScriptPhase $ sfn' () }
--                       MultiTarget r n -> setTargeting (TargetingMulti r n [])
--                       SingleTarget r -> setTargeting (TargetingSingle r)
--                     where
--                       sfn' = mapEffect EffTownArea . sfn charNum 1
--                       setTargeting targeting =
--                         ts { tsPhase = TargetingPhase $
--                           TownTargeting cost sfn' targeting }
--                   _ -> Nothing
--               PassiveAbility -> Nothing
--         Just (TownSidebar (UseCombatFeat _)) -> do
--           -- TODO: We should probably allow meta-abilities in town mode.
--           ignore

--         Just (TownInteract entry) -> do
--           case tsPhase ts of
--             WalkingPhase -> do
--               let pos = rectTopleft (geRect entry)
--               if Set.notMember pos (tsVisible ts) then ignore else do
--               let device = geValue entry
--               if pos `pSqDist` tsPartyPosition ts > devRange device then
--                 ignore else do
--               changeState ts { tsPhase = ScriptPhase $ mapEffect EffTownArea $
--                                          devInteract device entry $
--                                          tsActiveCharacter ts }
--             _ -> ignore
        Just (TownAbilities abilAct) -> do
          case tsPhase ts of
            ChooseAbilityPhase -> do
              case abilAct of
                UpgradeSkills -> startUpgradePhase ts
                UseAbility abilNum -> do
                  let party = arsParty ts
                  let charNum = tsActiveCharacter ts
                  let char = partyGetCharacter party charNum
                  fromMaybe ignore $ do
                    level <- tmGet abilNum (chrAbilities char)
                    case getAbility (chrClass char) abilNum level of
                      ActiveAbility cost effect -> do
                        guard (partyCanAffordCastingCost charNum cost party)
                        case effect of
                          GeneralAbility target sfn -> Just $ do
                            let sfn' = mapEffect EffTownArea . sfn charNum 1
                            let setTarget targ = changeState ts
                                  { tsPhase = TargetingPhase $
                                              TownTargeting cost sfn' targ }
                            (case target of
                               AllyTarget r -> setTarget (TargetingAlly r)
                               AreaTarget r f -> setTarget (TargetingArea r f)
                               AutoTarget -> changeState ts
                                 { tsPhase = ScriptPhase $ sfn' () }
                               MultiTarget r n ->
                                 setTarget (TargetingMulti r n [])
                               SingleTarget r ->
                                 setTarget (TargetingSingle r)) :: IO NextMode
                          CombatAbility _ _ -> Nothing
                          MetaAttack _ _ _ -> Nothing
                      PassiveAbility -> Nothing
                -- TODO: We should probably allow meta-abilities in town mode
                UseCombatFeat _ -> ignore
            _ -> ignore
        Just (TownInventory invAct) -> do
          case tsPhase ts of
            InventoryPhase mbItemTag -> do
              case invAct of
                ExchangeItem slot -> do
                  case partyTryExchangeItem slot mbItemTag (arsParty ts) of
                    Nothing -> ignore
                    Just (mbItemTag', party') -> do
                      changeState ts
                        { tsCommon = (tsCommon ts) { acsParty = party' },
                          tsPhase = InventoryPhase mbItemTag' }
                UpgradeStats -> do
                  if isJust mbItemTag then ignore else do
                  startUpgradePhase ts
                UseItem slot -> do
                  let party = arsParty ts
                  if isJust mbItemTag then ignore else do
                  case partyItemInSlot slot party of
                    Just (PotionItemTag potTag) -> do
                      let phase = ScriptPhase $ mapEffect EffTownArea $
                                  runPotionAction (getPotionAction potTag) $
                                  tsActiveCharacter ts
                      changePhaseAndParty ts phase (partyRemoveItem slot party)
                    _ -> ignore
            _ -> ignore
        Just (TownUpgrade upgAct) -> do
          case tsPhase ts of
            UpgradePhase st sk -> do
              let charNum = tsActiveCharacter ts
              case upgAct of
                IncreaseSkill abilNum -> do
                  let sk' = SM.adjust (+1) (charNum, abilNum) sk
                  changeState ts { tsPhase = UpgradePhase st sk' }
                DecreaseSkill abilNum -> do
                  let sk' = SM.adjust (subtract 1) (charNum, abilNum) sk
                  changeState ts { tsPhase = UpgradePhase st sk' }
                IncreaseStat stat -> do
                  let st' = SM.adjust (+1) (charNum, stat) st
                  changeState ts { tsPhase = UpgradePhase st' sk }
                DecreaseStat stat -> do
                  let st' = SM.adjust (subtract 1) (charNum, stat) st
                  changeState ts { tsPhase = UpgradePhase st' sk }
                CancelUpgrades -> changeState ts { tsPhase = WalkingPhase }
                CommitUpgrades ->
                  changePhaseAndParty ts WalkingPhase $
                  partySpendUpgrades st sk (arsParty ts)
            _ -> ignore
        Just (TownScript script) -> do
          case tsPhase ts of
            WalkingPhase -> changeState ts { tsPhase = ScriptPhase script }
            _ -> ignore
        Just (TownMove dir) -> do
          case tsPhase ts of
            WalkingPhase -> do
              let pos = tsPartyPosition ts
              let pos' = pos `plusDir` dir
              if arsIsBlockedForParty ts pos' then ignore else do
              let acs = tsCommon ts
              fields' <- decayFields baseFramesPerActionPoint (acsFields acs)
              let script = do
                    forM_ [minBound .. maxBound] $ \charNum -> do
                      addToCharacterAdrenaline (negate 1) charNum
                    startCombat <-
                      alsoWith (flip const) (partyWalkTo pos') $
                      concurrentAny (Grid.entries $ acsMonsters acs) $
                      monsterTownStep
                    inflictAllPeriodicDamage
                    when startCombat $ emitEffect EffStartCombat
              changeState ts { tsCommon = acs { acsFields = fields' },
                               tsPhase = ScriptPhase script }
            _ -> ignore
        Just (TownTargetCharacter charNum) -> do
          case tsPhase ts of
            TargetingPhase targeting -> do
              (case targeting of
                 TownTargeting { ttTargeting = TargetingAlly _,
                                 ttCastingCost = cost, ttScriptFn = sfn } -> do
                   -- No need to check against the casting range, because we're
                   -- in town mode, so all characters are together.
                   executeAbility ts cost $ sfn $ Right charNum
                 _ -> ignore) :: IO NextMode
            _ -> ignore
        Just (TownTargetPosition pos) -> do
          (case tsPhase ts of
             TargetingPhase TownTargeting { ttTargeting = targeting,
                                            ttCastingCost = cost,
                                            ttScriptFn = sfn } ->
               case targeting of
                 TargetingAlly rng ->
                   if cannotHit rng then ignore else execute (sfn $ Left pos)
                 TargetingArea rng areaFn ->
                   if cannotHit rng then ignore else
                     execute $ sfn (pos, areaFn ts originPos pos)
                 TargetingMulti rng n ps ->
                   if pos `elem` ps then switch (delete pos ps) else
                     if cannotHit rng then ignore else
                       let ps' = pos : ps
                       in if length ps' < n then switch ps'
                          else execute (sfn ps')
                   where
                     switch ps' = SameMode <$ writeIORef stateRef ts
                       { tsPhase = TargetingPhase TownTargeting
                         { ttTargeting = TargetingMulti rng n ps',
                           ttCastingCost = cost, ttScriptFn = sfn } }
                 TargetingSingle rng ->
                   if cannotHit rng then ignore
                   else execute (sfn pos)
               where
                 cannotHit rng =
                   pSqDist pos originPos > rng ||
                   not (arsIsVisibleToCharacter (tsActiveCharacter ts) ts pos)
                 execute = executeAbility ts cost
                 originPos = tsPartyPosition ts
             _ -> ignore) :: IO NextMode
        Just TownCancelTargeting -> do
          case tsPhase ts of
            TargetingPhase _ -> do
              changeState ts { tsPhase = WalkingPhase }
            _ -> ignore
        Just _ -> return SameMode -- FIXME handle other actions
        Nothing -> return SameMode

    executeAbility :: TownState -> CastingCost -> Script TownEffect ()
                   -> IO NextMode
    executeAbility ts cost script =
      changePhaseAndParty ts (ScriptPhase script) $
      partyDeductCastingCost (tsActiveCharacter ts) cost (arsParty ts)

    startUpgradePhase :: TownState -> IO NextMode
    startUpgradePhase ts = do
      changeState ts { tsPhase = UpgradePhase (SM.make 0) (SM.make 0) }

    tryToManuallyStartCombat :: TownState -> IO NextMode
    tryToManuallyStartCombat ts = do
      if arsAreMonstersNearby ts then doStartCombat ts else do
        let msg = "Can't start combat -- there are no enemies nearby."
        writeIORef stateRef $ arsSetMessage msg ts
        return SameMode

    doStartCombat :: TownState -> IO NextMode
    doStartCombat ts = do
      let pp = tsPartyPosition ts
      let arenaTopleft =
            pp `pSub` Point (half combatArenaCols) (half combatArenaRows)
      let arenaRect = makeRect arenaTopleft (combatArenaCols, combatArenaRows)
      let mkCharState _charNum claimed =
            let pos = arsFindOpenSpot ts pp arenaRect claimed
                ccs = CombatCharState
                        { ccsAnim = WalkAnim 6 6 pp,
                          ccsFaceDir = deltaFaceDir (pos `pSub` pp),
                          ccsMoments = 2 * momentsPerActionPoint,
                          ccsPosition = pos,
                          ccsVisible = Set.empty,
                          ccsWantsTurn = False }
            in (ccs, Set.insert pos claimed)
      let (combatMonsts, otherMonsts) = Grid.excise arenaRect (arsMonsters ts)
      let acs' = (tsCommon ts) { acsMonsters = combatMonsts }
      playSound (rsrcSound resources SndCombatStart)
      ChangeMode <$> newCombatMode' modes CombatState
        { csArenaTopleft = arenaTopleft,
          csCommon = acs',
          csCharStates = unfoldTotalMap mkCharState Set.empty,
          csMonstersNotInArena = otherMonsts,
          csPeriodicTimer = 0,
          csPhase = WaitingPhase,
          csTownFiredTriggerIds =
            Set.fromList $ map triggerId $ tsTriggersFired ts,
          csTriggers = [] } -- FIXME

    tryCheating :: TownState -> String -> IO Mode
    tryCheating ts string = do
      case reads string of
        [(cheat, "")] -> do
          let runScript script = do
                writeIORef stateRef ts { tsPhase = ScriptPhase script }
          case cheat of
            IAmLeTired -> runScript $ return () -- TODO heal party
            Plugh xp -> runScript $ grantExperience xp
            WhereAmI -> runScript $ setMessage $ show $ tsPartyPosition ts
            Xyzzy area x y -> runScript $ teleport area (Point x y)
        _ -> do
          let msg = "A hollow voice says, \"Fool!\""
          writeIORef stateRef $ arsSetMessage msg ts
      return mode

    changePhaseAndParty :: TownState -> TownPhase -> Party -> IO NextMode
    changePhaseAndParty ts phase' party' = do
      let acs' = (tsCommon ts) { acsParty = party' }
      changeState ts { tsCommon = acs', tsPhase = phase' }

    changeState :: TownState -> IO NextMode
    changeState ts' = SameMode <$ writeIORef stateRef ts'

    ignore :: IO NextMode
    ignore = return SameMode

  -- FIXME: Bug!  When we "return mode" in various places above, we will no
  -- longer be returning the focusBlurMode.
  focusBlurMode (readIORef stateRef) view mode

-------------------------------------------------------------------------------

-- | An 'Interrupt' indicates that we have executed a statement that requires
-- us to pause execution and switch to another 'Mode'.
data Interrupt = DoExit AreaTag
               | DoGameOver
               | forall a. DoMultiChoice String [(String, a)] (Maybe a)
                                         (a -> Script TownEffect ())
               | DoNarrate String (Script TownEffect ())
               | DoStartCombat
--                | forall a. DoTalk (Script TalkEffect a)
--                                   (a -> Script TownEffect ())
               | DoTeleport AreaTag Position
               | DoWait (Script TownEffect ())

-- | Perform our once-per-tick update of the 'TownState', which may include
-- executing triggers or the pending script.
doTick :: TownState -> IO (TownState, Maybe Interrupt)
doTick ts =
  case tsPhase ts of
    WalkingPhase -> updateState ts
    ChooseAbilityPhase -> return (tickTownAnimations ts, Nothing)
    InventoryPhase _ -> return (tickTownAnimations ts, Nothing)
    UpgradePhase _ _ -> return (tickTownAnimations ts, Nothing)
    TargetingPhase _ -> return (tickTownAnimations ts, Nothing)
    -- If there is a pending script, we need to finish it.  Once it's all done,
    -- we'll move on to updateState (unless we get interrupted first, in which
    -- case the updateState will have to wait for the next tick).
    ScriptPhase script -> executeScript updateState ts script

updateState :: TownState -> IO (TownState, Maybe Interrupt)
updateState ts = do
  (ts', mbInterrupt) <- executeTriggers ts
  return (if isNothing mbInterrupt then tickTownAnimations ts' else ts',
          mbInterrupt)

-- | Execute ready triggers one at a time until either no more are eligible to
-- run or one of them interrupts.  If more than one trigger is eligible at
-- once, no guarantees are made about which one runs first, and the first
-- trigger's effects may well cause the second trigger to no longer be eligible
-- and therefore to not run.
--
-- Only triggers in the \"ready\" set can run.  Once a ready trigger runs, it
-- is placed into the \"fired\" set, and does not return to the ready set until
-- its predicate evaluates to false at least once.
executeTriggers :: TownState -> IO (TownState, Maybe Interrupt)
executeTriggers ts = do
  let isActive = flip triggerPredicate ts
  let (stillFired, newlyReady) = partition isActive (tsTriggersFired ts)
  let (inactive, possiblyActive) = break isActive (tsTriggersReady ts)
  case possiblyActive of
    (activeTrigger : remaining) ->
      let ts' = ts { tsTriggersFired = activeTrigger : stillFired,
                     tsTriggersReady = inactive ++ remaining ++ newlyReady }
      in executeScript executeTriggers ts' (triggerAction activeTrigger)
    [] -> return (ts { tsTriggersFired = stillFired,
                       tsTriggersReady = inactive ++ newlyReady }, Nothing)

executeScript :: (TownState -> IO (TownState, Maybe Interrupt)) -> TownState
              -> Script TownEffect () -> IO (TownState, Maybe Interrupt)
executeScript done ts script = do
  case execScript script of
    ResultFinal () -> done ts { tsPhase = WalkingPhase }
    ResultEffect eff sfn -> do
      (ts', interruptOrScript) <- executeEffect ts eff sfn
      case interruptOrScript of
        Left interrupt -> return (ts', Just interrupt)
        Right script' -> executeScript done ts' script'

executeEffect :: TownState -> TownEffect a -> (a -> Script TownEffect ())
              -> IO (TownState, Either Interrupt (Script TownEffect ()))
executeEffect ts eff sfn =
  case eff of
    EffExitTowardArea tag -> return (ts, Left $ DoExit tag)
    EffGetPartyPosition -> return (ts, Right $ sfn $ tsPartyPosition ts)
    EffSetPartyAnim anim -> return (ts { tsPartyAnim = anim }, Right $ sfn ())
    EffSetPartyFaceDir dir -> do
      return (ts { tsPartyFaceDir = dir }, Right $ sfn ())
    EffSetPartyPosition dest -> do
      ts' <- updateTownVisibility ts { tsPartyPosition = dest }
      return (ts', Right $ sfn ())
    EffShop _ -> do fail "FIXME EffShop"
    EffStartCombat -> return (ts, Left DoStartCombat)
    EffTeleportToArea tag pos -> return (ts, Left $ DoTeleport tag pos)
    EffTownArea eff' ->
      case eff' of
        EffAreaCommon eff'' -> do
          (result, ts') <- executeAreaCommonEffect eff'' ts
          return (ts', Right $ sfn result)
        EffGameOver -> return (ts, Left DoGameOver)
        EffIfCombat _ script -> return (ts, Right (script >>= sfn))
        EffMultiChoice text choices cancel ->
          return (ts, Left $ DoMultiChoice text choices cancel sfn)
        EffNarrate text -> return (ts, Left $ DoNarrate text $ sfn ())
        EffWait -> return (ts, Left $ DoWait $ sfn ())

-------------------------------------------------------------------------------
