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
import Data.List (delete, find, partition)
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.IORef
import qualified Data.Set as Set

import Fallback.Constants
  (baseFramesPerActionPoint, combatArenaCols, combatArenaRows, combatArenaSize,
   momentsPerActionPoint, screenRect)
import Fallback.Control.Script
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
  (Point(Point), Position, half, makeRect, ofRadius, plusDir, pSqDist, pSub,
   rectContains)
import qualified Fallback.Data.SparseMap as SM
import qualified Fallback.Data.TotalMap as TM (get, unfold)
import Fallback.Draw (handleScreen, paintScreen, takeScreenshot)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog (newTextEntryDialogMode)
import Fallback.Mode.Error (popupIfErrors)
import Fallback.Mode.GameMenu (GameMenuState(TownMenuState), newGameMenuMode)
import Fallback.Mode.LoadGame (newLoadGameMode)
import Fallback.Mode.MultiChoice (newMultiChoiceMode)
import Fallback.Mode.Narrate (newNarrateMode)
import Fallback.Scenario.Abilities (getAbility)
import Fallback.Scenario.Areas (enterPartyIntoArea)
import Fallback.Scenario.MonsterAI (monsterTownStep)
import Fallback.Scenario.Potions (runPotionAction)
import Fallback.Scenario.Script
  (also_, alsoWith, alterAdrenaline, areaGet, concurrentAny, grantExperience,
   grantItem, inflictAllPeriodicDamage, partyWalkTo, setMessage, teleport,
   tickSummonsByOneRound)
import Fallback.Scenario.Triggers (getAreaExits, scenarioTriggers)
import Fallback.Scenario.Triggers.Script (startShopping)
import Fallback.Sound (playSound)
import Fallback.State.Action
import Fallback.State.Area
import Fallback.State.Combat
  (CombatCharState(..), CombatState(..), CombatPhase(WaitingPhase))
import Fallback.State.Creature (CreatureAnim(..), CreaturePose(..))
import Fallback.State.Item
  (ItemValue(..), getPotionAction, itemCost, itemValue)
import Fallback.State.Party
import Fallback.State.Region (RegionState(..))
import Fallback.State.Resources
  (Resources, SoundTag(SndCombatStart), rsrcSound)
import Fallback.State.Simple
  (CastingCost, Ingredient, ItemSlot, deltaFaceDir, ingredientCost)
import Fallback.State.Tags (AreaTag, ItemTag(PotionItemTag), allItemTags)
import Fallback.State.Town
import Fallback.Utility (flip3)
import Fallback.View (fromAction, viewHandler, viewPaint)
import Fallback.View.Abilities (AbilitiesAction(..))
import Fallback.View.Inventory (InventoryAction(..), ShoppingAction(..))
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
                                    chrMojo = chrMaxMojo party char }
          let party' = party { partyCharacters =
                                 restore <$> partyCharacters party }
          ChangeMode <$> newRegionMode' modes RegionState
            { rsClock = arsClock ts,
              rsParty = party',
              rsPreviousArea = partyCurrentArea party',
              rsRegion = partyCurrentRegion party',
              rsSelectedArea = area,
              rsUnsaved = True }
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
        Just (DoShopping forsale script) -> do
          modifyIORef stateRef $ \ts' ->
            ts' { tsPhase = ShoppingPhase Nothing forsale script }
          handleAction action
        Just (DoStartCombat topleft) -> doStartCombat ts topleft
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
          case tsPhase ts of
            TargetingPhase _ -> ignore
            ScriptPhase _ -> ignore
            _ -> changeState ts { tsActiveCharacter = charNum }
        Just (TownSidebar (QueryMinimap _)) -> ignore -- TODO?
        Just (TownSidebar ShowMenu) -> do
          case tsPhase ts of
            WalkingPhase -> do
              screenshot <- takeScreenshot screenRect
              let onDone ts' = do
                    writeIORef stateRef ts'
                    return mode
              ChangeMode <$> newGameMenuMode resources modes screenshot
                                             (TownMenuState ts) onDone view
            _ -> ignore
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
            ShoppingPhase _ _ _ -> ignore
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
            ShoppingPhase _ _ _ -> ignore
        Just (TownSidebar TryToggleCombat) -> do
          case tsPhase ts of
            WalkingPhase -> tryToManuallyStartCombat ts
            _ -> ignore
        Just (TownAbilities abilAct) -> do
          case tsPhase ts of
            ChooseAbilityPhase -> do
              case abilAct of
                UseAbility abilNum -> do
                  let party = arsParty ts
                  let charNum = tsActiveCharacter ts
                  let char = partyGetCharacter party charNum
                  fromMaybe ignore $ do
                    abilRank <- TM.get abilNum (chrAbilities char)
                    case getAbility (chrClass char) abilNum abilRank of
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
                               AreaTarget f r -> setTarget (TargetingArea f r)
                               AutoTarget -> changeState ts
                                 { tsPhase = ScriptPhase $ sfn' () }
                               JumpTarget f r ->
                                 setTarget $ TargetingJump f $
                                 arsCharacterJumpDestinations r charNum ts
                               MultiTarget n r ->
                                 setTarget (TargetingMulti n r [])
                               SingleTarget r ->
                                 setTarget (TargetingSingle r)) :: IO NextMode
                          CombatAbility _ _ -> Nothing
                          MetaAttack _ _ _ -> Nothing
                      PassiveAbility -> Nothing
                -- TODO: We should probably allow meta-abilities in town mode
                UseCombatFeat _ -> ignore
                UseNormalAttack -> ignore
            _ -> ignore
        Just (TownInventory invAct) -> do
          case tsPhase ts of
            InventoryPhase mbItemTag -> do
              case invAct of
                ExchangeItem slot -> do
                  trySwapItem ts mbItemTag slot InventoryPhase
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
                DoneInventory -> do
                  if isJust mbItemTag then ignore else do
                  changeState ts { tsPhase = WalkingPhase }
            _ -> ignore
        Just (TownShopping shopAct) -> do
          case tsPhase ts of
            ShoppingPhase mbItemTag forSale onDone -> do
              case shopAct of
                BuyIngredient quantity ing -> do
                  if isJust mbItemTag then ignore else do
                  let party = arsParty ts
                  if quantity + TM.get ing (partyIngredients party) >
                     partyMaxIngredientCount party then ignore else do
                  tryBuy ts (toInteger quantity * ingredientCost ing)
                         (partyGrantIngredient quantity ing)
                BuyItem tag -> do
                  if isJust mbItemTag then ignore else do
                  tryBuy ts (itemCost tag) (partyGrantItem tag)
                SellItem slot -> do
                  if isJust mbItemTag then ignore else do
                  let party = arsParty ts
                  case partyItemInSlot slot party of
                    Nothing -> ignore
                    Just tag -> do
                      case itemValue tag of
                        CannotSell -> ignore
                        CanSell value -> do
                          changeParty ts $ partyRemoveItem slot $
                            party { partyCoins = partyCoins party + value }
                SwapItem slot -> trySwapItem ts mbItemTag slot $
                                 flip3 ShoppingPhase forSale onDone
                DoneShopping -> do
                  if isJust mbItemTag then ignore else do
                  changeState ts { tsPhase = ScriptPhase onDone }
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
              changeState ts { tsCommon = acs { acsFields = fields' },
                               tsPhase = ScriptPhase (doTownStep pos') }
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
                 TargetingArea areaFn rng -> do
                   if cannotHit rng then ignore else do
                   let targets = areaFn ts originPos pos
                   if null targets then ignore else do
                   execute $ sfn (pos, targets)
                 TargetingJump areaFn targetable -> do
                   if Set.notMember pos targetable then ignore else do
                   execute $ sfn (pos, areaFn ts originPos pos)
                 TargetingMulti n rng ps ->
                   if pos `elem` ps then switch (delete pos ps) else
                     if cannotHit rng then ignore else
                       let ps' = pos : ps
                       in if length ps' < n then switch ps'
                          else execute (sfn ps')
                   where
                     switch ps' = SameMode <$ writeIORef stateRef ts
                       { tsPhase = TargetingPhase TownTargeting
                         { ttTargeting = TargetingMulti n rng ps',
                           ttCastingCost = cost, ttScriptFn = sfn } }
                 TargetingSingle rng ->
                   if cannotHit rng then ignore
                   else execute (sfn pos)
               where
                 cannotHit rng =
                   pSqDist pos originPos > ofRadius rng ||
                   not (arsIsVisibleToCharacter (tsActiveCharacter ts) ts pos)
                 execute = executeAbility ts cost
                 originPos = tsPartyPosition ts
             _ -> ignore) :: IO NextMode
        Just TownCancelTargeting -> do
          case tsPhase ts of
            TargetingPhase _ -> do
              changeState ts { tsPhase = WalkingPhase }
            _ -> ignore
        Nothing -> ignore

    tryBuy :: TownState -> Integer -> (Party -> Party) -> IO NextMode
    tryBuy ts cost partyFn = do
      let party = arsParty ts
      let coins' = partyCoins party - cost
      if coins' < 0 then ignore else do
      changeParty ts $ partyFn party { partyCoins = coins' }

    trySwapItem :: TownState -> Maybe ItemTag -> ItemSlot
                -> (Maybe ItemTag -> TownPhase) -> IO NextMode
    trySwapItem ts mbItemTag slot phaseFn = do
      case partyTryExchangeItem slot mbItemTag (arsParty ts) of
        Nothing -> ignore
        Just (mbItemTag', party') -> do
          changeState ts { tsCommon = (tsCommon ts) { acsParty = party' },
                           tsPhase = phaseFn mbItemTag' }

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
      if arsAreMonstersNearby ts then do
        doStartCombat ts (tsPartyPosition ts `pSub`
                          Point (half combatArenaCols) (half combatArenaRows))
       else do
        let msg = "Can't start combat -- there are no enemies nearby."
        writeIORef stateRef $ arsSetMessage msg ts
        return SameMode

    doStartCombat :: TownState -> Position -> IO NextMode
    doStartCombat ts arenaTopleft = do
      let arenaRect = makeRect arenaTopleft combatArenaSize
      let pp = tsPartyPosition ts
      let mkCharState _charNum claimed =
            let pos = arsFindOpenSpot ts pp arenaRect claimed
                ccs = CombatCharState
                        { ccsMoments = 2 * momentsPerActionPoint,
                          ccsPose = CreaturePose
                            { cpAlpha = 255, cpAnim = WalkAnim 6 6 pp,
                              cpFaceDir = deltaFaceDir (pos `pSub` pp) },
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
          csCharStates = TM.unfold mkCharState Set.empty,
          csMonstersNotInArena = otherMonsts,
          csPeriodicTimer = 0,
          csPhase = WaitingPhase,
          csTownFiredTriggerIds =
            Set.fromList $ map triggerId $ tsTriggersFired ts,
          csTriggers = [] } -- FIXME

    tryCheating :: TownState -> String -> IO Mode
    tryCheating ts string = do
      let runScript script = do
            writeIORef stateRef ts { tsPhase = ScriptPhase script }
      case reads string of
        [(cheat, "")] -> runScript $
          case cheat of
            Gimme tag -> grantItem tag
            IAmLeTired -> return () -- TODO heal party
            Plugh xp -> grantExperience xp
            StuffMart -> startShopping $ map Right allItemTags ++
                         map Left [minBound .. maxBound]
            WhereAmI -> setMessage $ show $ tsPartyPosition ts
            Xyzzy area x y -> teleport area (Point x y)
        _ -> do
          let msg = "A hollow voice says, \"Fool!\""
          writeIORef stateRef $ arsSetMessage msg ts
      return mode

    changeParty :: TownState -> Party -> IO NextMode
    changeParty ts party' = do
      changeState ts { tsCommon = (tsCommon ts) { acsParty = party' } }

    changePhaseAndParty :: TownState -> TownPhase -> Party -> IO NextMode
    changePhaseAndParty ts phase' party' = do
      let acs' = (tsCommon ts) { acsParty = party' }
      changeState ts { tsCommon = acs', tsPhase = phase' }

    changeState :: TownState -> IO NextMode
    changeState ts' = SameMode <$ writeIORef stateRef ts'

    ignore :: IO NextMode
    ignore = return SameMode

  return mode

-------------------------------------------------------------------------------

-- | An 'Interrupt' indicates that we have executed a statement that requires
-- us to pause execution and switch to another 'Mode'.
data Interrupt = DoExit AreaTag
               | DoGameOver
               | forall a. DoMultiChoice String [(String, a)] (Maybe a)
                                         (a -> Script TownEffect ())
               | DoNarrate String (Script TownEffect ())
               | DoShopping [Either Ingredient ItemTag] (Script TownEffect ())
               | DoStartCombat Position
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
    ShoppingPhase _ _ _ -> return (tickTownAnimations ts, Nothing)
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
    [] -> return $ checkForAreaExit $
          ts { tsTriggersFired = stillFired,
               tsTriggersReady = inactive ++ newlyReady }

checkForAreaExit :: TownState -> (TownState, Maybe Interrupt)
checkForAreaExit ts =
  let isActive = any (flip rectContains $ tsPartyPosition ts) . aeRects
  in (ts, fmap (DoExit . aeDestination) $ find isActive $
          getAreaExits scenarioTriggers $ arsCurrentArea ts)

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
    EffSetPartyAnim anim -> do
      return (ts { tsPartyPose = (tsPartyPose ts) { cpAnim = anim } },
              Right $ sfn ())
    EffSetPartyFaceDir dir -> do
      return (ts { tsPartyPose = (tsPartyPose ts) { cpFaceDir = dir } },
              Right $ sfn ())
    EffSetPartyPosition dest -> do
      ts' <- updateTownVisibility ts { tsPartyPosition = dest }
      return (ts', Right $ sfn ())
    EffShop forsale -> return (ts, Left $ DoShopping forsale $ sfn ())
    EffStartCombat topleft -> return (ts, Left $ DoStartCombat topleft)
    EffTeleportToArea tag pos -> return (ts, Left $ DoTeleport tag pos)
    EffTownArea eff' ->
      case eff' of
        EffAreaCommon eff'' -> do
          (result, ts') <- executeAreaCommonEffect eff'' ts
          return (ts', Right $ sfn result)
        EffFork script ->
          return (ts, Right (mapEffect EffTownArea script `also_` sfn ()))
        EffGameOver -> return (ts, Left DoGameOver)
        EffIfCombat _ script -> return (ts, Right (script >>= sfn))
        EffMultiChoice text choices cancel ->
          return (ts, Left $ DoMultiChoice text choices cancel sfn)
        EffNarrate text -> return (ts, Left $ DoNarrate text $ sfn ())
        EffWait -> return (ts, Left $ DoWait $ sfn ())

-------------------------------------------------------------------------------

-- | The script to run each time the party takes a step in town mode.  The
-- argument indicates the position to which the party is stepping.
doTownStep :: Position -> Script TownEffect ()
doTownStep partyWalkDest = do
  forM_ [minBound .. maxBound] $ \charNum -> do
    alterAdrenaline charNum (subtract 1)
  tickSummonsByOneRound
  shouldStartCombat <- do
    monsters <- areaGet (Grid.entries . arsMonsters)
    alsoWith (flip const) (partyWalkTo partyWalkDest) $ do
      concurrentAny monsters monsterTownStep
  inflictAllPeriodicDamage
  -- TODO decay status effects by one round
  when shouldStartCombat $ do
    partyPos <- emitEffect EffGetPartyPosition
    emitEffect $ EffStartCombat $ partyPos `pSub`
      Point (half combatArenaCols) (half combatArenaRows)

-------------------------------------------------------------------------------
