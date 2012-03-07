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

{-# LANGUAGE GADTs #-}

module Fallback.Mode.Combat
  (newCombatMode)
where

import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (guard, unless, when)
import Data.Foldable (toList)
import Data.IORef
import Data.List (find, foldl', delete)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set

import Fallback.Constants
  (baseFramesPerActionPoint, baseMomentsPerFrame, combatCameraOffset,
   maxActionPoints, momentsPerActionPoint, screenRect)
import Fallback.Data.Grid
import Fallback.Data.Point
import Fallback.Data.TotalMap (TotalMap, makeTotalMap, tmAlter, tmGet)
import Fallback.Draw (paintScreen, runDraw)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog (newQuitWithoutSavingMode)
import Fallback.Mode.Narrate (newNarrateMode)
import Fallback.Scenario.Abilities (getAbility)
import Fallback.Scenario.Feats (getFeat)
import Fallback.Scenario.MonsterAI (defaultMonsterCombatAI)
import Fallback.Scenario.Potions (runPotionAction)
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers (getAreaTriggers, scenarioTriggers)
import qualified Fallback.Sound as Sound (playSound)
import Fallback.State.Action
import Fallback.State.Area
import Fallback.State.Combat
import Fallback.State.Creature (CreatureAnim(..), mtSpeed, tickCreatureAnim)
import Fallback.State.Item (WeaponData(..), getPotionAction)
import Fallback.State.Party
import Fallback.State.Resources (Resources, SoundTag(..), rsrcSound)
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags (ItemTag(PotionItemTag))
import Fallback.State.Terrain (positionTopleft)
import Fallback.State.Town (TownPhase(WalkingPhase), TownState(..))
import Fallback.Utility (maybeM, minimumKey)
import Fallback.View (fromAction, viewHandler, viewPaint)
import Fallback.View.Abilities (AbilitiesAction(..))
import Fallback.View.Combat (CombatAction(..), newCombatView)
import Fallback.View.Inventory (InventoryAction(..))
import Fallback.View.Sidebar (SidebarAction(..))

-------------------------------------------------------------------------------

newCombatMode :: Resources -> Modes -> CombatState -> IO Mode
newCombatMode resources modes initState = do
  view <- runDraw (newCombatView resources)
  stateRef <- newIORef =<< updateCombatVisibility initState
  let

    mode EvQuit = do
      cs <- readIORef stateRef
      ChangeMode <$> newQuitWithoutSavingMode resources mode view cs
    mode (EvKeyDown KeyS _ _) = do
      let msg = "Can't save game while in combat."
      SameMode <$ modifyIORef stateRef (csSetMessage msg)
    mode event = do
      mbInterrupt <- if (event /= EvTick) then return Nothing else do
        (cs', mbInt) <- readIORef stateRef >>= doTick
        mbInt <$ writeIORef stateRef cs'
      cs <- readIORef stateRef
      action <- runDraw $ viewHandler view cs screenRect event
      when (event == EvTick) $ paintScreen (viewPaint view cs)
      case mbInterrupt of
        Just (DoActivateCharacter charNum) ->
          switchToCommandPhase charNum cs
        Just (DoMonsterTurn script) -> switchToMonsterTurn cs script
        Just DoEndCombat -> doEndCombat cs
        Just DoGameOver -> ChangeMode <$> newGameOverMode' modes
        Just (DoNarrate text) -> do
          ChangeMode <$> newNarrateMode resources view cs text (return mode)
        Just DoPeriodicAction -> doPeriodicAction cs
        Just DoWait -> handleAction cs action
        Nothing -> handleAction cs action

    handleAction cs action =
      case fromAction action of
        Nothing -> ignore
        Just (CombatSidebar (MakeCharacterActive charNum)) ->
          if not (csCharCanTakeTurn cs charNum) then ignore else
            case csPhase cs of
              WaitingPhase -> switchToCommandPhase charNum cs
              CommandPhase _ -> switchToCommandPhase charNum cs
              ChooseAbilityPhase _ -> switchToCommandPhase charNum cs
              MetaAbilityPhase _ -> switchToCommandPhase charNum cs
              InventoryPhase _ Nothing -> switchToCommandPhase charNum cs
              InventoryPhase _ (Just _) -> ignore
              TargetingPhase _ -> ignore
              ExecutionPhase ce -> changeState cs { csPhase =
                ExecutionPhase ce { cePendingCharacter = Just charNum } }
        Just (CombatSidebar ToggleAbilities) ->
          case csPhase cs of
            WaitingPhase -> ignore
            CommandPhase cc ->
              changeState cs { csPhase = ChooseAbilityPhase cc }
            ChooseAbilityPhase cc ->
              changeState cs { csPhase = CommandPhase cc }
            MetaAbilityPhase cm ->
              changeState cs { csPhase = CommandPhase (cmCommander cm) }
            InventoryPhase cc Nothing ->
              changeState cs { csPhase = ChooseAbilityPhase cc }
            InventoryPhase _ (Just _) -> ignore
            TargetingPhase _ -> ignore
            ExecutionPhase _ -> ignore
        Just (CombatSidebar ToggleInventory) ->
          case csPhase cs of
            WaitingPhase -> ignore
            CommandPhase cc ->
              changeState cs { csPhase = InventoryPhase cc Nothing }
            ChooseAbilityPhase cc ->
              changeState cs { csPhase = InventoryPhase cc Nothing }
            MetaAbilityPhase cm -> changeState cs { csPhase =
              InventoryPhase (cmCommander cm) Nothing }
            InventoryPhase cc Nothing ->
              changeState cs { csPhase = CommandPhase cc }
            InventoryPhase _ (Just _) -> ignore
            TargetingPhase _ -> ignore
            ExecutionPhase _ -> ignore
        Just (CombatSidebar TryToggleCombat) ->
          case csPhase cs of
            WaitingPhase -> tryToManuallyEndCombat cs
            CommandPhase _ -> tryToManuallyEndCombat cs
            ChooseAbilityPhase _ -> tryToManuallyEndCombat cs
            MetaAbilityPhase _ -> tryToManuallyEndCombat cs
            InventoryPhase _ Nothing -> tryToManuallyEndCombat cs
            InventoryPhase _ (Just _) -> ignore
            TargetingPhase _ -> ignore
            ExecutionPhase ce -> changeState cs { csPhase =
              ExecutionPhase ce { cePendingEndCombat = True } }
        Just (CombatSidebar _) -> ignore -- FIXME other sidebar commands
        Just (CombatAbilities UpgradeSkills) -> do
          fail "FIXME CombatMode UpgradeSkills"
        Just (CombatAbilities (UseAbility abilNum)) -> do
          case csPhase cs of
            ChooseAbilityPhase cc -> useAbility cs cc abilNum NormalCost 1
            MetaAbilityPhase cm ->
              useAbility cs (cmCommander cm) abilNum (cmCostModifier cm)
                         (cmPowerModifier cm)
            _ -> ignore
        Just (CombatAbilities (UseCombatFeat tag)) -> do
          case csPhase cs of
            ChooseAbilityPhase cc -> do
              let feat = getFeat tag
              let cost = cfCastingCost feat
              let charNum = ccCharacterNumber cc
              if not (partyCanAffordCastingCost charNum cost (arsParty cs))
                then ignore else do
              case cfEffect feat of
                MetaAbility costMod powerMod -> do
                  changeState cs { csPhase = MetaAbilityPhase CombatMetability
                    { cmCommander = cc, cmCostModifier = costMod,
                      cmFeatTag = tag, cmPowerModifier = powerMod } }
                StandardFeat tkind sfn -> do
                  switchToTargetingPhase cc cs cost tkind $ sfn charNum
            _ -> ignore
        Just (CombatInventory invAct) -> do
          case csPhase cs of
            InventoryPhase cc mbItemTag -> do
              case invAct of
                ExchangeItem slot -> do
                  case partyTryExchangeItem slot mbItemTag (arsParty cs) of
                    Nothing -> ignore
                    Just (mbItemTag', party') -> do
                      changePhaseAndParty cs (InventoryPhase cc mbItemTag')
                                          party'
                UpgradeStats -> do
                  fail "FIXME CombatMode UpgradeStats"
                UseItem slot -> do
                  if isJust mbItemTag then ignore else do
                  let party = arsParty cs
                  case partyItemInSlot slot party of
                    Just (PotionItemTag potionTag) -> do
                      let apNeeded = 2
                      if not (hasEnoughActionPoints cs cc apNeeded) then
                        ignore else do
                      let cs' = cs { csCommon = (csCommon cs) { acsParty =
                                  partyRemoveItem slot party } }
                      executeCommand cs' cc NoCost apNeeded $
                        mapEffect EffCombatArea $
                        runPotionAction (getPotionAction potionTag) $
                        ccCharacterNumber cc
                    _ -> ignore
            _ -> ignore
        Just (CombatMove dir) ->
          case csPhase cs of
            CommandPhase cc -> do
              let charNum = ccCharacterNumber cc
              let character = arsGetCharacter charNum cs
              let ccs = tmGet charNum $ csCharStates cs
              -- Check that we have enough AP to move:
              let apNeeded = if seIsEntangled (chrStatus character)
                             then 2 else 1
              if not (hasEnoughActionPoints cs cc apNeeded) then ignore else do
              -- Check that the terrain isn't blocking us:
              let pos' = ccsPosition ccs `plusDir` dir
              if arsIsBlockedForPartyModuloMonsters cs pos' then ignore else do
              -- Check if the square is occupied:
              case arsOccupant pos' cs of
                Nothing -> -- Walk
                  executeCommand cs cc NoCost apNeeded $
                  charWalkTo charNum pos' >>= wait
                Just (Left _charNum') -> -- Switch places if possible
                  ignore -- FIXME switch places
                Just (Right _) -> ignore
              -- TODO If moving away from hostile monster (and not invisible)
              --      monster gets free hit.
            _ -> ignore
        Just (CombatAttack target) ->
          case csPhase cs of
            CommandPhase cc -> do
              let charNum = ccCharacterNumber cc
              let ccs = tmGet charNum $ csCharStates cs
              -- Make sure we have enough action points:
              let apNeeded = maxActionPoints
              if not (hasEnoughActionPoints cs cc apNeeded) then ignore else do
              let char = arsGetCharacter charNum cs
              let wd = chrEquippedWeaponData char
              let origin = ccsPosition ccs
              -- Make sure the target is within range and line-of-sight:
              if not (pSqDist origin target <= rangeSqDist (wdRange wd) &&
                      arsIsVisibleToCharacter charNum cs target) then
                ignore else do
              -- Execute the attack:
              executeCommand cs cc NoCost apNeeded $ do
                characterWeaponAttack (ccCharacterNumber cc) target
            _ -> ignore
        Just CombatEndTurnEarly ->
          case csPhase cs of
            CommandPhase cc ->
              -- If another character wants a turn, they're up next, otherwise
              -- return to waiting phase.
              case ccssCharThatWantsATurn (csCharStates cs) (arsParty cs) of
                Nothing -> changeState $
                  (subtractUsedActionPoints cc cs) { csPhase = WaitingPhase }
                Just charNum -> switchToCommandPhase charNum cs
            _ -> ignore
        Just (CombatTargetPosition pos) ->
          (case csPhase cs of
             TargetingPhase CombatTargeting { ctCastingCost = cost,
                                              ctCommander = cc,
                                              ctTargeting = targeting,
                                              ctScriptFn = sfn } ->
               case targeting of
                 TargetingAlly rng ->
                   if cannotHit rng then ignore else execute (sfn $ Left pos)
                 TargetingArea rng areaFn ->
                   if cannotHit rng then ignore else
                     execute $ sfn (pos, areaFn cs originPos pos)
                 TargetingMulti rng n ps ->
                   if pos `elem` ps then switch (delete pos ps) else
                     if cannotHit rng then ignore else
                       let ps' = pos : ps
                       in if length ps' < n then switch ps'
                          else execute (sfn ps')
                   where
                     switch ps' = SameMode <$ writeIORef stateRef cs
                                  { csPhase = TargetingPhase CombatTargeting
                                    { ctCastingCost = cost, ctCommander = cc,
                                      ctTargeting = TargetingMulti rng n ps',
                                      ctScriptFn = sfn } }
                 TargetingSingle rng ->
                   if cannotHit rng then ignore else execute (sfn pos)
               where
                 cannotHit rng =
                   not (arsIsVisibleToCharacter charNum cs pos) ||
                   pSqDist pos originPos > rng
                 charNum = ccCharacterNumber cc
                 execute = executeCommand cs cc cost maxActionPoints
                 originPos = arsCharacterPosition charNum cs
             _ -> ignore) :: IO NextMode
        Just (CombatTargetCharacter charNum) ->
          (case csPhase cs of
             TargetingPhase (CombatTargeting { ctCastingCost = cost,
                                               ctCommander = cc,
                                               ctTargeting = TargetingAlly _,
                                               ctScriptFn = sfn }) ->
               -- FIXME check in-range and visible to character
               executeCommand cs cc cost maxActionPoints
                              (sfn $ Right charNum)
             _ -> ignore) :: IO NextMode
        Just CombatEndTargeting ->
          (case csPhase cs of
             TargetingPhase (CombatTargeting {
                               ctCastingCost = cost,
                               ctCommander = cc,
                               ctTargeting = TargetingMulti _ _ ps,
                               ctScriptFn = sfn }) ->
               if null ps then ignore else
                 executeCommand cs cc cost maxActionPoints (sfn ps)
             _ -> ignore) :: IO NextMode

    switchToCommandPhase :: CharacterNumber -> CombatState -> IO NextMode
    switchToCommandPhase charNum cs = do
      -- TODO play sound, fire off doodad
      let cs' = maybe id subtractUsedActionPoints (csCommander cs) cs
      let noWant ccs = ccs { ccsWantsTurn = False }
      changeState cs' { csCharStates =
                          tmAlter charNum noWant (csCharStates cs'),
                        csPhase = CommandPhase CombatCommander
                          { ccActionPointsUsed = 0,
                            ccCharacterNumber = charNum } }

    switchToTargetingPhase :: CombatCommander -> CombatState -> CastingCost
                           -> TargetKind a -> (a -> Script CombatEffect ())
                           -> IO NextMode
    switchToTargetingPhase cc cs cost tk sfn = do
      case tk of
        AllyTarget r -> setTargeting (TargetingAlly r)
        AreaTarget r f -> setTargeting (TargetingArea r f)
        AutoTarget -> executeCommand cs cc cost maxActionPoints $ sfn ()
        MultiTarget r n -> setTargeting (TargetingMulti r n [])
        SingleTarget r -> setTargeting (TargetingSingle r)
      where setTargeting targeting =
              changeState cs { csPhase = TargetingPhase CombatTargeting
                                 { ctCastingCost = cost,
                                   ctCommander = cc,
                                   ctScriptFn = sfn,
                                   ctTargeting = targeting } }

    useAbility :: CombatState -> CombatCommander -> AbilityNumber
               -> CostModifier -> PowerModifier -> IO NextMode
    useAbility cs cc abilNum costMod power = do
      let charNum = ccCharacterNumber cc
      let char = arsGetCharacter charNum cs
      fromMaybe ignore $ do
        level <- tmGet abilNum (chrAbilities char)
        case getAbility (chrClass char) abilNum level of
          ActiveAbility originalCost effect -> do
            let cost = modifyCost costMod originalCost
            guard $ partyCanAffordCastingCost charNum cost $ arsParty cs
            case effect of
              MetaAttack matype tkindFn sfn -> do
                let wrange = wdRange $ chrEquippedWeaponData char
                guard $ metaAttackMatches matype wrange
                let tkind = tkindFn $ rangeSqDist wrange
                Just $ switchToTargetingPhase cc cs cost tkind $
                  sfn charNum power
              GeneralAbility tkind sfn ->
                Just $ switchToTargetingPhase cc cs cost tkind $
                  mapEffect EffCombatArea . sfn charNum power
              CombatAbility tkind sfn ->
                Just $ switchToTargetingPhase cc cs cost tkind $
                  sfn charNum power
          PassiveAbility -> Nothing

    executeCommand :: CombatState -> CombatCommander -> CastingCost -> Int
                   -> Script CombatEffect () -> IO NextMode
    executeCommand cs cc cost apSpent script = do
      let phase = ExecutionPhase CombatExecution
            { ceCommander = Just cc { ccActionPointsUsed =
                                        ccActionPointsUsed cc + apSpent },
              cePendingCharacter = Nothing,
              cePendingEndCombat = False,
              ceScript = script }
      changePhaseAndParty cs phase $
        partyDeductCastingCost (ccCharacterNumber cc) cost (arsParty cs)

    doPeriodicAction :: CombatState -> IO NextMode
    doPeriodicAction cs = do
      let ce = CombatExecution
                 { ceCommander = Nothing,
                   cePendingCharacter = Nothing,
                   cePendingEndCombat = False,
                   ceScript = inflictAllPeriodicDamage }
      changeState cs { csPhase = ExecutionPhase ce }

    switchToMonsterTurn :: CombatState -> Script CombatEffect () -> IO NextMode
    switchToMonsterTurn cs script = do
      let ce = CombatExecution
                 { ceCommander = Nothing,
                   cePendingCharacter = Nothing,
                   cePendingEndCombat = False,
                   ceScript = script }
      changeState cs { csPhase = ExecutionPhase ce }

    tryToManuallyEndCombat :: CombatState -> IO NextMode
    tryToManuallyEndCombat cs = do
      if not (arsAreMonstersNearby cs) then doEndCombat cs else do
        let msg = "Can't end combat -- there are still enemies nearby."
        writeIORef stateRef $ csSetMessage msg cs
        return SameMode

    doEndCombat :: CombatState -> IO NextMode
    doEndCombat cs = do
      let townifyCharacter char =
            char { chrHealth = max 1 (chrHealth char),
                   chrStatus = townifyStatus (chrStatus char) }
      let activeChar = maybe minBound ccCharacterNumber (csCommander cs)
      let partyPos =
            let positions = map ccsPosition $ toList $ csCharStates cs
                center :: DPoint
                center = foldl' pAdd pZero $ map (fmap fromIntegral) positions
            in minimumKey (pSqDist center . fmap fromIntegral) positions
      let acs = csCommon cs
      let townTriggers =
            getAreaTriggers scenarioTriggers $ partyCurrentArea $ acsParty acs
      let wasFired = flip Set.member (csTownFiredTriggerIds cs) . triggerId
      let (monsters, extraMonsters) = gridMerge (csMonstersNotInArena cs)
                                                (gridEntries $ acsMonsters acs)
      let party' = (acsParty acs) { partyCharacters =
            fmap townifyCharacter $ partyCharacters $ acsParty acs }
      let acs' = acs { acsMonsters = monsters, acsParty = party' }
      unless (null extraMonsters) $ do
        putStrLn ("Warning: there are " ++ show (length extraMonsters) ++
                  " extra monsters.")
      Sound.playSound (rsrcSound resources SndCombatEnd)
      ChangeMode <$> newTownMode' modes TownState
        { tsActiveCharacter = activeChar,
          tsCommon = acs',
          tsPartyAnim =
            WalkAnim 4 4 (ccsPosition $ tmGet activeChar $ csCharStates cs),
          tsPartyFaceDir = FaceLeft, -- FIXME
          tsPartyPosition = partyPos,
          tsPhase = WalkingPhase,
          tsTriggersFired = filter wasFired townTriggers,
          tsTriggersReady = filter (not . wasFired) townTriggers }

    changePhaseAndParty :: CombatState -> CombatPhase -> Party -> IO NextMode
    changePhaseAndParty cs phase' party' = do
      let acs' = (csCommon cs) { acsParty = party' }
      changeState cs { csCommon = acs', csPhase = phase' }

    changeState :: CombatState -> IO NextMode
    changeState cs' = SameMode <$ writeIORef stateRef cs'

    ignore :: IO NextMode
    ignore = return SameMode

  focusBlurMode (readIORef stateRef) view mode

-------------------------------------------------------------------------------

data Interrupt = DoActivateCharacter CharacterNumber
               | DoMonsterTurn (Script CombatEffect ())
               | DoEndCombat
               | DoGameOver
               | DoPeriodicAction
               | DoNarrate String
               | DoWait

doTick :: CombatState -> IO (CombatState, Maybe Interrupt)
doTick cs =
  case csPhase cs of
    WaitingPhase -> tickWaiting cs'
    CommandPhase _ -> animationsOnly
    ChooseAbilityPhase _ -> animationsOnly
    MetaAbilityPhase _ -> animationsOnly
    InventoryPhase _ _ -> animationsOnly
    TargetingPhase _ -> animationsOnly
    ExecutionPhase ce -> doTickExecution cs' ce
  where
    acs = csCommon cs
    animationsOnly = return (cs', Nothing)
    cs' = cs { csCharStates = fmap tickCharStateBasic (csCharStates cs),
               csCommon = tickAnimations camTarget acs }
    tickCharStateBasic ccs = ccs { ccsAnim = tickCreatureAnim (ccsAnim ccs) }
    camTarget = fromIntegral <$> (positionTopleft (csArenaTopleft cs) `pSub`
                                  combatCameraOffset)

{-

Waiting:
  - update clock/doodads/anims
  - increase time bars
  - decay status
  - if monster wants a turn, switch to execution
  - if no more monsters, switch to town mode

Command:
  - update clock/doodads/anims

Execution:
  - update clock/doodads/anims
  - execute stmts
  - if no more stmts, switch to waiting (or command) (or town mode)

-}

tickWaiting :: CombatState -> IO (CombatState, Maybe Interrupt)
tickWaiting cs = do
  let acs = csCommon cs
  if gridNull (acsMonsters acs) then return (cs, Just DoEndCombat) else do
  -- Update monster status effects and time bars, and see if any monsters are
  -- ready for a turn.
  let (monsters', mbScript) =
        gridUpdateSelect tickMonsterWaiting (acsMonsters acs)
  -- Update party status effects.
  let party' = tickPartyWaiting (acsParty acs)
  -- Update party time bars.
  let ccss' = tickCharStatesWaiting (csCharStates cs)
  -- See if any party characters are ready for a turn.
  let mbCharNum = ccssCharThatWantsATurn ccss' party'
  -- Update the periodic timer.
  let timer' = (csPeriodicTimer cs + 1) `mod` baseFramesPerActionPoint
  -- Decay fields.
  fields' <- decayFields 1 (acsFields acs)
  -- Ready characters take precedence over ready monsters.
  let mbInterrupt = if timer' == 0 then Just DoPeriodicAction
                    else (DoActivateCharacter <$> mbCharNum) <|>
                         (DoMonsterTurn <$> mbScript)
  return (cs { csCharStates = ccss',
               csCommon = acs { acsFields = fields', acsMonsters = monsters',
                                acsParty = party' },
               csPeriodicTimer = timer' }, mbInterrupt)
  where
    tickPartyWaiting party =
      party { partyCharacters = tickCharWaiting <$> partyCharacters party }
    tickCharWaiting char =
      char { chrStatus = decayStatusEffects (chrStatus char) }
    tickCharStatesWaiting ccss =
      makeTotalMap $ \charNum ->
        tickCharStateWaiting (arsGetCharacter charNum cs) (tmGet charNum ccss)

doTickExecution :: CombatState -> CombatExecution
                -> IO (CombatState, Maybe Interrupt)
doTickExecution cs ce = do
  (cs', mbScriptInterrupt) <- executeScript cs (ceScript ce)
  case mbScriptInterrupt of
    Nothing ->
      if (gridNull (arsMonsters cs') ||
          cePendingEndCombat ce && not (arsAreMonstersNearby cs'))
      then return (cs', Just DoEndCombat) else
        let endTurn cs'' = do
              let mbInterrupt = do
                    charNum <- cePendingCharacter ce
                    guard (csCharCanTakeTurn cs charNum)
                    Just (DoActivateCharacter charNum)
              return (cs'' { csPhase = WaitingPhase }, mbInterrupt)
            hasAP cc = hasEnoughActionPoints cs cc 1
        in case ceCommander ce of
             Just cc | hasAP cc ->
               return (cs' { csPhase = CommandPhase cc }, Nothing)
             Just cc | otherwise ->
               endTurn (subtractUsedActionPoints cc cs')
             Nothing -> endTurn cs'
    Just (script, interrupt) ->
      return (cs' { csPhase = ExecutionPhase ce { ceScript = script } },
              Just interrupt)

subtractUsedActionPoints :: CombatCommander -> CombatState -> CombatState
subtractUsedActionPoints cc cs =
  let fn ccs = ccs { ccsMoments = ccsMoments ccs -
                       min maxActionPoints (ccActionPointsUsed cc) *
                       momentsPerActionPoint }
  in csAlterCharState (ccCharacterNumber cc) fn cs

executeScript :: CombatState -> Script CombatEffect ()
             -> IO (CombatState, Maybe (Script CombatEffect (), Interrupt))
executeScript cs script =
  case execScript script of
    ResultFinal () -> return (cs, Nothing)
    ResultEffect eff sfn -> do
      (cs', script', mbInterrupt) <- executeEffect cs eff sfn
      case mbInterrupt of
        Nothing -> executeScript cs' script'
        Just interrupt -> return (cs', Just (script', interrupt))

-- | Execute a single effect.  Return the updated 'CombatState', the remainder
-- of the script to run, and the interrupt (if any).
executeEffect :: CombatState -> CombatEffect a -> (a -> Script CombatEffect ())
              -> IO (CombatState, Script CombatEffect (), Maybe Interrupt)
executeEffect cs eff sfn =
  case eff of
    EffCombatArea eff' ->
      case eff' of
        EffAreaCommon eff'' -> do
          (result, cs') <- executeAreaCommonEffect eff'' cs
          return (cs', sfn result, Nothing)
        EffGameOver -> return (cs, sfn (), Just DoGameOver)
        EffIfCombat script _ -> return (cs, script >>= sfn, Nothing)
        EffMultiChoice _ _ _ ->
          fail "FIXME Combat executeEffect EffMultiChoice"
        EffNarrate text -> return (cs, sfn (), Just (DoNarrate text))
        EffWait -> return (cs, sfn (), Just DoWait)
    EffEndCombat -> return (cs, sfn (), Just DoEndCombat)
    EffGetCharFaceDir charNum -> do
      return (cs, sfn $ ccsFaceDir $ csGetCharState cs charNum, Nothing)
    EffGetCharMoments charNum -> do
      return (cs, sfn $ ccsMoments $ csGetCharState cs charNum, Nothing)
    EffSetCharAnim charNum anim -> do
      let setAnim ccs = ccs { ccsAnim = anim }
      return (csAlterCharState charNum setAnim cs, sfn (), Nothing)
    EffSetCharFaceDir charNum faceDir -> do
      let setFace ccs = ccs { ccsFaceDir = faceDir }
      return (csAlterCharState charNum setFace cs, sfn (), Nothing)
    EffSetCharMoments charNum moments -> do
      let setMoments ccs = ccs { ccsMoments = moments }
      return (csAlterCharState charNum setMoments cs, sfn (), Nothing)
    EffSetCharPosition charNum pos -> do
      let setPos ccs = ccs { ccsPosition = pos }
      cs' <- updateCombatVisibility cs
               { csCharStates = tmAlter charNum setPos (csCharStates cs) }
      return (cs', sfn (), Nothing)

-------------------------------------------------------------------------------

ccssCharThatWantsATurn :: TotalMap CharacterNumber CombatCharState
                       -> Party -> Maybe CharacterNumber
ccssCharThatWantsATurn ccss party =
  let fn charNum = ccsWantsTurn (tmGet charNum ccss) &&
                   chrCanTakeTurn (partyGetCharacter party charNum)
  in find fn [minBound .. maxBound]

tickMonsterWaiting :: GridEntry Monster
                   -> (Monster, Maybe (Script CombatEffect ()))
tickMonsterWaiting entry = (monst', mbScript) where
  monst = geValue entry
  monst' = monst { monstMoments = moments', monstStatus = status' }
  mbScript = if moments' >= maxActionPoints * momentsPerActionPoint
             then Just (defaultMonsterCombatAI entry >> resetMoments)
             else Nothing
  resetMoments = do
    monsters <- areaGet arsMonsters
    maybeM (gridLookup (geKey entry) monsters) $ \entry' -> do
      emitAreaEffect $ EffReplaceMonster (geKey entry') $
        Just (geValue entry') { monstMoments = 0 }
  moments' = max moments $ min (maxActionPoints * momentsPerActionPoint) $
             round (mtSpeed mtype * seSpeedMultiplier status *
                    fromIntegral baseMomentsPerFrame) + moments
  moments = monstMoments monst
  status' = decayStatusEffects status
  mtype = monstType monst
  status = monstStatus monst

-------------------------------------------------------------------------------
