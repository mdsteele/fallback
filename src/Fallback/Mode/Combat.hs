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
  (baseMomentsPerFrame, combatCameraOffset, framesPerRound, maxActionPoints,
   maxMoments, momentsPerActionPoint, roundsPerFrame)
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM
import Fallback.Draw (handleScreen, paintScreen)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog (newQuitWithoutSavingMode)
import Fallback.Mode.Narrate (newNarrateMode)
import Fallback.Scenario.Abilities (getAbility)
import Fallback.Scenario.Feats (featCastingCost, featEffect)
import Fallback.Scenario.MonsterAI (defaultMonsterCombatAI)
import Fallback.Scenario.Potions (runPotionAction)
import Fallback.Scenario.Script
import qualified Fallback.Sound as Sound (playSound)
import Fallback.State.Action
import Fallback.State.Area
import Fallback.State.Combat
import Fallback.State.Creature
import Fallback.State.Item (WeaponData(..), getPotionAction)
import Fallback.State.Party
import Fallback.State.Resources (Resources, SoundTag(..), rsrcSound)
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags (ItemTag(PotionItemTag))
import Fallback.State.Terrain (positionTopleft)
import Fallback.State.Town (TownPhase(WalkingPhase), TownState(..))
import Fallback.State.Trigger (fireTrigger)
import Fallback.Utility (maybeM, minimumKey)
import Fallback.View (fromAction, viewHandler, viewPaint)
import Fallback.View.Abilities (AbilitiesAction(..))
import Fallback.View.Combat (CombatAction(..), newCombatView)
import Fallback.View.Inventory (InventoryAction(..))
import Fallback.View.Sidebar (SidebarAction(..))

-------------------------------------------------------------------------------

newCombatMode :: Resources -> Modes -> CombatState -> IO Mode
newCombatMode resources modes initState = do
  view <- newCombatView resources
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
      action <- handleScreen $ viewHandler view cs event
      when (event == EvTick) $ paintScreen (viewPaint view cs)
      case mbInterrupt of
        Just (DoActivateCharacter charNum) ->
          switchToCommandPhase charNum cs
        Just (DoRunScript script) -> switchToNonCharacterTurn cs script
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
        Just (CombatAbilities (UseAbility abilNum)) -> do
          case csPhase cs of
            ChooseAbilityPhase cc ->
              useAbility cs cc abilNum FullAP NormalCost 1
            MetaAbilityPhase cm ->
              useAbility cs (cmCommander cm) abilNum (cmAPModifier cm)
                         (cmCostModifier cm) (cmPowerModifier cm)
            _ -> ignore
        Just (CombatAbilities (UseCombatFeat tag)) -> do
          case csPhase cs of
            ChooseAbilityPhase cc -> do
              let cost = featCastingCost tag
              let charNum = ccCharacterNumber cc
              if not (partyCanAffordCastingCost charNum cost (arsParty cs))
                then ignore else do
              case featEffect tag of
                MetaAbility apMod costMod powerMod -> do
                  -- TODO arrange to charge adrenaline after using ability
                  changeState cs { csPhase = MetaAbilityPhase CombatMetability
                    { cmAPModifier = apMod, cmCommander = cc,
                      cmCostModifier = costMod, cmFeatTag = tag,
                      cmPowerModifier = powerMod } }
                StandardFeat tkindFn sfn -> do
                  let tk = tkindFn $ rangeRadius $ wdRange $
                           chrEquippedWeaponData $ arsGetCharacter charNum cs
                  switchToTargetingPhase cc cs maxActionPoints cost tk
                                         (sfn charNum)
            _ -> ignore
        Just (CombatAbilities UseNormalAttack) -> do
          case csPhase cs of
            ChooseAbilityPhase cc -> do
              let charNum = ccCharacterNumber cc
              let wd = chrEquippedWeaponData $ arsGetCharacter charNum cs
              let tk = SingleTarget $ rangeRadius $ wdRange wd
              switchToTargetingPhase cc cs maxActionPoints NoCost tk $
                \target -> characterWeaponAttack charNum target $
                           baseAttackModifiers { amCanMiss = True }
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
                DoneInventory -> do
                  if isJust mbItemTag then ignore else do
                  changeState cs { csPhase = CommandPhase cc }
            _ -> ignore
        Just (CombatMove dir) ->
          case csPhase cs of
            CommandPhase cc -> do
              let charNum = ccCharacterNumber cc
              let character = arsGetCharacter charNum cs
              let ccs = TM.get charNum $ csCharStates cs
              -- Check that we have enough AP to move:
              let apNeeded = if seIsEntangled (chrStatus character)
                             then 2 else 1
              if not (hasEnoughActionPoints cs cc apNeeded) then ignore else do
              -- Check that the terrain isn't blocking us:
              let pos' = ccsPosition ccs `plusDir` dir
              if cannotWalkOn (arsTerrainOpenness pos' cs) then ignore else do
              -- Check if the square is occupied:
              case arsOccupant pos' cs of
                Nothing -> -- Walk
                  executeCommand cs cc NoCost apNeeded $ do
                    characterCombatWalk charNum pos'
                Just (Left _charNum') -> -- Switch places if possible
                  ignore -- FIXME switch places
                Just (Right _) -> ignore
            _ -> ignore
        Just (CombatAttack target) ->
          case csPhase cs of
            CommandPhase cc -> do
              let charNum = ccCharacterNumber cc
              let ccs = TM.get charNum $ csCharStates cs
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
                characterWeaponAttack (ccCharacterNumber cc) target $
                  baseAttackModifiers { amCanMiss = True }
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
             TargetingPhase CombatTargeting { ctActionPointsNeeded = apNeeded,
                                              ctCastingCost = cost,
                                              ctCommander = cc,
                                              ctTargeting = targeting,
                                              ctScriptFn = sfn } ->
               case targeting of
                 TargetingAlly rng ->
                   if cannotHit rng then ignore else execute (sfn $ Left pos)
                 TargetingArea areaFn rng -> do
                   if cannotHit rng then ignore else do
                   let targets = areaFn cs charNum pos
                   if null targets then ignore else do
                   execute $ sfn (pos, targets)
                 TargetingJump areaFn targetable -> do
                   if Set.notMember pos targetable then ignore else do
                   execute $ sfn (pos, areaFn cs charNum pos)
                 TargetingMulti n rng ps ->
                   if pos `elem` ps then switch (delete pos ps) else
                     if cannotHit rng then ignore else
                       let ps' = pos : ps
                       in if length ps' < n then switch ps'
                          else execute (sfn ps')
                   where
                     switch ps' = SameMode <$ writeIORef stateRef cs
                                  { csPhase = TargetingPhase CombatTargeting
                                    { ctActionPointsNeeded = apNeeded,
                                      ctCastingCost = cost, ctCommander = cc,
                                      ctTargeting = TargetingMulti n rng ps',
                                      ctScriptFn = sfn } }
                 TargetingSingle rng ->
                   if cannotHit rng then ignore else execute (sfn pos)
               where
                 cannotHit rng =
                   not (arsIsVisibleToCharacter charNum cs pos) ||
                   pSqDist pos (arsCharacterPosition charNum cs) > ofRadius rng
                 charNum = ccCharacterNumber cc
                 execute = executeCommand cs cc cost apNeeded
             _ -> ignore) :: IO NextMode
        Just (CombatTargetCharacter charNum) ->
          (case csPhase cs of
             TargetingPhase (CombatTargeting { ctActionPointsNeeded = apNeeded,
                                               ctCastingCost = cost,
                                               ctCommander = cc,
                                               ctTargeting = TargetingAlly _,
                                               ctScriptFn = sfn }) ->
               -- FIXME check in-range and visible to character
               executeCommand cs cc cost apNeeded $ sfn $ Right charNum
             _ -> ignore) :: IO NextMode
        Just CombatEndTargeting ->
          (case csPhase cs of
             TargetingPhase (CombatTargeting {
                               ctActionPointsNeeded = apNeeded,
                               ctCastingCost = cost,
                               ctCommander = cc,
                               ctTargeting = TargetingMulti _ _ ps,
                               ctScriptFn = sfn }) ->
               if null ps then ignore else
                 executeCommand cs cc cost apNeeded (sfn ps)
             _ -> ignore) :: IO NextMode
        Just CombatCancelAction ->
          case csPhase cs of
            WaitingPhase -> ignore
            CommandPhase _ -> ignore
            ChooseAbilityPhase cc -> do
              changeState cs { csPhase = CommandPhase cc }
            MetaAbilityPhase cm -> do
              changeState cs { csPhase = ChooseAbilityPhase (cmCommander cm) }
            InventoryPhase cc Nothing -> do
              changeState cs { csPhase = CommandPhase cc }
            InventoryPhase _ (Just _) -> ignore
            TargetingPhase ct -> do
              changeState cs { csPhase = CommandPhase (ctCommander ct) }
            ExecutionPhase _ -> ignore

    switchToCommandPhase :: CharacterNumber -> CombatState -> IO NextMode
    switchToCommandPhase charNum cs = do
      -- TODO play sound, fire off doodad
      let cs' = maybe id subtractUsedActionPoints (csCommander cs) cs
      let noWant ccs = ccs { ccsWantsTurn = False }
      changeState cs' { csCharStates =
                          TM.adjust charNum noWant (csCharStates cs'),
                        csPhase = CommandPhase CombatCommander
                          { ccActionPointsUsed = 0,
                            ccCharacterNumber = charNum } }

    switchToTargetingPhase :: CombatCommander -> CombatState -> ActionPoints
                           -> CastingCost -> TargetKind a
                           -> (a -> Script CombatEffect ()) -> IO NextMode
    switchToTargetingPhase cc cs actionPoints cost tk sfn = do
      case tk of
        AllyTarget r -> setTargeting (TargetingAlly r)
        AreaTarget f r -> setTargeting (TargetingArea f r)
        AutoTarget -> executeCommand cs cc cost actionPoints $ sfn ()
        JumpTarget areaFn radius ->
          setTargeting $ TargetingJump areaFn $
          arsCharacterJumpDestinations radius (ccCharacterNumber cc) cs
        MultiTarget n r -> setTargeting (TargetingMulti n r [])
        SingleTarget r -> setTargeting (TargetingSingle r)
      where setTargeting targeting =
              changeState cs { csPhase = TargetingPhase CombatTargeting
                                 { ctActionPointsNeeded = actionPoints,
                                   ctCastingCost = cost,
                                   ctCommander = cc,
                                   ctScriptFn = sfn,
                                   ctTargeting = targeting } }

    useAbility :: CombatState -> CombatCommander -> AbilityNumber -> APModifier
               -> CostModifier -> PowerModifier -> IO NextMode
    useAbility cs cc abilNum apMod costMod metaPower = do
      let charNum = ccCharacterNumber cc
      let char = arsGetCharacter charNum cs
      let power = metaPower * chrPowerModifier char
      fromMaybe ignore $ do
        abilRank <- TM.get abilNum (chrAbilities char)
        case getAbility (chrClass char) abilNum abilRank of
          ActiveAbility originalApNeeded originalCost effect -> do
            let apNeeded = modifyActionPoints apMod originalApNeeded
            guard $ hasEnoughActionPoints cs cc apNeeded
            let cost = modifyCost costMod originalCost
            guard $ partyCanAffordCastingCost charNum cost $ arsParty cs
            case effect of
              MetaAttack matype tkindFn sfn -> do
                let wrange = wdRange $ chrEquippedWeaponData char
                guard $ metaAttackMatches matype wrange
                let tkind = tkindFn $ rangeRadius wrange
                Just $ switchToTargetingPhase cc cs apNeeded cost tkind $
                  sfn charNum power
              GeneralAbility tkind sfn ->
                Just $ switchToTargetingPhase cc cs apNeeded cost tkind $
                  mapEffect EffCombatArea . sfn charNum power
              CombatAbility tkind sfn ->
                Just $ switchToTargetingPhase cc cs apNeeded cost tkind $
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

    switchToNonCharacterTurn :: CombatState -> Script CombatEffect ()
                             -> IO NextMode
    switchToNonCharacterTurn cs script = do
      let ce = CombatExecution
                 { ceCommander = Nothing,
                   cePendingCharacter = Nothing,
                   cePendingEndCombat = False,
                   ceScript = script }
      changeState cs { csPhase = ExecutionPhase ce }

    tryToManuallyEndCombat :: CombatState -> IO NextMode
    tryToManuallyEndCombat cs = do
      let doNotEndCombat reason = do
            let msg = "Can't end combat -- " ++ reason
            writeIORef stateRef $ csSetMessage msg cs
            return SameMode
      if not $ csCanRunAway cs then
        doNotEndCombat "you must win this battle." else do
      if arsAreEnemiesNearby cs then
        doNotEndCombat "there are still enemies nearby." else do
      doEndCombat cs

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
            in minimumKey (pDist center . fmap fromIntegral) positions
      let acs = csCommon cs
      let (monsters, extraMonsters) =
            Grid.merge (csMonstersNotInArena cs)
                       (Grid.entries $ acsMonsters acs)
      let party' = (acsParty acs) { partyCharacters =
            fmap townifyCharacter $ partyCharacters $ acsParty acs }
      let acs' = acs { acsMonsters = monsters, acsParty = party' }
      unless (null extraMonsters) $ do
        putStrLn ("Warning: there are " ++ show (length extraMonsters) ++
                  " extra monsters.")
      Sound.playSound (rsrcSound resources SndCombatEnd)
      let activeCcs = TM.get activeChar $ csCharStates cs
      ChangeMode <$> newTownMode' modes TownState
        { tsActiveCharacter = activeChar,
          tsCommon = acs',
          tsPartyPose = CreaturePose
            { cpAlpha = cpAlpha (ccsPose activeCcs),
              cpAnim = WalkAnim 4 4 (ccsPosition activeCcs),
              cpFaceDir =
                deltaFaceDir (partyPos `pSub` ccsPosition activeCcs) },
          tsPartyPosition = partyPos,
          tsPhase = WalkingPhase,
          tsTriggers = csTownTriggers cs }

    changePhaseAndParty :: CombatState -> CombatPhase -> Party -> IO NextMode
    changePhaseAndParty cs phase' party' = do
      let acs' = (csCommon cs) { acsParty = party' }
      changeState cs { csCommon = acs', csPhase = phase' }

    changeState :: CombatState -> IO NextMode
    changeState cs' = SameMode <$ writeIORef stateRef cs'

    ignore :: IO NextMode
    ignore = return SameMode

  return mode

-------------------------------------------------------------------------------

data Interrupt = DoActivateCharacter CharacterNumber
               | DoRunScript (Script CombatEffect ())
               | DoEndCombat
               | DoGameOver
               | DoPeriodicAction
               | DoNarrate String
               | DoWait

-- | Perform per-frame actions for the current phase, possibly creating an
-- interrupt (which may put us into another phase).
doTick :: CombatState -> IO (CombatState, Maybe Interrupt)
doTick cs =
  case csPhase cs of
    WaitingPhase -> doTickWaiting cs'
    CommandPhase _ -> animationsOnly
    ChooseAbilityPhase _ -> animationsOnly
    MetaAbilityPhase _ -> animationsOnly
    InventoryPhase _ _ -> animationsOnly
    TargetingPhase _ -> animationsOnly
    ExecutionPhase ce -> doTickExecution cs' ce
  where
    acs = csCommon cs
    animationsOnly = return (cs', Nothing)
    cs' = cs { csCharStates = TM.mapWithKey tickCcs (csCharStates cs),
               csCommon = tickAnimations camTarget
                                         (arsAllyOccupiedPositions cs) acs }
    tickCcs charNum ccs = ccs
      { ccsPose = tickCreaturePose (seInvisibility $ chrStatus $
                                    arsGetCharacter charNum cs)
                                   True (ccsPose ccs) }
    camTarget = fromIntegral <$> (positionTopleft (csArenaTopleft cs) `pSub`
                                  combatCameraOffset)

-- | Perform per-frame actions for the waiting phase, possibly creating an
-- interrupt (which may put us into another phase).
doTickWaiting :: CombatState -> IO (CombatState, Maybe Interrupt)
doTickWaiting cs = do
  if noMoreEnemyMonsters cs then return (cs, Just DoEndCombat) else do
  -- See if any combat triggers are ready to fire.
  let (triggers', mbScript) = fireTrigger cs (csTriggers cs)
  let cs' = cs { csTriggers = triggers' }
  case mbScript of
    Just script -> return (cs', Just $ DoRunScript script)
    Nothing -> doTickWaitingNoTriggers cs'

-- | Perform per-frame actions for the waiting phase, given that we have
-- already determined that there are no triggers to run.  This is called only
-- from 'doTickWaiting'.
doTickWaitingNoTriggers :: CombatState -> IO (CombatState, Maybe Interrupt)
doTickWaitingNoTriggers cs = do
  let acs = csCommon cs
  -- Update monster status effects and time bars, and see if any monsters are
  -- ready for a turn.
  let (monsters', mbScript) =
        Grid.updateSelect tickMonsterWaiting (acsMonsters acs)
  -- Update party status effects.
  let party' = tickPartyWaiting (acsParty acs)
  -- Update party time bars.
  let ccss' = tickCharStatesWaiting (csCharStates cs)
  -- See if any party characters are ready for a turn.
  let mbCharNum = ccssCharThatWantsATurn ccss' party'
  -- Update the periodic timer.
  let timer' = (csPeriodicTimer cs + 1) `mod` framesPerRound
  -- Decay fields.
  fields' <- decayFields 1 (acsFields acs)
  -- Ready characters take precedence over ready monsters.
  let mbInterrupt = if timer' == 0 then Just DoPeriodicAction
                    else (DoActivateCharacter <$> mbCharNum) <|>
                         (DoRunScript <$> mbScript)
  return (cs { csCharStates = ccss',
               csCommon = acs { acsFields = fields', acsMonsters = monsters',
                                acsParty = party' },
               csPeriodicTimer = timer' }, mbInterrupt)
  where
    tickPartyWaiting party =
      party { partyCharacters = tickCharWaiting <$> partyCharacters party }
    tickCharWaiting char =
      char { chrStatus = decayStatusEffects roundsPerFrame (chrStatus char) }
    tickCharStatesWaiting ccss =
      TM.make $ \charNum ->
        tickCharStateWaiting (arsGetCharacter charNum cs) (TM.get charNum ccss)

doTickExecution :: CombatState -> CombatExecution
                -> IO (CombatState, Maybe Interrupt)
doTickExecution cs ce = do
  (cs', mbScriptInterrupt) <- executeScript cs (ceScript ce)
  case mbScriptInterrupt of
    Nothing -> do
      -- TODO see if there are more triggers to run
      if (noMoreEnemyMonsters cs' ||
          cePendingEndCombat ce && csCanRunAway cs' &&
          not (arsAreEnemiesNearby cs'))
      then return (cs', Just DoEndCombat) else do
      let endTurn cs'' = do
            let mbInterrupt = do
                  charNum <- cePendingCharacter ce
                  guard (csCharCanTakeTurn cs charNum)
                  Just (DoActivateCharacter charNum)
            return (cs'' { csPhase = WaitingPhase }, mbInterrupt)
      case ceCommander ce of
        Just cc -> do
          if (chrIsConscious $ arsGetCharacter (ccCharacterNumber cc) cs') &&
             hasEnoughActionPoints cs' cc 1
          then return (cs' { csPhase = CommandPhase cc }, Nothing)
          else endTurn (subtractUsedActionPoints cc cs')
        Nothing -> endTurn cs'
    Just (script, interrupt) -> do
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
    ResultFailure message -> do
      return (cs, Just (return (), DoNarrate ("SCRIPT ERROR:\n" ++ message)))

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
        EffFork script ->
          return (cs, mapEffect EffCombatArea script `also_` sfn (), Nothing)
        EffGameOver -> return (cs, sfn (), Just DoGameOver)
        EffIfCombat script _ -> return (cs, script >>= sfn, Nothing)
        EffMultiChoice _ _ _ ->
          fail "FIXME Combat executeEffect EffMultiChoice"
        EffNarrate text -> return (cs, sfn (), Just (DoNarrate text))
        EffWait -> return (cs, sfn (), Just DoWait)
    EffEndCombat -> return (cs, sfn (), Just DoEndCombat)
    EffGetCharFaceDir charNum -> do
      return (cs, sfn $ cpFaceDir $ ccsPose $ csGetCharState cs charNum,
              Nothing)
    EffGetCharMoments charNum -> do
      return (cs, sfn $ ccsMoments $ csGetCharState cs charNum, Nothing)
    EffSetCharAnim charNum anim -> do
      let setAnim ccs = ccs { ccsPose = (ccsPose ccs) { cpAnim = anim } }
      return (csAlterCharState charNum setAnim cs, sfn (), Nothing)
    EffSetCharFaceDir charNum faceDir -> do
      let setFace ccs = ccs { ccsPose = (ccsPose ccs) { cpFaceDir = faceDir } }
      return (csAlterCharState charNum setFace cs, sfn (), Nothing)
    EffSetCharMoments charNum moments -> do
      let setMoments ccs = ccs { ccsMoments = moments }
      return (csAlterCharState charNum setMoments cs, sfn (), Nothing)
    EffSetCharPosition charNum pos -> do
      let setPos ccs = ccs { ccsPosition = pos }
      cs' <- updateCombatVisibility cs
               { csCharStates = TM.adjust charNum setPos (csCharStates cs) }
      return (cs', sfn (), Nothing)

-------------------------------------------------------------------------------

ccssCharThatWantsATurn :: TM.TotalMap CharacterNumber CombatCharState
                       -> Party -> Maybe CharacterNumber
ccssCharThatWantsATurn ccss party =
  let fn charNum = ccsWantsTurn (TM.get charNum ccss) &&
                   chrCanTakeTurn (partyGetCharacter party charNum)
  in find fn [minBound .. maxBound]

noMoreEnemyMonsters :: CombatState -> Bool
noMoreEnemyMonsters = all monstIsAlly . Grid.values . arsMonsters

tickMonsterWaiting :: Grid.Entry Monster
                   -> (Monster, Maybe (Script CombatEffect ()))
tickMonsterWaiting entry = (monst', mbScript) where
  key = Grid.geKey entry
  monst = Grid.geValue entry
  monst' = monst { monstMoments = moments', monstStatus = status',
                   monstSummoning = summoning' }
  mbScript = if maybe False ((0 >=) . msRemainingFrames) summoning'
             then Just (unsummonMonster key)
             else if moments' >= maxMoments
                  then Just (defaultMonsterCombatAI key >> resetMoments)
                  else Nothing
  resetMoments = do
    monsters <- areaGet arsMonsters
    maybeM (Grid.lookup key monsters) $ \entry' -> do
      emitAreaEffect $ EffReplaceMonster key $
        Just (Grid.geValue entry') { monstMoments = 0 }
  moments' = max moments $ min maxMoments $
             round (monstSpeed monst * seSpeedMultiplier status *
                    fromIntegral baseMomentsPerFrame) + moments
  moments = monstMoments monst
  status' = decayStatusEffects roundsPerFrame status
  status = monstStatus monst
  summoning' = tickSummoning <$> monstSummoning monst
  tickSummoning ms = ms { msRemainingFrames = max 0 $ subtract 1 $
                                              msRemainingFrames ms }

-------------------------------------------------------------------------------
