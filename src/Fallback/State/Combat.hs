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

{-# LANGUAGE ExistentialQuantification #-}

module Fallback.State.Combat where

import Data.Foldable (toList)
import Data.List (find)
import qualified Data.Set as Set

import Fallback.Constants
  (baseMomentsPerFrame, combatArenaSize, maxActionPoints,
   momentsPerActionPoint, sightRangeSquared)
import Fallback.Control.Script (Script)
import qualified Fallback.Data.Grid as Grid (Grid)
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM
import Fallback.State.Area
import Fallback.State.Creature (CreaturePose, Monster)
import Fallback.State.Doodad (makeMessage)
import Fallback.State.FOV (fieldOfView)
import Fallback.State.Party
import Fallback.State.Progress (HasProgress, getProgress)
import Fallback.State.Simple
  (ActionPoints, APModifier, CastingCost, CharacterNumber, CostModifier,
   PowerModifier)
import Fallback.State.Tags (FeatTag, ItemTag)
import Fallback.State.Terrain (terrainSize)
import Fallback.State.Town (TownState)
import Fallback.State.Trigger (Triggers)

-------------------------------------------------------------------------------
-- CombatState datatype:

{-
Combat starts if:
  - a town trigger executes a `combat' statement and any enemies are in view
  - a wandering monster passes near the party
  - the player asks for combat to start, with an enemy close enough

Combat ends if:
  - all enemies are defeated
  - a combat trigger executes an `end combat' statement
  - the player asks for combat to end, and no enemies are close by
-}

data CombatState = CombatState
  { csArenaTopleft :: Position,
    csCanRunAway :: Bool, -- Can the player ask combat to end?
    csCharStates :: TM.TotalMap CharacterNumber CombatCharState,
    csCommon :: AreaCommonState,
    csMonstersNotInArena :: Grid.Grid Monster,
    csPeriodicTimer :: Int,
    csPhase :: CombatPhase,
    csTownTriggers :: Triggers TownState TownEffect,
    csTriggers :: Triggers CombatState CombatEffect }

instance AreaState CombatState where
  arsBoundaryRect cs = makeRect (csArenaTopleft cs) combatArenaSize
  arsCharacterPosition charNum = ccsPosition . TM.get charNum . csCharStates
  arsCharacterAtPosition pos cs =
    let present charNum = arsCharacterPosition charNum cs == pos &&
                          chrIsConscious (arsGetCharacter charNum cs)
    in find present [minBound .. maxBound]
  arsCommon = csCommon
  arsSetCommon cs acs = cs { csCommon = acs }
  arsPartyPositions cs =
    map (ccsPosition . snd) $
    filter (chrIsConscious . (partyGetCharacter $ arsParty cs) . fst) $
    TM.assocs $ csCharStates cs
  arsUpdateVisibility = updateCombatVisibility
  arsVisibleForCharacter charNum = ccsVisible . TM.get charNum . csCharStates

instance HasProgress CombatState where
  getProgress = getProgress . acsParty . csCommon

-------------------------------------------------------------------------------
-- CombatState setters:

csAlterCharState :: CharacterNumber -> (CombatCharState -> CombatCharState)
                 -> CombatState -> CombatState
csAlterCharState charNum fn cs =
  cs { csCharStates = TM.adjust charNum fn (csCharStates cs) }

csSetMessage :: String -> CombatState -> CombatState
csSetMessage text cs =
  cs { csCommon = (csCommon cs) { acsMessage = Just (makeMessage text) } }

-------------------------------------------------------------------------------

csCommander :: CombatState -> Maybe CombatCommander
csCommander cs =
  case csPhase cs of
    WaitingPhase -> Nothing
    CommandPhase cc -> Just cc
    ChooseAbilityPhase cc -> Just cc
    MetaAbilityPhase cm -> Just (cmCommander cm)
    InventoryPhase cc _ -> Just cc
    TargetingPhase ct -> Just (ctCommander ct)
    ExecutionPhase ce -> ceCommander ce

csCharCanTakeTurn :: CombatState -> CharacterNumber -> Bool
csCharCanTakeTurn cs charNum =
  chrCanTakeTurn (arsGetCharacter charNum cs) &&
  ccsActionPoints (csGetCharState cs charNum) >= 1

csGetCharState :: CombatState -> CharacterNumber -> CombatCharState
csGetCharState cs charNum = TM.get charNum (csCharStates cs)

hasEnoughActionPoints :: CombatState -> CombatCommander -> Int -> Bool
hasEnoughActionPoints cs cc apNeeded =
  let charNum = ccCharacterNumber cc
      startingAp = ccsActionPoints $ TM.get charNum $ csCharStates cs
      apAlreadyUsed = ccActionPointsUsed cc
  in if startingAp >= maxActionPoints
     then apAlreadyUsed < maxActionPoints
     else startingAp - apAlreadyUsed >= apNeeded

-------------------------------------------------------------------------------

data CombatPhase = WaitingPhase
                 | CommandPhase CombatCommander
                 | ChooseAbilityPhase CombatCommander
                 | MetaAbilityPhase CombatMetability {-
                 | MetaAttackPhase CombatCommander -}
                 | InventoryPhase CombatCommander (Maybe ItemTag)
                 | TargetingPhase CombatTargeting
                 | ExecutionPhase CombatExecution

data CombatCommander = CombatCommander
  { ccActionPointsUsed :: ActionPoints,
    ccCharacterNumber :: CharacterNumber }

data CombatMetability = CombatMetability
  { cmAPModifier :: APModifier,
    cmCommander :: CombatCommander,
    cmCostModifier :: CostModifier,
    cmFeatTag :: FeatTag,
    cmPowerModifier :: PowerModifier }

data CombatTargeting = forall a. CombatTargeting
  { ctActionPointsNeeded :: ActionPoints,
    ctCastingCost :: CastingCost,
    ctCommander :: CombatCommander,
    ctScriptFn :: a -> Script CombatEffect (),
    ctTargeting :: Targeting a }

data CombatExecution = CombatExecution
  { ceCommander :: Maybe CombatCommander,
    cePendingCharacter :: Maybe CharacterNumber,
    cePendingEndCombat :: Bool,
    ceScript :: Script CombatEffect () }

-------------------------------------------------------------------------------

data CombatCharState = CombatCharState
  { ccsMoments :: Int,
    ccsPose :: CreaturePose,
    ccsPosition :: Position,
    ccsVisible :: Set.Set Position,
    ccsWantsTurn :: Bool }

ccsActionPoints :: CombatCharState -> Int
ccsActionPoints ccs = ccsMoments ccs `div` momentsPerActionPoint

tickCharStateWaiting :: Character -> CombatCharState -> CombatCharState
tickCharStateWaiting char ccs =
  ccs { ccsMoments = moments',
        ccsWantsTurn = ccsWantsTurn ccs || wantsTurn }
  where
    moments' = if not (chrIsConscious char) then 0 else
                 max (ccsMoments ccs) $
                 min (momentsPerActionPoint * maxActionPoints) $
                 ccsMoments ccs +
                 round (chrSpeed char * fromIntegral baseMomentsPerFrame)
    wantsTurn = aps < maxActionPoints && aps' >= maxActionPoints
    aps = ccsActionPoints ccs
    aps' = moments' `div` momentsPerActionPoint

-------------------------------------------------------------------------------

updateCombatVisibility :: CombatState -> IO CombatState
updateCombatVisibility cs = do
  let acs = csCommon cs
  let terrain = acsTerrain acs
  let updateCcs charNum ccs = ccs { ccsVisible =
        if not $ chrIsConscious $ partyGetCharacter (acsParty acs) charNum
        then Set.empty else
          fieldOfView (terrainSize terrain) (arsIsOpaque cs) sightRangeSquared
                      (ccsPosition ccs) Set.empty }
  let ccss' = TM.mapWithKey updateCcs (csCharStates cs)
  let visible' = Set.unions $ map ccsVisible $ toList ccss'
  let party' = partyUpdateExploredMap terrain visible' (acsParty acs)
  updateMinimap acs (Set.toList visible')
  return cs { csCharStates = ccss',
              csCommon = acs { acsParty = party', acsVisible = visible' } }

-------------------------------------------------------------------------------
