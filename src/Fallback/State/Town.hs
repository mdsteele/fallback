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

module Fallback.State.Town where

import qualified Data.Set as Set

import Fallback.Constants (cameraCenterOffset, sightRangeSquared)
import Fallback.Control.Script (Script)
import Fallback.Data.Point
import qualified Fallback.Data.SparseMap as SM
import Fallback.State.Area
import Fallback.State.Creature (CreaturePose, tickCreaturePose)
import Fallback.State.FOV (fieldOfView)
import Fallback.State.Party
import Fallback.State.Progress (HasProgress, getProgress)
import Fallback.State.Simple
import Fallback.State.Status (Invisibility(NoInvisibility))
import Fallback.State.Tags (AreaTag, ItemTag)
import Fallback.State.Terrain (positionCenter, terrainSize)

-------------------------------------------------------------------------------
-- TownState datatype:

data TownState = TownState
  { tsActiveCharacter :: CharacterNumber,
    tsCommon :: AreaCommonState,
    tsPartyPose :: CreaturePose,
    tsPartyPosition :: Position,
    tsPhase :: TownPhase,
    tsTriggersFired :: [Trigger TownState TownEffect],
    tsTriggersReady :: [Trigger TownState TownEffect] }

instance AreaState TownState where
  arsBoundaryRect ts = makeRect pZero $ terrainSize $ arsTerrain ts
  arsCharacterPosition _ = tsPartyPosition
  arsCharacterAtPosition pos ts = if pos /= tsPartyPosition ts then Nothing
                                  else Just (tsActiveCharacter ts)
  arsCommon = tsCommon
  arsSetCommon ts acs = ts { tsCommon = acs }
  arsPartyPositions = (:[]) . tsPartyPosition
  arsUpdateVisibility = updateTownVisibility
  arsVisibleForCharacter _ = acsVisible . tsCommon

instance HasProgress TownState where
  getProgress = getProgress . acsParty . tsCommon

data TownTargeting = forall a. TownTargeting
  { ttCastingCost :: CastingCost,
    ttScriptFn :: a -> Script TownEffect () ,
    ttTargeting :: Targeting a }

data TownPhase = WalkingPhase
               | ChooseAbilityPhase
               | InventoryPhase (Maybe ItemTag)
               | UpgradePhase (SM.SparseMap (CharacterNumber, Stat) Int)
                              (SM.SparseMap (CharacterNumber,
                                             AbilityNumber) Int)
               | TargetingPhase TownTargeting
               | ScriptPhase (Script TownEffect ())
               | ShoppingPhase (Maybe ItemTag) [Either Ingredient ItemTag]
                               (Script TownEffect ())

-------------------------------------------------------------------------------

updateTownVisibility :: TownState -> IO TownState
updateTownVisibility ts = do
  let acs = tsCommon ts
  let terrain = acsTerrain acs
  let visible' = fieldOfView (terrainSize terrain) (arsIsOpaque ts)
                             sightRangeSquared (tsPartyPosition ts) Set.empty
  let party' = partyUpdateExploredMap terrain visible' (arsParty ts)
  updateMinimap acs (Set.toList visible')
  return ts { tsCommon = acs { acsParty = party', acsVisible = visible' } }

tickTownAnimations :: TownState -> TownState
tickTownAnimations ts =
  let acs' = tickAnimations (positionCenter (tsPartyPosition ts) `pSub`
                             cameraCenterOffset)
                            (arsAllyOccupiedPositions ts) (tsCommon ts)
  in ts { tsCommon = acs',
          tsPartyPose = tickCreaturePose NoInvisibility True (tsPartyPose ts) }

-------------------------------------------------------------------------------

data TownCheatCode = Gimme ItemTag
                   | IAmLeTired
                   | Plugh Int
                   | StuffMart
                   | WhereAmI
                   | Xyzzy AreaTag Int Int
  deriving (Read, Show)

-------------------------------------------------------------------------------
