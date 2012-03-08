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

import Fallback.Constants
  (cameraCenterOffset, combatArenaCols, combatArenaRows, sightRangeSquared)
import Fallback.Control.Script (Script)
import Fallback.Data.Point
import qualified Fallback.Data.SparseMap as SM
import Fallback.State.Area
import Fallback.State.Creature (CreatureAnim(..), tickCreatureAnim)
import Fallback.State.FOV (fieldOfView)
import Fallback.State.Party
import Fallback.State.Progress (HasProgress, getProgress)
import Fallback.State.Simple
import Fallback.State.Tags (AreaTag, ItemTag)
import Fallback.State.Terrain

-------------------------------------------------------------------------------
-- TownState datatype:

data TownState = TownState
  { tsActiveCharacter :: CharacterNumber,
    tsCommon :: AreaCommonState,
    tsPartyAnim :: CreatureAnim,
    tsPartyFaceDir :: FaceDir,
    tsPartyPosition :: Position,
    tsPhase :: TownPhase,
    tsTriggersFired :: [Trigger TownState TownEffect],
    tsTriggersReady :: [Trigger TownState TownEffect] }

instance AreaState TownState where
  arsArenaTopleft ts = tsPartyPosition ts `pSub`
                       Point (half combatArenaCols) (half combatArenaRows)
  arsCharacterPosition _ = tsPartyPosition
  arsCharacterAtPosition pos ts = if pos /= tsPartyPosition ts then Nothing
                                  else Just (tsActiveCharacter ts)
  arsCommon = tsCommon
  arsSetCommon ts acs = ts { tsCommon = acs }
  arsPartyPositions = (:[]) . tsPartyPosition
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
--                | ConversationPhase (Conversation TownEffect)

-------------------------------------------------------------------------------

updateTownVisibility :: TownState -> IO TownState
updateTownVisibility ts = do
  let acs = tsCommon ts
  let terrainMap = acsTerrainMap acs
  let visible' = fieldOfView (tmapSize terrainMap) (arsIsOpaque ts)
                             sightRangeSquared (tsPartyPosition ts) Set.empty
  let party' = partyUpdateExploredMap terrainMap visible' (arsParty ts)
  updateMinimap acs (Set.toList visible')
  return ts { tsCommon = acs { acsParty = party', acsVisible = visible' } }

tickTownAnimations :: TownState -> TownState
tickTownAnimations ts =
  let acs' = tickAnimations (positionCenter (tsPartyPosition ts) `pSub`
                             cameraCenterOffset) (tsCommon ts)
  in ts { tsCommon = acs', tsPartyAnim = tickCreatureAnim (tsPartyAnim ts) }

-------------------------------------------------------------------------------

data TownCheatCode = IAmLeTired
                   | Plugh Int
                   | WhereAmI
                   | Xyzzy AreaTag Int Int
  deriving (Read, Show)

-------------------------------------------------------------------------------
