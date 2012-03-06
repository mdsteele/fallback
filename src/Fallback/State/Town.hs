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

import qualified Data.Map as Map
import qualified Data.Set as Set

import Fallback.Constants
  (cameraCenterOffset, combatArenaCols, combatArenaRows, sightRangeSquared)
import Fallback.Control.Script (Script)
import Fallback.Data.Clock (Clock, clockInc)
import Fallback.Data.Grid (Grid, gridUpdate)
import Fallback.Data.Point
import qualified Fallback.Data.SparseMap as SM
import Fallback.Draw (Minimap)
import Fallback.State.Area
import Fallback.State.Camera (Camera, tickCamera)
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
    tsCamera :: Camera,
    tsClock :: Clock,
    tsDevices :: Grid Device,
    tsDoodads :: Doodads,
    tsFields :: Map.Map Position Field,
    tsMessage :: Maybe Message,
    tsMinimap :: Minimap,
    tsMonsters :: Grid Monster,
    tsParty :: Party,
    tsPartyAnim :: CreatureAnim,
    tsPartyFaceDir :: FaceDir,
    tsPartyPosition :: Position,
    tsPhase :: TownPhase,
    tsTerrain :: TerrainMap,
    tsTriggersFired :: [Trigger TownState TownEffect],
    tsTriggersReady :: [Trigger TownState TownEffect],
    tsVisible :: Set.Set Position }

instance AreaState TownState where
  arsArenaTopleft ts = tsPartyPosition ts `pSub`
                       Point (half combatArenaCols) (half combatArenaRows)
  arsCharacterPosition _ = tsPartyPosition
  arsCharacterAtPosition pos ts = if pos /= tsPartyPosition ts then Nothing
                                  else Just (tsActiveCharacter ts)
  arsDevices = tsDevices
  arsFields = tsFields
  arsMonsters = tsMonsters
  arsParty = tsParty
  arsPartyPositions = (:[]) . tsPartyPosition
  arsResources = partyResources . tsParty
  arsTerrain = tsTerrain
  arsVisibleForCharacter _ = tsVisible
  arsVisibleForParty = tsVisible

instance HasProgress TownState where
  getProgress = getProgress . tsParty

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
-- TownState setters:

tsSetMessage :: String -> TownState -> TownState
tsSetMessage text ts = ts { tsMessage = Just (makeMessage text) }

-------------------------------------------------------------------------------

updateTownVisibility :: TownState -> IO TownState
updateTownVisibility ts = do
  let terrain = tsTerrain ts
  let visible' = fieldOfView (tmapSize terrain) (arsIsOpaque ts)
                             sightRangeSquared (tsPartyPosition ts) Set.empty
  let party' = partyUpdateExploredMap terrain visible' (tsParty ts)
  updateMinimap (tsMinimap ts) terrain (Set.toList visible')
  return ts { tsParty = party', tsVisible = visible' }

tickTownAnimations :: TownState -> TownState
tickTownAnimations ts =
  ts { tsClock = clockInc (tsClock ts),
       tsCamera = tickCamera (positionCenter (tsPartyPosition ts) `pSub`
                              cameraCenterOffset) (tsCamera ts),
       tsDoodads = tickDoodads (tsDoodads ts),
       tsMessage = tsMessage ts >>= decayMessage,
       tsMonsters = gridUpdate tickMonsterAnim (tsMonsters ts),
       tsPartyAnim = tickCreatureAnim (tsPartyAnim ts) }

-------------------------------------------------------------------------------

data TownCheatCode = IAmLeTired
                   | Plugh Int
                   | WhereAmI
                   | Xyzzy AreaTag Int Int
  deriving (Read, Show)

-------------------------------------------------------------------------------
