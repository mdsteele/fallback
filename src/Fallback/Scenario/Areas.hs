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

module Fallback.Scenario.Areas
  (areaEntrance, areaLinks, areaLocation, areaTerrain,
   enterPartyIntoArea, createMinimap, startingArea, startingPosition)
where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Fallback.Control.Error (IOEO, onlyIO)
import Fallback.Data.Clock (initClock)
import Fallback.Data.Grid (emptyGrid)
import Fallback.Data.Point (IPoint, Point(Point), Position)
import Fallback.Draw (Minimap, newMinimap)
import Fallback.Scenario.Triggers
import Fallback.State.Area (TownEffect, Trigger, emptyDoodads)
import Fallback.State.Camera (makeCameraWithCenter)
import Fallback.State.Creature (CreatureAnim(NoAnim))
import Fallback.State.Party
  (Party(partyCurrentArea), partyExploredMap, partyResources)
import Fallback.State.Simple (FaceDir(..))
import Fallback.State.Tags (AreaTag(..))
import Fallback.State.Terrain
import Fallback.State.Town

-------------------------------------------------------------------------------

areaEntrance :: AreaTag -> AreaTag -> Position
areaEntrance = getAreaEntrance scenarioTriggers

areaLinks :: AreaTag -> [AreaTag]
areaLinks = getAreaLinks scenarioTriggers

areaLocation :: AreaTag -> IPoint
areaLocation MountainPath = Point 314 293
areaLocation Corenglen = Point 389 348
areaLocation _ = Point 100 100 -- FIXME

areaTerrain :: Party -> AreaTag -> String
areaTerrain = getAreaTerrain scenarioTriggers

areaTriggers :: AreaTag -> [Trigger TownState TownEffect]
areaTriggers = getAreaTriggers scenarioTriggers

-------------------------------------------------------------------------------

enterPartyIntoArea :: Party -> AreaTag -> Position -> IOEO TownState
enterPartyIntoArea origParty tag position = do
  let party = origParty { partyCurrentArea = tag }
  terrain <- loadTerrainMap (partyResources party) (areaTerrain party tag)
  minimap <- onlyIO $ createMinimap terrain party
  onlyIO $ updateTownVisibility $ TownState
    { tsActiveCharacter = minBound,
      tsCamera = makeCameraWithCenter (positionCenter position),
      tsClock = initClock,
      tsDevices = emptyGrid,
      tsDoodads = emptyDoodads,
      tsFields = Map.empty,
      tsMessage = Nothing,
      tsMinimap = minimap,
      tsMonsters = emptyGrid,
      tsParty = party,
      tsPartyAnim = NoAnim,
      tsPartyFaceDir = FaceRight, -- TODO face towards center of map
      tsPartyPosition = position,
      tsPhase = WalkingPhase,
      tsTerrain = terrain,
      tsTriggersFired = [],
      tsTriggersReady = areaTriggers tag,
      tsVisible = Set.empty }

createMinimap :: TerrainMap -> Party -> IO Minimap
createMinimap terrain party = do
  minimap <- newMinimap $ tmapSize terrain
  updateMinimap minimap terrain $
    filter (partyExploredMap terrain party `hasExplored`) $
    tmapAllPositions terrain
  return minimap

-------------------------------------------------------------------------------
