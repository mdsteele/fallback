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
   enterPartyIntoArea, startingArea, startingPosition)
where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Fallback.Control.Error (IOEO, onlyIO)
import Fallback.Data.Clock (initClock)
import Fallback.Data.Grid (emptyGrid)
import Fallback.Data.Point (IPoint, Point(Point), Position)
import Fallback.Scenario.Triggers
import Fallback.State.Area
  (AreaCommonState(..), TownEffect, Trigger, emptyDoodads)
import Fallback.State.Camera (makeCameraWithCenter)
import Fallback.State.Creature (CreatureAnim(NoAnim))
import Fallback.State.Minimap (newMinimapFromTerrain)
import Fallback.State.Party (Party(partyCurrentArea), partyExploredMap)
import Fallback.State.Resources (Resources)
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

enterPartyIntoArea :: Resources -> Party -> AreaTag -> Position
                   -> IOEO TownState
enterPartyIntoArea resources origParty tag position = do
  let party = origParty { partyCurrentArea = tag }
  tmap <- loadTerrainMap resources (areaTerrain party tag)
  let terrain = Terrain { terrainMap = tmap, terrainOverrides = Map.empty }
  minimap <- onlyIO $ newMinimapFromTerrain terrain $
             partyExploredMap terrain party
  onlyIO $ updateTownVisibility $ TownState
    { tsActiveCharacter = minBound,
      tsCommon = AreaCommonState
        { acsCamera = makeCameraWithCenter (positionCenter position),
          acsClock = initClock,
          acsDevices = emptyGrid,
          acsDoodads = emptyDoodads,
          acsFields = Map.empty,
          acsMessage = Nothing,
          acsMinimap = minimap,
          acsMonsters = emptyGrid,
          acsParty = party,
          acsResources = resources,
          acsTerrain = terrain,
          acsVisible = Set.empty },
      tsPartyAnim = NoAnim,
      tsPartyFaceDir = FaceRight, -- TODO face towards center of map
      tsPartyPosition = position,
      tsPhase = WalkingPhase,
      tsTriggersFired = [],
      tsTriggersReady = areaTriggers tag }

-------------------------------------------------------------------------------
