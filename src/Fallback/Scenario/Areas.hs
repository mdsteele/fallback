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
  (areaEntrance, areaLinks, areaLocation, areaTerrain, enterPartyIntoArea)
where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Fallback.Control.Error (IOEO, onlyIO)
import Fallback.Data.Clock (initClock)
import qualified Fallback.Data.Grid as Grid (empty)
import Fallback.Data.Point (IPoint, Point(Point), Position, half, pSub)
import qualified Fallback.Data.SparseMap as SM (make)
import Fallback.Scenario.Triggers
import Fallback.State.Area (AreaCommonState(..), TownEffect)
import Fallback.State.Camera (makeCameraWithCenter)
import Fallback.State.Creature (CreatureAnim(NoAnim), CreaturePose(..))
import Fallback.State.Doodad (emptyDoodads)
import Fallback.State.Minimap (newMinimapFromTerrain)
import Fallback.State.Party (Party(partyCurrentArea), partyExploredMap)
import Fallback.State.Resources (Resources)
import Fallback.State.Simple (MarkKey, deltaFaceDir)
import Fallback.State.Tags (AreaTag(..))
import Fallback.State.Terrain
import Fallback.State.Town
import Fallback.State.Trigger (Trigger, makeUnfiredTriggers)

-------------------------------------------------------------------------------

areaEntrance :: AreaTag -> AreaTag -> MarkKey
areaEntrance = getAreaEntrance scenarioTriggers

areaLinks :: AreaTag -> Set.Set AreaTag
areaLinks = getAreaLinks scenarioTriggers

areaLocation :: AreaTag -> IPoint
areaLocation MountainPath = Point 314 293
areaLocation Corenglen = Point 389 348
areaLocation FrozenPass = Point 110 130
areaLocation Holmgare = Point 175 170
areaLocation SewerCaves = Point 175 130
areaLocation PerilousRoad = Point 255 255
areaLocation StoneBridge = Point 303 328
areaLocation Tragorda = Point 412 335
areaLocation WhistlingWoods = Point 386 261
areaLocation IcyConfluence = Point 340 215
areaLocation Marata = Point 332 164
areaLocation IronMine = Point 351 100
areaLocation NorthernTundra = Point 406 162
areaLocation Duskwood = Point 473 261
areaLocation Icehold = Point 532 184
areaLocation _ = Point 100 100 -- FIXME

areaTerrain :: Party -> AreaTag -> String
areaTerrain = getAreaTerrain scenarioTriggers

areaTriggers :: AreaTag -> [Trigger TownState TownEffect]
areaTriggers = getAreaTriggers scenarioTriggers

-------------------------------------------------------------------------------

enterPartyIntoArea :: Resources -> Party -> AreaTag -> Either Position MarkKey
                   -> IOEO TownState
enterPartyIntoArea resources origParty tag destination = do
  let party = origParty { partyCurrentArea = tag }
  tmap <- loadTerrainMap resources (areaTerrain party tag)
  position <- do
    case destination of
      Left pos -> return pos
      Right mark -> do
        case Set.toList $ tmapLookupMark mark tmap of
          [pos] -> return pos
          _ -> fail ("No such entrance mark: " ++ show mark)
  let terrain = Terrain { terrainMap = tmap, terrainOverrides = Map.empty }
  let mapCenter = fmap half $ uncurry Point $ terrainSize terrain
  minimap <- onlyIO $ newMinimapFromTerrain terrain $
             partyExploredMap terrain party
  onlyIO $ updateTownVisibility $ TownState
    { tsActiveCharacter = minBound,
      tsCommon = AreaCommonState
        { acsCamera = makeCameraWithCenter (positionCenter position),
          acsClock = initClock,
          acsDevices = Grid.empty,
          acsDoodads = emptyDoodads,
          acsFields = Map.empty,
          acsMessage = Nothing,
          acsMinimap = minimap,
          acsMonsters = Grid.empty,
          acsParty = party,
          acsRemains = SM.make [],
          acsResources = resources,
          acsTerrain = terrain,
          acsVisible = Set.empty },
      tsPartyPose = CreaturePose
        { cpAlpha = 255, cpAnim = NoAnim,
          cpFaceDir = deltaFaceDir (mapCenter `pSub` position) },
      tsPartyPosition = position,
      tsPhase = WalkingPhase,
      tsTriggers = makeUnfiredTriggers (areaTriggers tag) }

-------------------------------------------------------------------------------
