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

module Fallback.State.Minimap
  (newMinimapFromTerrain, updateMinimapFromTerrain,
   newMinimapFromTerrainMap, updateMinimapFromTerrainMap)
where

import Fallback.Data.Point (Position)
import Fallback.Draw (Minimap, alterMinimap, newMinimap)
import Fallback.State.Terrain

-------------------------------------------------------------------------------

newMinimapFromTerrain :: Terrain -> ExploredMap -> IO Minimap
newMinimapFromTerrain terrain explored = do
  minimap <- newMinimap $ terrainSize terrain
  updateMinimapFromTerrain minimap terrain $
    filter (explored `hasExplored`) $ tmapAllPositions $ terrainMap terrain
  return minimap

updateMinimapFromTerrain :: Minimap -> Terrain -> [Position] -> IO ()
updateMinimapFromTerrain minimap terrain positions = do
  let posToColor pos = (pos, ttColor $ terrainGetTile pos terrain)
  alterMinimap minimap $ map posToColor positions

newMinimapFromTerrainMap :: TerrainMap -> IO Minimap
newMinimapFromTerrainMap tmap = do
  minimap <- newMinimap $ tmapSize tmap
  updateMinimapFromTerrainMap minimap tmap $ tmapAllPositions tmap
  return minimap

updateMinimapFromTerrainMap :: Minimap -> TerrainMap -> [Position] -> IO ()
updateMinimapFromTerrainMap minimap tmap positions = do
  let posToColor pos = (pos, ttColor $ tmapGet tmap pos)
  alterMinimap minimap $ map posToColor positions

-------------------------------------------------------------------------------
