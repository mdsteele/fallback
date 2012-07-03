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

module Fallback.State.Terrain
  (-- * Terrain tiles
   TerrainTile(..),
   -- * Terrain
   Terrain(..), terrainSize, terrainOffTile, terrainGetTile, terrainSetTile,
   -- * Terrain maps
   TerrainMap, makeEmptyTerrainMap, tmapSize, tmapName, tmapOffTile,
   tmapAllPositions, tmapGet, tmapSet, tmapResize, tmapShift,
   loadTerrainMap, saveTerrainMap,
   -- * Positions
   positionRect, positionTopleft, positionCenter, pointPosition, prectRect,
   -- * Explored maps
   ExploredMap, unexploredMap, hasExplored, setExplored)
where

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Traversable as Trav (mapM)
import qualified Text.Read as Read (readPrec)

import Fallback.Constants (tileHeight, tileWidth)
import Fallback.Control.Error
import Fallback.Data.Point
import Fallback.Resource (getResourcePath)
import Fallback.State.Resources (Resources, rsrcTileset)
import Fallback.State.Tileset

-------------------------------------------------------------------------------

data Terrain = Terrain
  { terrainMap :: TerrainMap,
    terrainOverrides :: Map.Map Position TerrainTile }

terrainSize :: Terrain -> (Int, Int)
terrainSize = tmapSize . terrainMap

terrainOffTile :: Terrain -> TerrainTile
terrainOffTile = tmapOffTile . terrainMap

terrainGetTile :: Position -> Terrain -> TerrainTile
terrainGetTile pos terrain =
  fromMaybe (tmapGet (terrainMap terrain) pos) $
  Map.lookup pos $ terrainOverrides terrain

terrainSetTile :: Position -> TerrainTile -> Terrain -> Terrain
terrainSetTile pos tile terrain = terrain { terrainOverrides = over' } where
  over' = (if ttId tile == ttId (tmapGet (terrainMap terrain) pos)
           then Map.delete pos else Map.insert pos tile)
          (terrainOverrides terrain)

-------------------------------------------------------------------------------

data TerrainMap = TerrainMap
  { tmapArray :: Array Position TerrainTile,
    tmapName :: String,
    tmapOffTile :: TerrainTile }

makeEmptyTerrainMap :: (Int, Int) -> TerrainTile -> TerrainTile -> TerrainMap
makeEmptyTerrainMap (w, h) offTile nullTile = TerrainMap
  { tmapArray = listArray (Point 0 0, Point (w - 1) (h - 1)) (repeat nullTile),
    tmapName = "", tmapOffTile = offTile }

tmapSize :: TerrainMap -> (Int, Int)
tmapSize tmap =
  let Point maxX maxY = snd $ bounds $ tmapArray tmap in (maxX + 1, maxY + 1)

tmapAllPositions :: TerrainMap -> [Position]
tmapAllPositions = range . bounds . tmapArray

tmapGet :: TerrainMap -> Position -> TerrainTile
tmapGet tmap pos =
  if inRange (bounds arr) pos then arr ! pos else tmapOffTile tmap
  where arr = tmapArray tmap

tmapSet :: [Position] -> TerrainTile -> TerrainMap -> TerrainMap
tmapSet ps tile tmap = tmap { tmapArray = arr // updates } where
  updates = map (flip (,) tile) $ filter (inRange $ bounds arr) ps
  arr = tmapArray tmap

tmapResize :: TerrainTile -> (Int, Int) -> TerrainMap -> TerrainMap
tmapResize nullTile (w, h) tmap = tmap { tmapArray = arr' } where
  arr' = listArray bound (repeat nullTile) //
         (filter (inRange bound . fst) $ assocs $ tmapArray tmap)
  bound = (Point 0 0, Point (w - 1) (h - 1))

tmapShift :: TerrainTile -> IPoint -> TerrainMap -> TerrainMap
tmapShift nullTile delta tmap = tmap { tmapArray = arr' } where
  arr' = listArray bound (repeat nullTile) //
         (filter (inRange bound . fst) $ map shift $ assocs $ tmapArray tmap)
  shift (point, tile) = (point `pAdd` delta, tile)
  bound = bounds (tmapArray tmap)

-------------------------------------------------------------------------------

loadTerrainMap :: Resources -> String -> IOEO TerrainMap
loadTerrainMap resources name = do
  let tileset = rsrcTileset resources
  path <- onlyIO $ getResourcePath "terrain" name
  let getTile tid = maybe (fail $ "Bad tile id: " ++ show tid) return $
                    tilesetLookup tid tileset
  tileArray <- do
    string <- onlyIO $ readFile path
    ((w, h), ids) <- maybe (fail $ "Couldn't read terrain from " ++ name)
                         (return . fst) (listToMaybe $ reads string)
    onlyEO $ Trav.mapM getTile $
      listArray (Point 0 0, Point (w - 1) (h - 1)) ids
  return TerrainMap { tmapArray = tileArray, tmapName = name,
                      tmapOffTile = tilesetGet OffTile tileset }

saveTerrainMap :: String -> TerrainMap -> IO ()
saveTerrainMap name tmap = do
  path <- getResourcePath "terrain" name
  writeFile path $ show (tmapSize tmap, fmap ttId $ elems $ tmapArray tmap)

-------------------------------------------------------------------------------
-- Positions:

positionRect :: Position -> IRect
positionRect (Point x y) =
  Rect (x * tileWidth) (y * tileHeight) tileWidth tileHeight

positionTopleft :: Position -> IPoint
positionTopleft (Point x y) = Point (x * tileWidth) (y * tileHeight)

positionCenter :: (Axis a) => Position -> Point a
positionCenter = rectCenter . fmap fromIntegral . positionRect

pointPosition :: IPoint -> Position
pointPosition (Point x y) = Point (x `div` tileWidth) (y `div` tileHeight)

prectRect :: PRect -> IRect
prectRect (Rect x y w h) =
  Rect (x * tileWidth) (y * tileHeight) (w * tileWidth) (h * tileHeight)

-------------------------------------------------------------------------------
-- Explored maps:

-- TODO: Which ends up being more efficient for our use case, a (UArray
-- Position Bool), or a (Set Position)?
newtype ExploredMap = ExploredMap (UArray Position Bool)

instance Show ExploredMap where
  showsPrec p (ExploredMap arr) = showsPrec p (bounds arr, string)
    where string = map fn $ elems arr
          fn b = if b then '1' else '0'

instance Read ExploredMap where
  readPrec = do
    (bound, string) <- Read.readPrec
    return $ ExploredMap $ listArray bound $ map ('1' ==) string

unexploredMap :: Terrain -> ExploredMap
unexploredMap terrain =
  ExploredMap $ listArray (bounds $ tmapArray $ terrainMap terrain) $
  repeat False

hasExplored :: ExploredMap -> Position -> Bool
hasExplored (ExploredMap arr) pos = inRange (bounds arr) pos && (arr ! pos)

setExplored :: [Position] -> ExploredMap -> ExploredMap
setExplored ps (ExploredMap arr) = ExploredMap $ arr // map (flip (,) True) ps

-------------------------------------------------------------------------------
