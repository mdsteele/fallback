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
   MarkKey, tmapAllMarks, tmapLookupMark, tmapGetMarks, tmapSetMarks,
   RectKey, tmapAllRects, tmapLookupRect, tmapSetRect,
   loadTerrainMap, saveTerrainMap,
   -- * Positions
   positionRect, positionTopleft, positionCenter, pointPosition, prectRect,
   -- * Explored maps
   ExploredMap, unexploredMap, hasExplored, setExplored)
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Traversable as Trav (mapM)
import qualified Text.Read as Read (readPrec)

import Fallback.Constants (tileHeight, tileWidth)
import Fallback.Control.Error
import Fallback.Control.Parse
import qualified Fallback.Data.Multimap as MM
import Fallback.Data.Point
import Fallback.Resource (getResourcePath, parseFromFile, saveToFile)
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
    tmapMarks :: MM.Multimap MarkKey Position,
    tmapName :: String,
    tmapOffTile :: TerrainTile,
    tmapRects :: Map.Map RectKey PRect }

makeEmptyTerrainMap :: (Int, Int) -> TerrainTile -> TerrainTile -> TerrainMap
makeEmptyTerrainMap (w, h) offTile nullTile = TerrainMap
  { tmapArray = listArray (Point 0 0, Point (w - 1) (h - 1)) (repeat nullTile),
    tmapMarks = MM.empty, tmapName = "", tmapOffTile = offTile,
    tmapRects = Map.empty }

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
tmapShift nullTile delta tmap =
  tmap { tmapArray = arr',
         tmapMarks = MM.map (`pAdd` delta) (tmapMarks tmap),
         tmapRects = Map.map (`rectPlus` delta) (tmapRects tmap) }
  where
    arr' = listArray bound (repeat nullTile) //
           (filter (inRange bound . fst) $ map shift $ assocs $ tmapArray tmap)
    shift (point, tile) = (point `pAdd` delta, tile)
    bound = bounds (tmapArray tmap)

-------------------------------------------------------------------------------

type MarkKey = String

tmapAllMarks :: TerrainMap -> [(MarkKey, Position)]
tmapAllMarks = MM.toList . tmapMarks

tmapLookupMark :: MarkKey -> TerrainMap -> Set.Set Position
tmapLookupMark key = MM.lookup key . tmapMarks

tmapGetMarks :: Position -> TerrainMap -> Set.Set MarkKey
tmapGetMarks pos tmap = MM.reverseLookup pos $ tmapMarks tmap

tmapSetMarks :: Position -> Set.Set MarkKey -> TerrainMap -> TerrainMap
tmapSetMarks pos keys tmap =
  tmap { tmapMarks = MM.reverseSet pos keys $ tmapMarks tmap }

type RectKey = String

tmapAllRects :: TerrainMap -> [(RectKey, PRect)]
tmapAllRects = Map.toList . tmapRects

tmapLookupRect :: RectKey -> TerrainMap -> Maybe PRect
tmapLookupRect key = Map.lookup key . tmapRects

tmapSetRect :: RectKey -> Maybe PRect -> TerrainMap -> TerrainMap
tmapSetRect key mbRect tmap =
  tmap { tmapRects = (maybe (Map.delete key) (Map.insert key) mbRect) $
                     tmapRects tmap }

-------------------------------------------------------------------------------

loadTerrainMap :: Resources -> String -> IOEO TerrainMap
loadTerrainMap resources name = do
  let tileset = rsrcTileset resources
  path <- onlyIO $ getResourcePath "terrain" name
  let getTile tid = maybe (fail $ "Bad tile id: " ++ show tid) return $
                    tilesetLookup tid tileset
  (tileArray, marks, rects) <- do
    mbTerrainData <- onlyIO $ parseFromFile path parseTerrainData
    ((w, h), ids, marks, rects) <-
      maybe (fail $ "Couldn't read terrain from " ++ name)
            return mbTerrainData
    when (length ids /= w * h) $ fail ("Terrain size mismatch for" ++ name)
    tiles <- onlyEO $ Trav.mapM getTile $
             listArray (Point 0 0, Point (w - 1) (h - 1)) ids
    return (tiles, marks, rects)
  return TerrainMap { tmapArray = tileArray,
                      tmapMarks = MM.fromList marks,
                      tmapName = name,
                      tmapOffTile = tilesetGet OffTile tileset,
                      tmapRects = Map.fromList rects }

parseTerrainData :: Parser ((Int, Int), [Int], [(String, Position)],
                            [(String, PRect)])
parseTerrainData = weaveBracesCommas $ (,,,) <$>
  meshKeyVal "size" <*>
  meshKeyVal "tiles" <*>
  meshKeyDefault "marks" [] <*>
  meshKeyDefault "rects" []

saveTerrainMap :: String -> TerrainMap -> IOEO ()
saveTerrainMap name tmap = do
  path <- onlyIO $ getResourcePath "terrain" name
  saveToFile path $ showBracesCommas [
    (showKeyVal "size" $ tmapSize tmap),
    (showKeyList "tiles" 15 $ map ttId $ elems $ tmapArray tmap),
    (showKeyList "marks" 1 $ MM.toList $ tmapMarks tmap),
    (showKeyList "rects" 1 $ Map.toList $ tmapRects tmap)]

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
-- Position Bool), or a QuadTree (or a (Set Position))?
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
{-
newtype ExploredMap = ExploredMap QT.QuadTree

instance Show ExploredMap where
  showsPrec p (ExploredMap qt) = showsPrec p qt

instance Read ExploredMap where
  readPrec = fmap ExploredMap Read.readPrec

unexploredMap :: Terrain -> ExploredMap
unexploredMap _ = ExploredMap QT.empty

hasExplored :: ExploredMap -> Position -> Bool
hasExplored (ExploredMap qt) pos = QT.member pos qt

setExplored :: [Position] -> ExploredMap -> ExploredMap
setExplored ps (ExploredMap qt) = ExploredMap $ foldr QT.insert qt ps
-}
-------------------------------------------------------------------------------
