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

module Fallback.State.Tileset
  (TerrainTile(..), TileTag(..),
   Tileset, tilesetGet, tilesetLookup, tilesetArray, loadTileset)
where

import Control.Monad (foldM)
import Data.Array ((!), Array, Ix, listArray, range, rangeSize)
import qualified Data.Map as Map

import Fallback.Data.Clock (Clock, clockMod)
import Fallback.Data.Color (Color(Color), blackColor, whiteColor)
import Fallback.Data.TotalMap (TotalMap, makeTotalMapA, tmGet)
import Fallback.Draw (Sheet, Sprite)
import Fallback.State.Simple (TerrainOpenness(..))
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

data TerrainTile = TerrainTile
  { ttColor :: Color,
    ttId :: Int,
    ttOpenness :: TerrainOpenness,
    ttSprite :: Clock -> Sprite }

-------------------------------------------------------------------------------

data TileTag = OffTile | NullTile | StoneFloorTile
             -- Doors and gates:
             | AdobeDoorClosedTile | AdobeDoorOpenTile
             | AdobeGateClosedTile | AdobeGateOpenTile
             | BasaltDoorClosedTile | BasaltDoorOpenTile
             | BasaltGateClosedTile | BasaltGateOpenTile
             | StoneDoorClosedTile | StoneDoorOpenTile
             | StoneGateClosedTile | StoneGateOpenTile
             -- Other devices:
             | LeverLeftTile | LeverRightTile
  deriving (Bounded, Eq, Ix, Ord)

tileTagId :: TileTag -> Int
tileTagId OffTile = 0000
tileTagId NullTile = 0001
tileTagId StoneFloorTile = 8222
tileTagId AdobeDoorClosedTile = 3891
tileTagId AdobeDoorOpenTile = 2993
tileTagId AdobeGateClosedTile = 8625
tileTagId AdobeGateOpenTile = 0605
tileTagId BasaltDoorClosedTile = 6933
tileTagId BasaltDoorOpenTile = 6383
tileTagId BasaltGateClosedTile = 0865
tileTagId BasaltGateOpenTile = 7148
tileTagId StoneDoorClosedTile = 5588
tileTagId StoneDoorOpenTile = 0983
tileTagId StoneGateClosedTile = 2330
tileTagId StoneGateOpenTile = 5719
tileTagId LeverLeftTile = 7991
tileTagId LeverRightTile = 0761

-------------------------------------------------------------------------------

data Tileset = Tileset
  { tilesetArray :: Array Int TerrainTile,
    tilesetMap :: Map.Map Int TerrainTile,
    tilesetTotalMap :: TotalMap TileTag TerrainTile }

tilesetLookup :: Int -> Tileset -> Maybe TerrainTile
tilesetLookup tid tileset = Map.lookup tid $ tilesetMap tileset

tilesetGet :: TileTag -> Tileset -> TerrainTile
tilesetGet tag tileset = tmGet tag $ tilesetTotalMap tileset

-------------------------------------------------------------------------------

loadTileset :: Sheet -> IO Tileset
loadTileset terrainSheet = do
  let tilesList = map makeTile tileSpecs
  tilesMap <- flip3 foldM Map.empty tilesList $ \mp tile ->
    if Map.member (ttId tile) mp
    then fail $ "repeat tile ID: " ++ show (ttId tile)
    else return $ Map.insert (ttId tile) tile mp
  tilesTM <- makeTotalMapA $ \tag ->
    maybe (fail $ "no such tile ID: " ++ show (tileTagId tag)) return $
    Map.lookup (tileTagId tag) tilesMap
  let tilesArr = listArray (0, length tilesList - 1) tilesList
  return Tileset { tilesetArray = tilesArr, tilesetMap = tilesMap,
                   tilesetTotalMap = tilesTM }
  where
    makeTile spec =
      let fn = case tspecSprite spec of
                 Left loc -> const (terrainSheet ! loc)
                 Right (rang, slowdown) ->
                   let size = rangeSize rang
                       images = listArray (0, size - 1) $
                                map (terrainSheet !) $ range rang
                   in ((images :: Array Int Sprite) !) . clockMod size slowdown
      in TerrainTile { ttColor = tspecColor spec, ttId = tspecId spec,
                       ttOpenness = tspecOpenness spec, ttSprite = fn }

-------------------------------------------------------------------------------

data TileSpec = TileSpec
  { tspecId :: Int,
    tspecSprite :: Either (Int, Int) (((Int, Int), (Int, Int)), Int),
    tspecOpenness :: TerrainOpenness,
    tspecColor :: Color }

-- Tile IDs must be unique in this list.  The loadTileset function will verify
-- that this is so.
tileSpecs :: [TileSpec]
tileSpecs = [
 TileSpec 0000 (Left (0, 0)) TerrainSolid black, -- unexplored
 TileSpec 0001 (Left (0, 1)) TerrainSolid magenta, -- null tile

 TileSpec 8983 (Left (0, 4)) TerrainOpen green, -- grass
 TileSpec 2583 (Left (0, 5)) TerrainOpen green, -- grass w/ pebbles
 TileSpec 2938 (Left (0, 6)) TerrainOpen green, -- grass w/ shrubs
 TileSpec 8301 (Left (7, 9)) TerrainWindow gray, -- boulder
 TileSpec 8740 (Left (8, 7)) TerrainWindow gray, -- small rock pile
 TileSpec 0678 (Left (8, 8)) TerrainWindow gray, -- big rock pile left
 TileSpec 8415 (Left (8, 9)) TerrainWindow gray, -- big rock pile right
 TileSpec 1397 (Left (7, 11)) TerrainSolid darkgreen, -- big tree dark
 TileSpec 5398 (Left (20, 11)) TerrainSolid darkgreen, -- big tree light
 TileSpec 7100 (Left (20, 10)) TerrainSolid darkgreen, -- two trees
 TileSpec 5308 (Left (8, 2)) TerrainSolid darkgreen, -- dense trees
 TileSpec 7966 (Left (8, 3)) TerrainWindow green, -- sparse trees
 TileSpec 0723 (Left (8, 4)) TerrainWindow green, -- small tree
 TileSpec 8596 (Left (21, 4)) TerrainOpen green, -- six small bushes
 TileSpec 8799 (Left (22, 9)) TerrainWindow green, -- signpost

 TileSpec 2276 (Left (21, 7)) TerrainOpen gray, -- stone road
 TileSpec 8769 (Left (20, 0)) TerrainOpen gray,
 TileSpec 6164 (Left (20, 1)) TerrainOpen gray,
 TileSpec 6847 (Left (21, 0)) TerrainOpen gray,
 TileSpec 6145 (Left (21, 1)) TerrainOpen gray,

 TileSpec 1783 (Left (3, 6)) TerrainOpen darkgreen, -- dark/grass border
 TileSpec 8052 (Left (3, 7)) TerrainOpen darkgreen,
 TileSpec 6875 (Left (3, 8)) TerrainOpen darkgreen,
 TileSpec 2628 (Left (3, 9)) TerrainOpen darkgreen,
 TileSpec 1435 (Left (3, 10)) TerrainOpen darkgreen,
 TileSpec 3002 (Left (3, 11)) TerrainOpen darkgreen,
 TileSpec 7912 (Left (4, 2)) TerrainOpen darkgreen,
 TileSpec 3602 (Left (4, 3)) TerrainOpen darkgreen,
 TileSpec 7088 (Left (4, 4)) TerrainOpen darkgreen,
 TileSpec 3632 (Left (4, 5)) TerrainOpen darkgreen,
 TileSpec 7401 (Left (4, 6)) TerrainOpen darkgreen,
 TileSpec 8417 (Left (4, 7)) TerrainOpen darkgreen,

 TileSpec 3404 (Left (3, 4)) TerrainOpen darkgreen, -- dark grass
 TileSpec 1953 (Left (3, 5)) TerrainWindow gray, -- w/ rocks

 TileSpec 2851 (Left (1, 10)) TerrainSolid brown, -- grass wall
 TileSpec 5871 (Left (1, 11)) TerrainSolid brown,
 TileSpec 4461 (Left (2, 2)) TerrainSolid brown,
 TileSpec 8920 (Left (2, 3)) TerrainSolid brown,
 TileSpec 5255 (Left (2, 4)) TerrainSolid brown,
 TileSpec 3054 (Left (2, 5)) TerrainSolid brown,
 TileSpec 5504 (Left (2, 6)) TerrainSolid brown,
 TileSpec 1491 (Left (2, 7)) TerrainSolid brown,
 TileSpec 1825 (Left (2, 8)) TerrainSolid brown,
 TileSpec 6055 (Left (2, 9)) TerrainSolid brown,
 TileSpec 3302 (Left (2, 10)) TerrainSolid brown,
 TileSpec 3597 (Left (2, 11)) TerrainSolid brown,
 TileSpec 6745 (Left (3, 2)) TerrainSolid brown,
 TileSpec 3394 (Left (3, 3)) TerrainSolid brown,

 TileSpec 4181 (Left (4, 8)) TerrainHover blue, -- ocean
 TileSpec 7279 (Left (4, 9)) TerrainHover blue, -- shore
 TileSpec 1729 (Left (4, 10)) TerrainHover blue,
 TileSpec 3908 (Left (4, 11)) TerrainHover blue,
 TileSpec 1479 (Left (5, 2)) TerrainHover blue,
 TileSpec 6744 (Left (5, 3)) TerrainHover blue,
 TileSpec 8336 (Left (5, 4)) TerrainHover blue,
 TileSpec 4855 (Left (5, 5)) TerrainHover blue,
 TileSpec 9373 (Left (5, 6)) TerrainHover blue,
 TileSpec 5854 (Left (5, 7)) TerrainHover blue,
 TileSpec 1359 (Left (5, 8)) TerrainHover blue,
 TileSpec 5285 (Left (5, 9)) TerrainHover blue,
 TileSpec 6087 (Left (5, 10)) TerrainHover blue,
 TileSpec 2467 (Left (5, 11)) TerrainHover blue,
 TileSpec 9702 (Left (6, 2)) TerrainHover blue,

 TileSpec 3320 (Left (6, 3)) TerrainOpen gray, -- bridge
 TileSpec 7779 (Left (6, 4)) TerrainOpen gray,
 TileSpec 0387 (Left (6, 5)) TerrainOpen gray,
 TileSpec 3226 (Left (6, 6)) TerrainOpen gray,
 TileSpec 0153 (Left (6, 7)) TerrainOpen gray,
 TileSpec 7584 (Left (6, 8)) TerrainOpen gray,

 TileSpec 7292 (Left (8, 10)) TerrainSolid white, -- stone wall
 TileSpec 3112 (Left (8, 11)) TerrainSmoke white, -- stone secret door
 TileSpec 5588 (Left (9, 2)) TerrainSolid gray, -- stone closed door
 TileSpec 0983 (Left (9, 3)) TerrainOpen gray, -- stone open door
 TileSpec 2330 (Left (9, 4)) TerrainWindow gray, -- stone closed gate
 TileSpec 5719 (Left (9, 5)) TerrainOpen gray, -- stone open gate
 TileSpec 3254 (Left (9, 6)) TerrainSolid white, -- stone sign
 TileSpec 6250 (Left (9, 7)) TerrainSolid white, -- stone crack
 TileSpec 0111 (Left (9, 8)) TerrainSolid white, -- stone dirty
 TileSpec 7883 (Left (9, 9)) TerrainSolid white, -- stone decoration
 TileSpec 9398 (Left (9, 10)) TerrainSolid white, -- stone painting
 TileSpec 3037 (Left (9, 11)) TerrainWindow white, -- stone window

 TileSpec 7791 (Left (10, 2)) TerrainSolid white, -- basalt wall
 TileSpec 1306 (Left (10, 3)) TerrainSmoke white, -- basalt secret door
 TileSpec 6933 (Left (10, 4)) TerrainSolid gray, -- basalt closed door
 TileSpec 6383 (Left (10, 5)) TerrainOpen gray, -- basalt open door
 TileSpec 0865 (Left (10, 6)) TerrainWindow gray, -- basalt closed gate
 TileSpec 7148 (Left (10, 7)) TerrainOpen gray, -- basalt open gate
 TileSpec 9011 (Left (10, 8)) TerrainSolid white, -- basalt sign
 TileSpec 6051 (Left (10, 9)) TerrainSolid white, -- basalt crack
 TileSpec 6455 (Left (10, 10)) TerrainSolid white, -- basalt dirty
 TileSpec 0170 (Left (10, 11)) TerrainWindow white, -- basalt window

 TileSpec 1752 (Left (11, 2)) TerrainSolid white, -- adobe wall
 TileSpec 5489 (Left (11, 3)) TerrainSmoke white, -- adobe secret door
 TileSpec 3891 (Left (11, 4)) TerrainSolid gray, -- adobe closed door
 TileSpec 2993 (Left (11, 5)) TerrainOpen gray, -- adobe open door
 TileSpec 8625 (Left (11, 6)) TerrainWindow gray, -- adobe closed gate
 TileSpec 0605 (Left (11, 7)) TerrainOpen gray, -- adobe open gate
 TileSpec 0364 (Left (11, 8)) TerrainSolid white, -- adobe sign
 TileSpec 7185 (Left (11, 9)) TerrainSolid white, -- adobe crack
 TileSpec 1814 (Left (11, 10)) TerrainSolid white, -- adobe dirty
 TileSpec 3403 (Left (11, 11)) TerrainSolid white, -- adobe decoration
 TileSpec 5216 (Left (12, 2)) TerrainWindow white, -- adobe window

 TileSpec 8559 (Left (13, 0)) TerrainOpen darkgray, -- stairs up
 TileSpec 5724 (Left (13, 1)) TerrainOpen darkgray, -- stairs up
 TileSpec 9605 (Left (14, 0)) TerrainOpen darkgray, -- stairs down
 TileSpec 4839 (Left (14, 1)) TerrainOpen darkgray, -- stairs down
 TileSpec 0832 (Left (15, 0)) TerrainHover black, -- pit

 TileSpec 8222 (Left (12, 5)) TerrainOpen gray, -- stone floor
 TileSpec 0957 (Left (6, 0)) TerrainOpen gray,
 TileSpec 9622 (Left (6, 1)) TerrainOpen gray,
 TileSpec 2040 (Left (7, 0)) TerrainOpen gray,
 TileSpec 6842 (Left (7, 1)) TerrainOpen gray,
 TileSpec 6296 (Left (8, 0)) TerrainOpen gray,
 TileSpec 7558 (Left (8, 1)) TerrainOpen gray,
 TileSpec 5948 (Left (9, 0)) TerrainOpen gray,
 TileSpec 8510 (Left (9, 1)) TerrainOpen gray,
 TileSpec 4219 (Left (10, 0)) TerrainOpen gray,
 TileSpec 2097 (Left (10, 1)) TerrainOpen gray,
 TileSpec 8859 (Left (11, 0)) TerrainOpen gray,
 TileSpec 2411 (Left (11, 1)) TerrainOpen gray,

 TileSpec 1602 (Right (((47, 8), (47, 11)), 4)) TerrainOpen gray, -- torch
 TileSpec 6808 (Right (((48, 8), (48, 11)), 4)) TerrainOpen gray, -- torch
 TileSpec 6445 (Left (12, 6)) TerrainOpen gray, -- rune
 TileSpec 2349 (Left (12, 7)) TerrainWindow gray, -- pedestal
 TileSpec 4183 (Left (12, 8)) TerrainWindow gray, -- desk
 TileSpec 7723 (Left (13, 2)) TerrainWindow gray, -- book on pedestal
 TileSpec 4645 (Left (13, 3)) TerrainWindow gray, -- column
 TileSpec 9307 (Left (13, 4)) TerrainWindow gray, -- potted plant
 TileSpec 7551 (Left (13, 5)) TerrainWindow gray, -- statue
 TileSpec 2983 (Left (13, 7)) TerrainSolid brown, -- bookcase
 TileSpec 7576 (Left (25, 4)) TerrainWindow gray, -- shelving
 TileSpec 9720 (Left (12, 9)) TerrainWindow gray, -- table (east end)
 TileSpec 9689 (Left (12, 10)) TerrainWindow gray, -- table (east/west)
 TileSpec 8593 (Left (12, 11)) TerrainWindow gray, -- table (west end)
 TileSpec 8590 (Left (15, 2)) TerrainWindow gray, -- table (north end)
 TileSpec 9381 (Left (15, 3)) TerrainWindow gray, -- table (north/south)
 TileSpec 4094 (Left (15, 4)) TerrainWindow gray, -- table (south end)
 TileSpec 2367 (Left (13, 10)) TerrainWindow gray, -- chest
 TileSpec 9475 (Left (13, 11)) TerrainOpen gray, -- chair
 TileSpec 2060 (Left (14, 2)) TerrainOpen gray, -- chair
 TileSpec 2118 (Left (14, 3)) TerrainOpen gray, -- chair
 TileSpec 5736 (Left (14, 4)) TerrainOpen gray, -- chair
 TileSpec 0422 (Left (14, 5)) TerrainWindow gray, -- bed
 TileSpec 4530 (Left (23, 2)) TerrainWindow gray, -- bed w/ person
 TileSpec 8329 (Left (14, 6)) TerrainWindow gray, -- throne
 TileSpec 1231 (Left (14, 7)) TerrainWindow gray, -- dresser
 TileSpec 9028 (Left (15, 5)) TerrainWindow gray, -- cauldron
 TileSpec 1969 (Left (15, 7)) TerrainOpen gray, -- rug
 TileSpec 7555 (Left (15, 8)) TerrainWindow gray, -- anvil
 TileSpec 0761 (Left (21, 5)) TerrainOpen gray, -- lever right
 TileSpec 7991 (Left (21, 6)) TerrainOpen gray, -- lever left
 TileSpec 4682 (Left (44, 11)) TerrainWindow gray, -- wheel
 TileSpec 3813 (Left (12, 1)) TerrainOpen gray, -- stone floor w/ snow

 TileSpec 0040 (Left (15, 9)) TerrainOpen gray, -- white tile floor
 TileSpec 6711 (Left (16, 4)) TerrainWindow gray, -- column

 TileSpec 4252 (Left (16, 5)) TerrainOpen green, -- green tile floor
 TileSpec 5404 (Left (16, 8)) TerrainOpen green, -- rug
 TileSpec 9056 (Left (16, 9)) TerrainWindow cyan, -- small ice crystal
 TileSpec 9349 (Left (24, 8)) TerrainWindow cyan, -- ice column
 TileSpec 4357 (Left (21, 11)) TerrainSolid cyan, -- large ice crystal
 TileSpec 7021 (Left (17, 2)) TerrainOpen green, -- rune
 TileSpec 5215 (Left (17, 3)) TerrainWindow green, -- throne
 TileSpec 3038 (Left (17, 9)) TerrainHover green, -- fire pit
 TileSpec 5664 (Left (24, 9)) TerrainOpen green, -- pentagram top-left
 TileSpec 7118 (Left (24, 10)) TerrainOpen green, -- pentagram top-right
 TileSpec 7760 (Left (24, 11)) TerrainOpen green, -- pentagram bottom-left
 TileSpec 1433 (Left (25, 2)) TerrainOpen green, -- pentagram bottom-right

 TileSpec 5709 (Left (0, 2)) TerrainOpen lightgray, -- snow
 TileSpec 3930 (Left (0, 3)) TerrainOpen lightgray, -- snow w/ three mushrooms
 TileSpec 7591 (Left (7, 4)) TerrainOpen lightgray, -- snow w/ five mushrooms
 TileSpec 6995 (Left (7, 3)) TerrainWindow lightgray, -- dead shrubs
 TileSpec 2571 (Left (7, 2)) TerrainWindow lightgray, -- dead tree
 TileSpec 1332 (Left (1, 0)) TerrainSolid brown, -- big dead tree
 TileSpec 7122 (Left (1, 1)) TerrainWindow green, -- three evergreens
 TileSpec 0781 (Left (2, 0)) TerrainSolid darkgreen, -- dense evergreens
 TileSpec 3384 (Left (6, 9)) TerrainWindow gray, -- small rocks
 TileSpec 3236 (Left (6, 10)) TerrainWindow gray, -- big rocks left
 TileSpec 2011 (Left (22, 4)) TerrainWindow gray, -- big rocks center
 TileSpec 8721 (Left (6, 11)) TerrainWindow gray, -- big rocks right
 TileSpec 5390 (Left (24, 7)) TerrainSolid cyan, -- snow w/ ice wall
 TileSpec 9409 (Left (18, 2)) TerrainOpen lightgray, -- green trash
 TileSpec 9456 (Left (18, 4)) TerrainOpen lightgray, -- red trash
 TileSpec 1287 (Left (18, 5)) TerrainWindow lightgray, -- signpost
 TileSpec 4556 (Left (18, 6)) TerrainWindow lightgray, -- obelisk
 TileSpec 0563 (Right (((46, 8), (46, 11)), 4)) TerrainWindow orange, -- cmpfir
 TileSpec 5643 (Left (23, 7)) TerrainWindow lightgray, -- stalagmites

 TileSpec 7032 (Left (22, 0)) TerrainOpen gray, -- snowy road
 TileSpec 3714 (Left (22, 1)) TerrainOpen gray,
 TileSpec 6386 (Left (23, 0)) TerrainOpen gray,
 TileSpec 3082 (Left (23, 1)) TerrainOpen gray,

 TileSpec 5203 (Left (0, 7)) TerrainSolid purple, -- snow wall
 TileSpec 1455 (Left (0, 8)) TerrainSolid purple,
 TileSpec 6668 (Left (0, 9)) TerrainSolid purple,
 TileSpec 6722 (Left (0, 10)) TerrainSolid purple,
 TileSpec 0245 (Left (0, 11)) TerrainSolid purple,
 TileSpec 2645 (Left (1, 2)) TerrainSolid purple,
 TileSpec 0059 (Left (1, 3)) TerrainSolid purple,
 TileSpec 8854 (Left (1, 4)) TerrainSolid purple,
 TileSpec 0941 (Left (1, 5)) TerrainSolid purple,
 TileSpec 4238 (Left (1, 6)) TerrainSolid purple,
 TileSpec 1331 (Left (1, 7)) TerrainSolid purple,
 TileSpec 7167 (Left (1, 8)) TerrainSolid purple,
 TileSpec 7043 (Left (1, 9)) TerrainSolid purple,

 TileSpec 3425 (Left (33, 4)) TerrainSolid purple, -- snow/cave wall
 TileSpec 1968 (Left (33, 5)) TerrainSolid purple,
 TileSpec 4167 (Left (33, 6)) TerrainSolid purple,
 TileSpec 3630 (Left (33, 7)) TerrainSolid purple,
 TileSpec 8963 (Left (33, 8)) TerrainSolid purple,
 TileSpec 6115 (Left (33, 9)) TerrainSolid purple,
 TileSpec 3402 (Left (33, 10)) TerrainSolid purple,
 TileSpec 1279 (Left (33, 11)) TerrainSolid purple,

 TileSpec 5679 (Left (32, 2)) TerrainOpen lightgray, -- snow/cave floor
 TileSpec 9193 (Left (32, 3)) TerrainOpen lightgray,
 TileSpec 1548 (Left (32, 4)) TerrainOpen lightgray,
 TileSpec 1210 (Left (32, 5)) TerrainOpen lightgray,
 TileSpec 7007 (Left (32, 6)) TerrainOpen lightgray,
 TileSpec 5586 (Left (32, 7)) TerrainOpen lightgray,
 TileSpec 6754 (Left (32, 8)) TerrainOpen lightgray,
 TileSpec 2158 (Left (32, 9)) TerrainOpen lightgray,
 TileSpec 4973 (Left (32, 10)) TerrainOpen lightgray,
 TileSpec 6025 (Left (32, 11)) TerrainOpen lightgray,
 TileSpec 7303 (Left (33, 2)) TerrainOpen lightgray,
 TileSpec 5329 (Left (33, 3)) TerrainOpen lightgray,
 TileSpec 7042 (Left (34, 9)) TerrainOpen lightgray, -- stalagmites

 TileSpec 1171 (Left (26, 8)) TerrainOpen bluegreen, -- cave floor
 TileSpec 6498 (Left (26, 9)) TerrainOpen bluegreen, -- w/ shrooms
 TileSpec 8959 (Left (28, 11)) TerrainOpen bluegreen, -- w/ shrooms
 TileSpec 4581 (Left (28, 10)) TerrainOpen bluegreen, -- w/ shrooms
 TileSpec 9760 (Left (31, 2)) TerrainOpen bluegreen, -- w/ shrooms
 TileSpec 1376 (Left (28, 5)) TerrainWindow gray, -- small rocks
 TileSpec 0772 (Left (28, 6)) TerrainWindow gray, -- big rocks left
 TileSpec 0179 (Left (29, 2)) TerrainWindow gray, -- big rocks center
 TileSpec 6341 (Left (28, 7)) TerrainWindow gray, -- big rocks right
 TileSpec 5892 (Left (29, 3)) TerrainHover blue, -- small pool
 TileSpec 6109 (Left (29, 5)) TerrainWindow bluegreen, -- stalagmites
 TileSpec 6914 (Left (29, 6)) TerrainWindow bluegreen, -- stalagmites
 TileSpec 7234 (Left (30, 10)) TerrainWindow bluegreen, -- stalagmites
 TileSpec 5653 (Left (30, 11)) TerrainWindow bluegreen, -- stalagmites
 TileSpec 5073 (Left (29, 9)) TerrainOpen bluegreen, -- green trash
 TileSpec 6814 (Left (29, 11)) TerrainOpen bluegreen, -- red trash
 TileSpec 3086 (Left (29, 7)) TerrainWindow bluegreen, -- chest
 TileSpec 6852 (Left (30, 2)) TerrainWindow bluegreen, -- sign
 TileSpec 0545 (Left (30, 3)) TerrainWindow bluegreen, -- obelisk
 TileSpec 5306 (Left (30, 4)) TerrainWindow bluegreen, -- totems
 TileSpec 4196 (Left (30, 5)) TerrainWindow bluegreen, -- statue

 TileSpec 3431 (Left (28, 1)) TerrainOpen gray, -- mine cart tracks
 TileSpec 4408 (Left (29, 0)) TerrainOpen gray,
 TileSpec 3899 (Left (29, 1)) TerrainOpen gray,
 TileSpec 0486 (Left (30, 0)) TerrainOpen gray,
 TileSpec 2317 (Left (30, 1)) TerrainOpen gray,
 TileSpec 9224 (Left (31, 0)) TerrainOpen gray,
 TileSpec 3915 (Left (31, 1)) TerrainOpen gray,
 TileSpec 8591 (Left (32, 0)) TerrainOpen gray,
 TileSpec 6079 (Left (32, 1)) TerrainOpen gray,
 TileSpec 9108 (Left (33, 0)) TerrainOpen gray,
 TileSpec 3895 (Left (33, 1)) TerrainOpen gray,

 TileSpec 1422 (Left (26, 10)) TerrainSolid purple, -- cave wall
 TileSpec 8648 (Left (26, 11)) TerrainSolid purple,
 TileSpec 7655 (Left (27, 2)) TerrainSolid purple,
 TileSpec 7069 (Left (27, 3)) TerrainSolid purple,
 TileSpec 9022 (Left (27, 4)) TerrainSolid purple,
 TileSpec 9090 (Left (27, 5)) TerrainSolid purple,
 TileSpec 2636 (Left (27, 6)) TerrainSolid purple,
 TileSpec 8111 (Left (27, 7)) TerrainSolid purple,
 TileSpec 5652 (Left (27, 8)) TerrainSolid purple,
 TileSpec 2680 (Left (27, 9)) TerrainSolid purple,
 TileSpec 9166 (Left (27, 10)) TerrainSolid purple,
 TileSpec 5750 (Left (27, 11)) TerrainSolid purple,
 TileSpec 1212 (Left (28, 2)) TerrainSolid purple,
 TileSpec 4444 (Left (27, 0)) TerrainSmoke purple, -- cave passwall
 TileSpec 5916 (Left (27, 1)) TerrainSmoke purple, -- cave passwall

 TileSpec 2937 (Right (((45, 0), (45, 3)), 15)) TerrainHover blue, -- water
 TileSpec 7629 (Right (((46, 0), (46, 3)), 15)) TerrainOpen gray, -- bridge
 TileSpec 1917 (Right (((47, 0), (47, 3)), 15)) TerrainOpen gray, -- bridge
 TileSpec 5658 (Right (((48, 0), (48, 3)), 15)) TerrainWindow blue, -- wtr rock
 TileSpec 4863 (Right (((45, 4), (45, 7)), 15)) TerrainWindow blue, -- wtrfall

 TileSpec 0285 (Right (((49, 0), (49, 3)), 6)) TerrainHover orange] -- lava
  where
    black = blackColor
    blue = Color 0 0 255
    bluegreen = Color 0 192 128
    brown = Color 128 64 0
    cyan = Color 0 255 255
    darkgray = Color 64 64 64
    darkgreen = Color 0 96 0
    gray = Color 128 128 128
    green = Color 0 128 0
    lightgray = Color 192 192 192
    magenta = Color 255 0 255
    orange = Color 255 128 0
    purple = Color 64 16 32
    white = whiteColor

-- 7264, 7108, 5376, 1701, 3235, 6921, 4701, 9878, 6996, 2443, 6760, 0295

-------------------------------------------------------------------------------
