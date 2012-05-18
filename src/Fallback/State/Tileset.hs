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
  (TerrainTile(..), TileAppearance(..), TileOverlay(..), TileTag(..),
   Tileset, tilesetGet, tilesetLookup, tilesetArray, loadTileset)
where

import Control.Monad (foldM)
import Data.Array (Array, Ix, listArray)
import qualified Data.Map as Map

import Fallback.Data.Color (Color(Color), blackColor, whiteColor)
import Fallback.Data.TotalMap (TotalMap, makeTotalMapA, tmGet)
import Fallback.State.Simple (TerrainOpenness(..))
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

data TerrainTile = Tile
  { ttId :: Int,
    ttAppearance :: TileAppearance,
    ttOpenness :: TerrainOpenness,
    ttColor :: Color }

data TileAppearance = Still !Int !Int
                    | Anim !Int !Int !Int !TileOverlay

data TileOverlay = NoOverlay | Overlay !Int !Int

-------------------------------------------------------------------------------

data TileTag = OffTile | NullTile
             | StoneFloorTile | WhiteTileFloorTile | WaterAnimTile
             | LavaAnimTile | AdobeCrackedWallTile
             -- Doors and gates:
             | AdobeDoorClosedTile | AdobeDoorOpenTile
             | AdobeGateClosedTile | AdobeGateOpenTile
             | BasaltDoorClosedTile | BasaltDoorOpenTile
             | BasaltGateClosedTile | BasaltGateOpenTile
             | StoneDoorClosedTile | StoneDoorOpenTile
             | StoneGateClosedTile | StoneGateOpenTile
             -- Mine cart:
             | MineCartEmptyHorzTile | MineCartEmptyVertTile
             | MineCartFullHorzTile | MineCartFullVertTile
             -- Other devices:
             | LeverLeftTile | LeverRightTile
  deriving (Bounded, Eq, Ix, Ord)

tileTagId :: TileTag -> Int
tileTagId OffTile = 0000
tileTagId NullTile = 0001
tileTagId StoneFloorTile = 8222
tileTagId WhiteTileFloorTile = 0040
tileTagId WaterAnimTile = 2937
tileTagId LavaAnimTile = 0285
tileTagId AdobeCrackedWallTile = 7185
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
tileTagId MineCartEmptyHorzTile = 8300
tileTagId MineCartEmptyVertTile = 5199
tileTagId MineCartFullHorzTile = 3187
tileTagId MineCartFullVertTile = 3525
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

loadTileset :: IO Tileset
loadTileset = do
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

-------------------------------------------------------------------------------

-- Tile IDs must be unique in this list.  The loadTileset function will verify
-- that this is so.
tilesList :: [TerrainTile]
tilesList = [
 Tile 0000 (Still 0 0) TerrainSolid black, -- unexplored
 Tile 0001 (Still 0 1) TerrainSolid magenta, -- null tile

 Tile 8983 (Still 0 4) TerrainOpen green, -- grass
 Tile 2583 (Still 0 5) TerrainOpen green, -- grass w/ pebbles
 Tile 2938 (Still 0 6) TerrainOpen green, -- grass w/ shrubs
 Tile 8301 (Still 7 9) TerrainWindow gray, -- boulder
 Tile 8740 (Still 8 7) TerrainWindow gray, -- small rock pile
 Tile 0678 (Still 8 8) TerrainWindow gray, -- big rock pile left
 Tile 8415 (Still 8 9) TerrainWindow gray, -- big rock pile right
 Tile 1397 (Still 7 11) TerrainSolid darkgreen, -- big tree dark
 Tile 5398 (Still 20 11) TerrainSolid darkgreen, -- big tree light
 Tile 7100 (Still 20 10) TerrainSolid darkgreen, -- two trees
 Tile 5308 (Still 8 2) TerrainSolid darkgreen, -- dense trees
 Tile 7966 (Still 8 3) TerrainWindow green, -- sparse trees
 Tile 0723 (Still 8 4) TerrainWindow green, -- small tree
 Tile 8596 (Still 21 4) TerrainOpen green, -- six small bushes
 Tile 8799 (Still 22 9) TerrainWindow green, -- signpost

 Tile 2276 (Still 21 7) TerrainOpen gray, -- stone road
 Tile 8769 (Still 20 0) TerrainOpen gray,
 Tile 6164 (Still 20 1) TerrainOpen gray,
 Tile 6847 (Still 21 0) TerrainOpen gray,
 Tile 6145 (Still 21 1) TerrainOpen gray,

 Tile 1783 (Still 3 6) TerrainOpen darkgreen, -- dark/grass border
 Tile 8052 (Still 3 7) TerrainOpen darkgreen,
 Tile 6875 (Still 3 8) TerrainOpen darkgreen,
 Tile 2628 (Still 3 9) TerrainOpen darkgreen,
 Tile 1435 (Still 3 10) TerrainOpen darkgreen,
 Tile 3002 (Still 3 11) TerrainOpen darkgreen,
 Tile 7912 (Still 4 2) TerrainOpen darkgreen,
 Tile 3602 (Still 4 3) TerrainOpen darkgreen,
 Tile 7088 (Still 4 4) TerrainOpen darkgreen,
 Tile 3632 (Still 4 5) TerrainOpen darkgreen,
 Tile 7401 (Still 4 6) TerrainOpen darkgreen,
 Tile 8417 (Still 4 7) TerrainOpen darkgreen,

 Tile 3404 (Still 3 4) TerrainOpen darkgreen, -- dark grass
 Tile 1953 (Still 3 5) TerrainWindow gray, -- w/ rocks
 Tile 8040 (Still 37 7) TerrainWindow gray, -- w/ boulder

 Tile 2851 (Still 1 10) TerrainSolid brown, -- grass wall
 Tile 4461 (Still 2 2) TerrainSolid brown,
 Tile 8920 (Still 2 3) TerrainSolid brown,
 Tile 5255 (Still 2 4) TerrainSolid brown,
 Tile 3054 (Still 2 5) TerrainSolid brown,
 Tile 5504 (Still 2 6) TerrainSolid brown,
 Tile 1491 (Still 2 7) TerrainSolid brown,
 Tile 1825 (Still 2 8) TerrainSolid brown,
 Tile 6055 (Still 2 9) TerrainSolid brown,
 Tile 3302 (Still 2 10) TerrainSolid brown,
 Tile 3597 (Still 2 11) TerrainSolid brown,
 Tile 6745 (Still 3 2) TerrainSolid brown,
 Tile 3394 (Still 3 3) TerrainSolid brown,

 Tile 4181 (Still 4 8) TerrainHover blue, -- ocean
 Tile 7279 (Still 4 9) TerrainHover blue, -- shore
 Tile 1729 (Still 4 10) TerrainHover blue,
 Tile 3908 (Still 4 11) TerrainHover blue,
 Tile 1479 (Still 5 2) TerrainHover blue,
 Tile 6744 (Still 5 3) TerrainHover blue,
 Tile 8336 (Still 5 4) TerrainHover blue,
 Tile 4855 (Still 5 5) TerrainHover blue,
 Tile 9373 (Still 5 6) TerrainHover blue,
 Tile 5854 (Still 5 7) TerrainHover blue,
 Tile 1359 (Still 5 8) TerrainHover blue,
 Tile 5285 (Still 5 9) TerrainHover blue,
 Tile 6087 (Still 5 10) TerrainHover blue,
 Tile 2467 (Still 5 11) TerrainHover blue,
 Tile 9702 (Still 6 2) TerrainHover blue,

 Tile 3320 (Still 6 3) TerrainOpen gray, -- bridge
 Tile 7779 (Still 6 4) TerrainOpen gray,
 Tile 0387 (Still 6 5) TerrainOpen gray,
 Tile 3226 (Still 6 6) TerrainOpen gray,
 Tile 0153 (Still 6 7) TerrainOpen gray,
 Tile 7584 (Still 6 8) TerrainOpen gray,

 Tile 7292 (Still 8 10) TerrainSolid white, -- stone wall
 Tile 3112 (Still 8 11) TerrainSmoke white, -- stone secret door
 Tile 5588 (Still 9 2) TerrainSolid gray, -- stone closed door
 Tile 0983 (Still 9 3) TerrainOpen gray, -- stone open door
 Tile 2330 (Still 9 4) TerrainWindow gray, -- stone closed gate
 Tile 5719 (Still 9 5) TerrainOpen gray, -- stone open gate
 Tile 3254 (Still 9 6) TerrainSolid white, -- stone sign
 Tile 6250 (Still 9 7) TerrainSolid white, -- stone crack
 Tile 0111 (Still 9 8) TerrainSolid white, -- stone dirty
 Tile 7883 (Still 9 9) TerrainSolid white, -- stone decoration
 Tile 9398 (Still 9 10) TerrainSolid white, -- stone painting
 Tile 3037 (Still 9 11) TerrainWindow white, -- stone window

 Tile 7791 (Still 10 2) TerrainSolid white, -- basalt wall
 Tile 1306 (Still 10 3) TerrainSmoke white, -- basalt secret door
 Tile 6933 (Still 10 4) TerrainSolid gray, -- basalt closed door
 Tile 6383 (Still 10 5) TerrainOpen gray, -- basalt open door
 Tile 0865 (Still 10 6) TerrainWindow gray, -- basalt closed gate
 Tile 7148 (Still 10 7) TerrainOpen gray, -- basalt open gate
 Tile 9011 (Still 10 8) TerrainSolid white, -- basalt sign
 Tile 6051 (Still 10 9) TerrainSolid white, -- basalt crack
 Tile 6455 (Still 10 10) TerrainSolid white, -- basalt dirty
 Tile 0170 (Still 10 11) TerrainWindow white, -- basalt window

 Tile 1752 (Still 11 2) TerrainSolid white, -- adobe wall
 Tile 5489 (Still 11 3) TerrainSmoke white, -- adobe secret door
 Tile 3891 (Still 11 4) TerrainSolid gray, -- adobe closed door
 Tile 2993 (Still 11 5) TerrainOpen gray, -- adobe open door
 Tile 8625 (Still 11 6) TerrainWindow gray, -- adobe closed gate
 Tile 0605 (Still 11 7) TerrainOpen gray, -- adobe open gate
 Tile 0364 (Still 11 8) TerrainSolid white, -- adobe sign
 Tile 7185 (Still 11 9) TerrainSolid white, -- adobe crack
 Tile 1814 (Still 11 10) TerrainSolid white, -- adobe dirty
 Tile 3403 (Still 11 11) TerrainSolid white, -- adobe decoration
 Tile 5216 (Still 12 2) TerrainWindow white, -- adobe window

 Tile 8559 (Still 13 0) TerrainOpen darkgray, -- stairs up
 Tile 5724 (Still 13 1) TerrainOpen darkgray, -- stairs up
 Tile 9605 (Still 14 0) TerrainOpen darkgray, -- stairs down
 Tile 4839 (Still 14 1) TerrainOpen darkgray, -- stairs down
 Tile 0832 (Still 15 0) TerrainHover black, -- pit

 Tile 8222 (Still 12 5) TerrainOpen gray, -- stone floor
 Tile 0957 (Still 6 0) TerrainOpen gray,
 Tile 9622 (Still 6 1) TerrainOpen gray,
 Tile 2040 (Still 7 0) TerrainOpen gray,
 Tile 6842 (Still 7 1) TerrainOpen gray,
 Tile 6296 (Still 8 0) TerrainOpen gray,
 Tile 7558 (Still 8 1) TerrainOpen gray,
 Tile 5948 (Still 9 0) TerrainOpen gray,
 Tile 8510 (Still 9 1) TerrainOpen gray,
 Tile 4219 (Still 10 0) TerrainOpen gray,
 Tile 2097 (Still 10 1) TerrainOpen gray,
 Tile 8859 (Still 11 0) TerrainOpen gray,
 Tile 2411 (Still 11 1) TerrainOpen gray,

 Tile 1602 (Anim 47 8 4 NoOverlay) TerrainOpen gray, -- torch
 Tile 6808 (Anim 48 8 4 NoOverlay) TerrainOpen gray, -- torch
 Tile 6445 (Still 12 6) TerrainOpen gray, -- green rune
 Tile 7894 (Still 25 5) TerrainOpen gray, -- bubble rune
 Tile 2349 (Still 12 7) TerrainWindow gray, -- pedestal
 Tile 4183 (Still 12 8) TerrainWindow gray, -- desk
 Tile 7723 (Still 13 2) TerrainWindow gray, -- book on pedestal
 Tile 4645 (Still 13 3) TerrainWindow gray, -- column
 Tile 4892 (Still 12 0) TerrainSolid lightgray, -- huge column
 Tile 9307 (Still 13 4) TerrainWindow gray, -- potted plant
 Tile 7398 (Still 13 8) TerrainWindow gray, -- brazier
 Tile 7551 (Still 13 5) TerrainWindow gray, -- statue
 Tile 2983 (Still 13 7) TerrainSolid brown, -- bookcase
 Tile 7576 (Still 25 4) TerrainWindow gray, -- shelving
 Tile 9720 (Still 12 9) TerrainWindow gray, -- counter (east end)
 Tile 9689 (Still 12 10) TerrainWindow gray, -- counter (east/west)
 Tile 8593 (Still 12 11) TerrainWindow gray, -- counter (west end)
 Tile 8590 (Still 15 2) TerrainWindow gray, -- counter (north end)
 Tile 9381 (Still 15 3) TerrainWindow gray, -- counter (north/south)
 Tile 4094 (Still 15 4) TerrainWindow gray, -- counter (south end)
 Tile 8601 (Still 14 11) TerrainWindow gray, -- table (east/west)
 Tile 5275 (Still 14 10) TerrainWindow gray, -- table (north/south)
 Tile 0636 (Still 14 9) TerrainWindow gray, -- table w/ plates (east/west)
 Tile 8011 (Still 14 8) TerrainWindow gray, -- table w/ plates (north/south)
 Tile 1198 (Still 13 9) TerrainWindow gray, -- good alter
 Tile 9927 (Still 15 6) TerrainWindow gray, -- bloody alter
 Tile 2367 (Still 13 10) TerrainWindow gray, -- chest
 Tile 9475 (Still 13 11) TerrainOpen gray, -- chair
 Tile 2060 (Still 14 2) TerrainOpen gray, -- chair
 Tile 2118 (Still 14 3) TerrainOpen gray, -- chair
 Tile 5736 (Still 14 4) TerrainOpen gray, -- chair
 Tile 0422 (Still 14 5) TerrainWindow gray, -- bed
 Tile 4530 (Still 23 2) TerrainWindow gray, -- bed w/ person
 Tile 8329 (Still 14 6) TerrainWindow gray, -- throne
 Tile 1231 (Still 14 7) TerrainWindow gray, -- dresser
 Tile 3432 (Still 15 1) TerrainWindow gray, -- wardrobe
 Tile 9028 (Still 15 5) TerrainWindow gray, -- cauldron
 Tile 1969 (Still 15 7) TerrainOpen gray, -- rug
 Tile 7555 (Still 15 8) TerrainWindow gray, -- anvil
 Tile 0761 (Still 21 5) TerrainOpen gray, -- lever right
 Tile 7991 (Still 21 6) TerrainOpen gray, -- lever left
 Tile 4682 (Still 44 11) TerrainWindow gray, -- wheel
 Tile 3813 (Still 12 1) TerrainOpen gray, -- stone floor w/ snow

 Tile 0040 (Still 15 9) TerrainOpen lightgray, -- white tile floor
 Tile 6711 (Still 16 4) TerrainWindow lightgray, -- column
 Tile 5268 (Still 15 10) TerrainWindow lightgray, -- white alter
 Tile 9820 (Still 15 11) TerrainWindow lightgray, -- black alter
 Tile 8211 (Still 16 2) TerrainWindow lightgray, -- brazier
 Tile 6392 (Still 16 3) TerrainWindow lightgray, -- throne

 Tile 4252 (Still 16 5) TerrainOpen green, -- green tile floor
 Tile 5404 (Still 16 8) TerrainOpen green, -- rug
 Tile 9056 (Still 16 9) TerrainWindow cyan, -- small ice crystal
 Tile 9349 (Still 24 8) TerrainWindow cyan, -- ice column
 Tile 4357 (Still 21 11) TerrainSolid cyan, -- large ice crystal
 Tile 7021 (Still 17 2) TerrainOpen green, -- rune
 Tile 5215 (Still 17 3) TerrainWindow green, -- throne
 Tile 3038 (Still 17 9) TerrainHover green, -- fire pit
 Tile 5664 (Still 24 9) TerrainOpen green, -- pentagram top-left
 Tile 7118 (Still 24 10) TerrainOpen green, -- pentagram top-right
 Tile 7760 (Still 24 11) TerrainOpen green, -- pentagram bottom-left
 Tile 1433 (Still 25 2) TerrainOpen green, -- pentagram bottom-right

 Tile 5709 (Still 0 2) TerrainOpen lightgray, -- snow
 Tile 3930 (Still 0 3) TerrainOpen lightgray, -- snow w/ three mushrooms
 Tile 7591 (Still 7 4) TerrainOpen lightgray, -- snow w/ five mushrooms
 Tile 6995 (Still 7 3) TerrainWindow lightgray, -- dead shrubs
 Tile 2571 (Still 7 2) TerrainWindow brown, -- dead tree
 Tile 1332 (Still 1 0) TerrainSolid brown, -- big dead tree
 Tile 7122 (Still 1 1) TerrainWindow green, -- three evergreens
 Tile 0781 (Still 2 0) TerrainSolid darkgreen, -- dense evergreens
 Tile 3384 (Still 6 9) TerrainWindow gray, -- small rocks
 Tile 3236 (Still 6 10) TerrainWindow gray, -- big rocks left
 Tile 2011 (Still 22 4) TerrainWindow gray, -- big rocks center
 Tile 8721 (Still 6 11) TerrainWindow gray, -- big rocks right
 Tile 5390 (Still 24 7) TerrainSolid cyan, -- snow w/ ice wall
 Tile 9409 (Still 18 2) TerrainOpen lightgray, -- green trash
 Tile 9456 (Still 18 4) TerrainOpen lightgray, -- red trash
 Tile 1287 (Still 18 5) TerrainWindow lightgray, -- signpost
 Tile 4556 (Still 18 6) TerrainWindow lightgray, -- obelisk
 Tile 8284 (Still 18 8) TerrainWindow lightgray, -- statue
 Tile 0563 (Anim 46 8 4 NoOverlay) TerrainWindow orange, -- campfire
 Tile 5643 (Still 23 7) TerrainWindow lightgray, -- stalagmites

 Tile 7032 (Still 22 0) TerrainOpen gray, -- snowy road
 Tile 3714 (Still 22 1) TerrainOpen gray,
 Tile 6386 (Still 23 0) TerrainOpen gray,
 Tile 3082 (Still 23 1) TerrainOpen gray,

 Tile 5203 (Still 0 7) TerrainSolid purple, -- snow wall
 Tile 1455 (Still 0 8) TerrainSolid purple,
 Tile 6668 (Still 0 9) TerrainSolid purple,
 Tile 6722 (Still 0 10) TerrainSolid purple,
 Tile 0245 (Still 0 11) TerrainSolid purple,
 Tile 2645 (Still 1 2) TerrainSolid purple,
 Tile 0059 (Still 1 3) TerrainSolid purple,
 Tile 8854 (Still 1 4) TerrainSolid purple,
 Tile 0941 (Still 1 5) TerrainSolid purple,
 Tile 4238 (Still 1 6) TerrainSolid purple,
 Tile 1331 (Still 1 7) TerrainSolid purple,
 Tile 7167 (Still 1 8) TerrainSolid purple,
 Tile 7043 (Still 1 9) TerrainSolid purple,

 Tile 3425 (Still 33 4) TerrainSolid purple, -- snow/cave wall
 Tile 1968 (Still 33 5) TerrainSolid purple,
 Tile 4167 (Still 33 6) TerrainSolid purple,
 Tile 3630 (Still 33 7) TerrainSolid purple,
 Tile 8963 (Still 33 8) TerrainSolid purple,
 Tile 6115 (Still 33 9) TerrainSolid purple,
 Tile 3402 (Still 33 10) TerrainSolid purple,
 Tile 1279 (Still 33 11) TerrainSolid purple,

 Tile 5679 (Still 32 2) TerrainOpen lightgray, -- snow/cave floor
 Tile 9193 (Still 32 3) TerrainOpen lightgray,
 Tile 1548 (Still 32 4) TerrainOpen lightgray,
 Tile 1210 (Still 32 5) TerrainOpen lightgray,
 Tile 7007 (Still 32 6) TerrainOpen lightgray,
 Tile 5586 (Still 32 7) TerrainOpen lightgray,
 Tile 6754 (Still 32 8) TerrainOpen lightgray,
 Tile 2158 (Still 32 9) TerrainOpen lightgray,
 Tile 4973 (Still 32 10) TerrainOpen lightgray,
 Tile 6025 (Still 32 11) TerrainOpen lightgray,
 Tile 7303 (Still 33 2) TerrainOpen lightgray,
 Tile 5329 (Still 33 3) TerrainOpen lightgray,
 Tile 7042 (Still 34 9) TerrainWindow lightgray, -- stalagmites

 Tile 1171 (Still 26 8) TerrainOpen bluegreen, -- cave floor
 Tile 6498 (Still 26 9) TerrainOpen bluegreen, -- w/ shrooms
 Tile 8959 (Still 28 11) TerrainOpen bluegreen, -- w/ shrooms
 Tile 4581 (Still 28 10) TerrainOpen bluegreen, -- w/ shrooms
 Tile 9760 (Still 31 2) TerrainOpen bluegreen, -- w/ shrooms
 Tile 1376 (Still 28 5) TerrainWindow gray, -- small rocks
 Tile 0772 (Still 28 6) TerrainWindow gray, -- big rocks left
 Tile 0179 (Still 29 2) TerrainWindow gray, -- big rocks center
 Tile 6341 (Still 28 7) TerrainWindow gray, -- big rocks right
 Tile 5892 (Still 29 3) TerrainHover blue, -- small pool
 Tile 6109 (Still 29 5) TerrainWindow bluegreen, -- stalagmites
 Tile 6914 (Still 29 6) TerrainWindow bluegreen, -- stalagmites
 Tile 7234 (Still 30 10) TerrainWindow bluegreen, -- stalagmites
 Tile 5653 (Still 30 11) TerrainWindow bluegreen, -- stalagmites
 Tile 5073 (Still 29 9) TerrainOpen bluegreen, -- green trash
 Tile 6814 (Still 29 11) TerrainOpen bluegreen, -- red trash
 Tile 3086 (Still 29 7) TerrainWindow bluegreen, -- chest
 Tile 6852 (Still 30 2) TerrainWindow bluegreen, -- sign
 Tile 0545 (Still 30 3) TerrainWindow bluegreen, -- obelisk
 Tile 5306 (Still 30 4) TerrainWindow bluegreen, -- totems
 Tile 4196 (Still 30 5) TerrainWindow bluegreen, -- statue

 Tile 3431 (Still 28 1) TerrainOpen gray, -- mine tracks
 Tile 4408 (Still 29 0) TerrainOpen gray,
 Tile 3899 (Still 29 1) TerrainOpen gray,
 Tile 0486 (Still 30 0) TerrainOpen gray,
 Tile 2317 (Still 30 1) TerrainOpen gray,
 Tile 9224 (Still 31 0) TerrainOpen gray,
 Tile 3915 (Still 31 1) TerrainOpen gray,
 Tile 8591 (Still 32 0) TerrainOpen gray,
 Tile 6079 (Still 32 1) TerrainOpen gray,
 Tile 9108 (Still 33 0) TerrainOpen gray,
 Tile 3895 (Still 33 1) TerrainOpen gray,
 Tile 8300 (Still 34 0) TerrainWindow gray, -- mine cart horz
 Tile 5199 (Still 34 1) TerrainWindow gray, -- mine cart vert
 Tile 3187 (Still 35 0) TerrainWindow gray, -- mine cart horz w/ rocks
 Tile 3525 (Still 35 1) TerrainWindow gray, -- mine cart vert w/ rocks

 Tile 1422 (Still 26 10) TerrainSolid purple, -- cave wall
 Tile 8648 (Still 26 11) TerrainSolid purple,
 Tile 7655 (Still 27 2) TerrainSolid purple,
 Tile 7069 (Still 27 3) TerrainSolid purple,
 Tile 9022 (Still 27 4) TerrainSolid purple,
 Tile 9090 (Still 27 5) TerrainSolid purple,
 Tile 2636 (Still 27 6) TerrainSolid purple,
 Tile 8111 (Still 27 7) TerrainSolid purple,
 Tile 5652 (Still 27 8) TerrainSolid purple,
 Tile 2680 (Still 27 9) TerrainSolid purple,
 Tile 9166 (Still 27 10) TerrainSolid purple,
 Tile 5750 (Still 27 11) TerrainSolid purple,
 Tile 1212 (Still 28 2) TerrainSolid purple,
 Tile 4444 (Still 27 0) TerrainSmoke purple, -- cave passwall
 Tile 5916 (Still 27 1) TerrainSmoke purple, -- cave passwall

 Tile 2937 (Anim 45 0 15 NoOverlay) TerrainHover blue, -- water
 Tile 5658 (Anim 48 0 15 NoOverlay) TerrainWindow blue, -- water rock
 Tile 4863 (Anim 45 4 15 NoOverlay) TerrainWindow blue, -- waterfall
 Tile 0295 (Anim 45 0 15 $ Overlay 0 0) TerrainHover blue, -- w/ cave floor
 Tile 6760 (Anim 45 0 15 $ Overlay 0 1) TerrainHover blue,
 Tile 2443 (Anim 45 0 15 $ Overlay 0 2) TerrainHover blue,
 Tile 6996 (Anim 45 0 15 $ Overlay 0 3) TerrainHover blue,
 Tile 9878 (Anim 45 0 15 $ Overlay 0 4) TerrainHover blue,
 Tile 4701 (Anim 45 0 15 $ Overlay 0 5) TerrainHover blue,
 Tile 6921 (Anim 45 0 15 $ Overlay 0 6) TerrainHover blue,
 Tile 3235 (Anim 45 0 15 $ Overlay 0 7) TerrainHover blue,
 Tile 1701 (Anim 45 0 15 $ Overlay 1 4) TerrainHover blue,
 Tile 5376 (Anim 45 0 15 $ Overlay 1 5) TerrainHover blue,
 Tile 7629 (Anim 45 0 15 $ Overlay 2 0) TerrainOpen gray, -- w/ vert bridge
 Tile 7108 (Anim 45 0 15 $ Overlay 2 1) TerrainOpen gray,
 Tile 7264 (Anim 45 0 15 $ Overlay 2 2) TerrainOpen gray,
 Tile 7739 (Anim 45 0 15 $ Overlay 2 3) TerrainOpen gray,
 Tile 1917 (Anim 45 0 15 $ Overlay 2 4) TerrainOpen gray, -- w/ horz bridge
 Tile 5497 (Anim 45 0 15 $ Overlay 2 5) TerrainOpen gray,
 Tile 6446 (Anim 45 0 15 $ Overlay 2 6) TerrainOpen gray,
 Tile 8790 (Anim 45 0 15 $ Overlay 2 7) TerrainOpen gray,
 Tile 2494 (Anim 45 0 15 $ Overlay 3 0) TerrainHover blue, -- w/ cave wall
 Tile 2431 (Anim 45 0 15 $ Overlay 3 1) TerrainHover blue,
 Tile 3058 (Anim 45 0 15 $ Overlay 3 2) TerrainHover blue,
 Tile 0367 (Anim 45 0 15 $ Overlay 3 3) TerrainHover blue,
 Tile 2864 (Anim 45 0 15 $ Overlay 3 4) TerrainHover blue,
 Tile 4648 (Anim 45 0 15 $ Overlay 3 5) TerrainHover blue,
 Tile 9755 (Anim 45 0 15 $ Overlay 3 6) TerrainHover blue,
 Tile 8118 (Anim 45 0 15 $ Overlay 3 7) TerrainHover blue,
 Tile 5153 (Anim 45 0 15 $ Overlay 4 0) TerrainHover blue, -- w/ floor/wall
 Tile 5183 (Anim 45 0 15 $ Overlay 4 1) TerrainHover blue,
 Tile 5641 (Anim 45 0 15 $ Overlay 4 2) TerrainHover blue,
 Tile 8290 (Anim 45 0 15 $ Overlay 4 3) TerrainHover blue,
 Tile 3530 (Anim 45 0 15 $ Overlay 4 4) TerrainHover blue,
 Tile 4921 (Anim 45 0 15 $ Overlay 4 5) TerrainHover blue,
 Tile 3361 (Anim 45 0 15 $ Overlay 4 6) TerrainHover blue,
 Tile 2212 (Anim 45 0 15 $ Overlay 4 7) TerrainHover blue,
 Tile 2776 (Anim 45 0 15 $ Overlay 5 0) TerrainHover blue, -- w/ snow
 Tile 2885 (Anim 45 0 15 $ Overlay 5 1) TerrainHover blue,
 Tile 2206 (Anim 45 0 15 $ Overlay 5 2) TerrainHover blue,
 Tile 8474 (Anim 45 0 15 $ Overlay 5 3) TerrainHover blue,
 Tile 6450 (Anim 45 0 15 $ Overlay 5 4) TerrainHover blue,
 Tile 4061 (Anim 45 0 15 $ Overlay 5 5) TerrainHover blue,
 Tile 3848 (Anim 45 0 15 $ Overlay 5 6) TerrainHover blue,
 Tile 0073 (Anim 45 0 15 $ Overlay 5 7) TerrainHover blue,

 Tile 0285 (Anim 49 0 6 NoOverlay) TerrainHover orange] -- lava

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

-------------------------------------------------------------------------------
