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

module Fallback.Scenario.Triggers.IronMine
  (compileIronMine)
where

import Control.Monad (join, when)
import Data.List (intersperse)
import Data.Maybe (listToMaybe)

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Draw.Base (blitTopleft)
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Area
import Fallback.State.Doodad (Doodad(..), DoodadHeight(LowDood))
import Fallback.State.Resources
import Fallback.State.Tags
import Fallback.State.Terrain (positionCenter, positionTopleft)
import Fallback.State.Tileset (TileTag(..))
import Fallback.Utility (maybeM)

-------------------------------------------------------------------------------

compileIronMine :: Globals -> CompileScenario ()
compileIronMine globals = compileArea IronMine Nothing $ do

  makeExit Marata [Rect 42 58 10 2] (Point 46 56)

  onStartDaily 244106 $ do
    addUnlockedDoors globals
  uniqueDevice 335736 "Signpost" signRadius $ \_ _ -> do
    narrate "Someone has helpfully posted a sign here:\n\n\
      \          {b}MINE CLOSED{_}\n\
      \      {i}DANGER: UNDEAD!{_}"
  uniqueDevice 895064 "SwitchingSign" signRadius $ \_ _ -> do
    narrate "The sign tacked to the wall reads:\n\n\
      \      {i}SWITCHING STATION{_}"
  uniqueDevice 208934 "RecordsSign" signRadius $ \_ _ -> do
    narrate "The sign tacked to the wall reads:\n\n\
      \      {i}RECORDS OFFICE{_}"

  once 807555 (walkIn (Rect 18 18 3 2)) $ do
    narrate "Ulgghh.  It appears that the miners used this narrow tunnel as a\
      \ latrine, to save themselves the trouble of walking all the way back\
      \ outside the mine.  The smell of urine and garbage back here is awful."

  once 799563 (walkIn (Rect 9 1 12 9)) $ do
    narrate "Looking upwards, you see that the miners cut a ventilation shaft\
      \ in the ceiling of the cave here leading up to the surface above.  Snow\
      \ has drifted down the shaft from the outside, eventually building up\
      \ over a small patch of this chamber.\n\n\
      \The air is remarkably fresh and crisp in here.  Between the snow and\
      \ the cold air, you imagine that this chamber makes a lovely nest for\
      \ the two wild ice lizards that have somehow gotten in here.  They look\
      \ up at you from their nap, evidentally deciding that you will make a\
      \ nice meal."
    -- TODO make lizard chase you (change to wide-radius guard AI)

  -- Mine tracks/cart state:
  tracksSetToTurn <- newPersistentVar 231202 False
  -- The mineCartLocation should always be between 0 and 5 inclusive:
  --   0 means the cart is in its starting position
  --   1 means the cart is at the wall
  --   2 means the cart is near the switching station
  --   3 means the cart is near the cave entrance
  --   4 means the cart is in the rocks room
  --   5 means the cart has asploded the wall and is gone
  mineCartLocation <- newPersistentVar 626625 (0 :: Int)
  mineCartFilledWithRocks <- newPersistentVar 699149 False

  -- Switching station lever:
  let setLeverTile turn = do
        setTerrain (if turn then LeverRightTile else LeverLeftTile) =<<
          lookupTerrainMark "Lever"
  uniqueDevice 646620 "Lever" signRadius $ \_ _ -> do
    let labelStr turn = if turn then "TURN" else "STRAIGHT"
    current <- readVar tracksSetToTurn
    change <- forcedChoice
      ("There is a large iron lever mounted in the floor here.  On either\
       \ end of the metal base is engraved a label: \"STRAIGHT\" and\
       \ \"TURN.\"  The lever is currently in the \"" ++ labelStr current ++
       "\" position.")
      [("Move the lever to the \"" ++ labelStr (not current) ++
        "\" position", True), ("Leave it alone.", False)]
    when change $ do
    playSound SndLever
    writeVar tracksSetToTurn (not current)
    setLeverTile (not current)
    narrate "You hear several loud, metallic {i}clank{_} sounds echo through\
      \ the mine, and then silence."
  onStartDaily 789534 $ do
    setLeverTile =<< readVar tracksSetToTurn

  let addCartAtLocation cartDevice cartLoc = do
        let mbPos = listToMaybe $ snd $ cartPath False False cartLoc
        maybeM mbPos $ \position -> do
          cartFull <- readVar mineCartFilledWithRocks
          let tile = if cartLoc == 1
                     then (if cartFull then MineCartFullVertTile
                           else MineCartEmptyVertTile)
                     else (if cartFull then MineCartFullHorzTile
                           else MineCartEmptyHorzTile)
          setTerrain tile [position]
          addDevice_ cartDevice position

  let wallPositions = [Point x 26 | x <- [25, 26, 27]]

  mineCart <- newDevice 293845 1 $ \ge charNum -> do
    cartLoc <- readVar mineCartLocation
    cartFull <- readVar mineCartFilledWithRocks
    let fillCart = do
          writeVar mineCartFilledWithRocks True
          removeDevice (Grid.geKey ge)
          addCartAtLocation (Grid.geValue ge) cartLoc
          narrate "Oof, these rocks are heavy.  It takes several of you\
            \ working together to lift them up over the edge of the cart. \
            \ But it was well worth all that sweat and effort--{i}now{_} you\
            \ have a cart full of rocks!  It is very heavy.  Fortunately, it\
            \ is still relatively easy to push."
    let pushCart = do
          _charPos <- areaGet (arsCharacterPosition charNum)
          -- TODO if party is standing in the way, don't move the cart
          turn <- readVar tracksSetToTurn
          let (newLoc, path) = cartPath cartFull turn cartLoc
          removeDevice (Grid.geKey ge)
          resetTerrain [rectTopleft $ Grid.geRect ge]
          doMineCartChain cartFull path
          writeVar mineCartLocation newLoc
          addCartAtLocation (Grid.geValue ge) newLoc
          if (newLoc /= 5) then playSound SndMineCartStop else do
            shakeCamera 20 20
            playSound SndBoomBig
            let pt = positionCenter (Point 26 26) `pSub` Point 0 18
            forkScript $ doExplosionDoodad FireBoom pt
            wait 5 >> resetTerrain wallPositions
    let desc = if not cartFull then "It is currently empty."
               else "It is currently full of very heavy boulders."
    let canFill = cartLoc == 4 && not cartFull
    let suffix = if not canFill then "" else "\n\n\
          \You also notice that this chamber is scattered with loose\
          \ boulders.  They're far too heavy for you to carry very far, but\
          \ with some effort you could probably heave some of into the\
          \ cart, if you wanted to."
    let choices = (if not canFill then id
                   else (("Fill the cart with boulders.", fillCart) :))
                  [("Give the cart a shove.", pushCart),
                   ("Leave it alone.", return ())]
    join $ forcedChoice
      ("You examine the cart.  " ++ desc ++"  Despite its weight, it rolls\
       \ easily along the tracks; the axles seem to still be well-oiled.  If\
       \ you were to give it a shove, it would probably follow the  the\
       \ tracks all the way toward wherever they lead." ++ suffix) choices

  onStartDaily 450713 $ do
    cartLoc <- readVar mineCartLocation
    addCartAtLocation mineCart cartLoc
    when (cartLoc /= 5) $ do
      setTerrain AdobeCrackedWallTile wallPositions

  once 542400 (walkIn (Rect 45 1 9 7) `andP` varEq mineCartLocation 0) $ do
    narrate "Ah ha!  There's a mine cart sitting at the end of the tracks in\
      \ this chamber.  From here, it looks to still be in good condition. \
      \ Perhaps it can be of some use to you?"

-------------------------------------------------------------------------------

cartPath :: Bool {-^full-} -> Bool {-^turn-} -> Int {-^loc-}
         -> (Int, [Position])
cartPath _ _ 0 = (1, [Point 48 4, Point 51 4, Point 51 21, Point 26 21,
                      Point 26 25])
cartPath _ False 1 = (2, path1to2)
cartPath _ True 1 = (3, [Point 26 25, Point 26 21, Point 45 21, Point 45 23,
                         Point 49 23, Point 49 36, Point 39 36, Point 39 44,
                         Point 42 44, Point 42 51, Point 43 51])
cartPath full False 2 = (if full then 5 else 1, reverse path1to2)
cartPath _ True 2 = (3, path2to3)
cartPath _ False 3 = (4, path3to4)
cartPath _ True 3 = (2, reverse path2to3)
cartPath _ False 4 = (3, reverse path3to4)
cartPath _ True 4 = (2, [Point 15 26, Point  7 26, Point  7 30, Point  2 30,
                         Point  2 20, Point  6 20, Point  6 14, Point 10 14,
                         Point 10  8, Point 27  8, Point 27  3, Point 34  3,
                         Point 34 13, Point 35 13, Point 35 24, Point 31 24])
cartPath _ _ loc = (loc, [])

path1to2, path2to3, path3to4 :: [Position]
path1to2 = [Point 26 25, Point 26 13, Point 35 13, Point 35 24, Point 31 24]
path2to3 = [Point 31 24, Point 35 24, Point 35 14, Point 47 14, Point 47 23,
            Point 49 23, Point 49 36, Point 39 36, Point 39 44, Point 42 44,
            Point 42 51, Point 43 51]
path3to4 = [Point 43 51, Point 42 51, Point 42 44, Point 13 44, Point 13 35,
            Point  7 35, Point  7 26, Point 15 26]

-------------------------------------------------------------------------------

doMineCartDoodad :: (FromAreaEffect f) => Bool -> Position -> Position
                 -> Script f ()
doMineCartDoodad cartFull startPos endPos = do
  resources <- areaGet arsResources
  let sprite = rsrcSprite resources $
               if pointX startPos == pointX endPos
               then (if cartFull then MineCartFullVertSprite
                     else MineCartEmptyVertSprite)
               else (if cartFull then MineCartFullHorzSprite
                     else MineCartEmptyHorzSprite)
  let startPt = positionTopleft startPos
      endPt = positionTopleft endPos
  let limit = 2 * (abs (pointX startPos - pointX endPos) +
                   abs (pointY startPos - pointY endPos))
  let paint count cameraTopleft = do
        let Point dx dy = startPt `pSub` endPt
        let topleft = endPt `pSub` cameraTopleft `pAdd`
                      Point (dx * count `div` limit) (dy * count `div` limit)
        blitTopleft sprite topleft
  addDoodad $ Doodad { doodadCountdown = limit, doodadHeight = LowDood,
                       doodadPaint = paint }
  wait limit

doMineCartChain :: (FromAreaEffect f) => Bool -> [Position] -> Script f ()
doMineCartChain cartFull positions = do
  sequence_ $ intersperse (playSound SndMineCartTurn) $
    map (uncurry $ doMineCartDoodad cartFull) $ byPairs positions

byPairs :: [a] -> [(a, a)]
byPairs (x1 : x2 : xs) = (x1, x2) : byPairs (x2 : xs)
byPairs _ = []

-------------------------------------------------------------------------------
