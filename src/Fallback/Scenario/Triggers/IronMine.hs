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

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals (Globals(..))
import Fallback.State.Area
import Fallback.State.Tags
import Fallback.State.Tileset (TileTag(..))
import Fallback.Utility (maybeM)

-------------------------------------------------------------------------------

compileIronMine :: Globals -> CompileScenario ()
compileIronMine globals = compileArea IronMine Nothing $ do
  -- The standard interaction radius for signs/placards:
  let signRadius = 3 :: Int

  onStartDaily 244106 $ do
    addDevice_ (gAdobeDoor globals) (Point 39 29)
    addDevice_ (gAdobeDoor globals) (Point 22 6)
  uniqueDevice 335736 (Point 45 54) signRadius $ \_ _ -> do
    narrate "Someone has helpfully posted a sign here:\n\n\
      \          {b}MINE CLOSED{_}\n\
      \      {i}DANGER: UNDEAD!{_}"
  uniqueDevice 895064 (Point 38 29) signRadius $ \_ _ -> do
    narrate "The sign tacked to the wall reads:\n\n\
      \      {i}SWITCHING STATION{_}"
  uniqueDevice 895064 (Point 23 6) signRadius $ \_ _ -> do
    narrate "The sign tacked to the wall reads:\n\n\
      \      {i}RECORDS OFFICE{_}"

  once 807555 (walkIn (Rect 18 18 3 2)) $ do
    narrate "Ulgghh.  It appears that the miners used this narrow tunnel as\
      \ a latrine, to save them the trouble of walking all the way back\
      \ outside the mine.  The smell of urine and garbage back here is\
      \ awful."

  once 799563 (walkIn (Rect 9 1 12 9)) $ do
    narrate "Looking upwards, you see that the miners cut a ventilation\
      \ shaft in the ceiling of the cave here leading up to the surface\
      \ above.  Snow has drifted down the shaft from the outside, eventually\
      \ building up over a small patch of this chamber.\n\n\
      \The air is remarkably fresh and crisp in here.  Between the snow and\
      \ the cold air, you imagine that this chamber makes a lovely nest for\
      \ the wild ice lizard that has somehow gotten in here.  It looks up at\
      \ you from its nap, evidentally deciding that you will make a nice\
      \ meal."
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
  uniqueDevice 646620 (Point 39 31) signRadius $ \ge _ -> do
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
    -- TODO play sound
    tile <- getTerrainTile (if current then LeverLeftTile
                            else LeverRightTile)
    setTerrain [(rectTopleft (Grid.geRect ge), tile)]
    writeVar tracksSetToTurn (not current)
    narrate "You hear several loud, metallic {i}clank{_} sounds echo through\
      \ the mine, and then silence."

  let addCartAtLocation cartDevice cartLoc = do
        let mbPos = case cartLoc of
                      0 -> Just $ Point 48 4
                      1 -> Just $ Point 26 25
                      2 -> Just $ Point 31 24
                      3 -> Just $ Point 43 52
                      4 -> Just $ Point 15 26
                      _ -> Nothing
        maybeM mbPos $ \position -> do
          cartFull <- readVar mineCartFilledWithRocks
          tile <- getTerrainTile $
                  if cartLoc == 1
                  then (if cartFull then MineCartFullVertTile
                        else MineCartEmptyVertTile)
                  else (if cartFull then MineCartFullHorzTile
                        else MineCartEmptyHorzTile)
          setTerrain [(position, tile)]
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
          let newLoc = case cartLoc of
                         0 -> 1
                         1 -> if turn then 3 else 2
                         2 -> if turn then 3 else if cartFull then 5 else 1
                         3 -> if turn then 2 else 4
                         4 -> if turn then 2 else 3
                         _ -> cartLoc
          removeDevice (Grid.geKey ge)
          resetTerrain [rectTopleft $ Grid.geRect ge]
          -- TODO doodad/sound for rolling cart
          writeVar mineCartLocation newLoc
          addCartAtLocation (Grid.geValue ge) newLoc
          when (newLoc == 5) $ do
            -- TODO splosion doodad/sound
            resetTerrain wallPositions
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
      tile <- getTerrainTile AdobeCrackedWallTile
      setTerrain $ map (flip (,) tile) $ wallPositions

  once 542400 (walkIn (Rect 45 1 9 7) `andP` varEq mineCartLocation 0) $ do
    narrate "Ah ha!  There's a mine cart sitting at the end of the tracks in\
      \ this chamber.  From here, it looks to still be in good condition. \
      \ Perhaps it can be of some use to you?"

-------------------------------------------------------------------------------
