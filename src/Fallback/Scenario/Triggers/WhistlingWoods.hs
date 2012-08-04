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

module Fallback.Scenario.Triggers.WhistlingWoods
  (compileWhistlingWoods)
where

import Control.Monad ((<=<), unless)

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Creature
import Fallback.State.Tags (AreaTag(..), MonsterTag(..))
import Fallback.State.Tileset (TileTag(SnowMushroomsTile))

-------------------------------------------------------------------------------

compileWhistlingWoods :: Globals -> CompileScenario ()
compileWhistlingWoods _globals = compileArea WhistlingWoods Nothing $ do

  makeExit IcyConfluence [Rect 0 32 2 9] (Point 3 36)
  makeExit Tragorda [Rect 24 64 10 2] (Point 28 62)
  makeExit Duskwood [Rect 80 39 2 6] (Point 78 42)

  uniqueDevice 198298 "RoadSign" signRadius $ \_ _ -> do
    narrate "There's a weather-beaten signpost along the road here.  It\
      \ says:\n\n\
      \      {c}City of Tragorda: 2 mi. S{_}\n\
      \      {c}Sabina Confluence: 3 mi. W{_}"

  once 239891 (walkIn "JustInside") $ do
    narrate "You venture into the forest north of Tragorda.  Since you didn't\
      \ find Sophia anywhere in the city, maybe you'll find her here.\n\n\
      \Other than on the wide pathway that you're standing in, the woods are\
      \ very thick.  The wind seems to whistle as it blows between the trees. \
      \ You stand and listen for a moment, and if you didn't know any better,\
      \ you'd say it was whistling not at random, but with a mournful tune."

  once 491840 (walkIn "IntersectionA") $ do
    narrate "The road through the woods bends to the west here, while a\
      \ narrower trail leads into the woods.  Possibly Sophia stayed along the\
      \ road, but since no one has found her yet, perhaps instead she's lost\
      \ in the woods somewhere.\n\n\
      \Again, you stand and listen for a moment.  Presently, you hear what\
      \ sounds like footsteps running away to the north."

  -- Forest gates:
  numGatesOpen <- newPersistentVar 401194 (0 :: Int)
  trigger 294134 (varIs (>= 1) numGatesOpen) $ do
    setTerrain SnowMushroomsTile =<< lookupTerrainMark "Gate1"
  trigger 729871 (varIs (>= 2) numGatesOpen) $ do
    setTerrain SnowMushroomsTile =<< lookupTerrainMark "Gate2"
  trigger 197837 (varIs (>= 3) numGatesOpen) $ do
    setTerrain SnowMushroomsTile =<< lookupTerrainMark "Gate3"
  trigger 068724 (varIs (>= 4) numGatesOpen) $ do
    setTerrain SnowMushroomsTile =<< lookupTerrainMark "Gate4"
  trigger 297510 (varIs (>= 5) numGatesOpen) $ do
    setTerrain SnowMushroomsTile =<< lookupTerrainMark "Gate5"

  -- Sophia:
  let maybeSophiaPos defaultAction actionFn = do
        numGates <- readVar numGatesOpen
        let mbMark = case numGates of
                       0 -> Just "Sophia1"
                       1 -> Just "Sophia2"
                       2 -> Just "Sophia5"
                       3 -> Just "Sophia5"
                       4 -> Just "Sophia5"
                       _ -> Nothing
        maybe defaultAction (actionFn <=< demandOneTerrainMark) mbMark
  sophiaKey <- newTransientVar 392018 $ do
    maybeSophiaPos (return Grid.nilKey) $ \pos -> do
    mbEntry <- tryAddMonster pos (makeMonster TownChildPurple)
      { monstIsAlly = True,
        monstName = "Sophia",
        monstTownAI = ImmobileAI }
    maybe (fail $ "Failed to place Sophia at " ++ show pos)
          (return . Grid.geKey) mbEntry
  let openGate num = do
        writeVar numGatesOpen num
        key <- readVar sophiaKey
        maybeSophiaPos (removeMonster key) $ \pos -> do
        ok <- tryMoveMonster key pos
        unless ok $ fail ("Failed to move Sophia to " ++ show pos)

  -- Encounters with Sophia:
  once 597194 (walkIn "NearGate1" `andP` varEq numGatesOpen 0) $ do
    narrate "Sophia runs off." -- FIXME
    openGate 1
  once 497811 (walkIn "NearGate2" `andP` varEq numGatesOpen 1) $ do
    narrate "Sophia runs off." -- FIXME
    openGate 2
  once 624091 (walkIn "NearGate3" `andP` varEq numGatesOpen 2) $ do
    openGate 3
  once 829716 (walkIn "NearGate4" `andP` varEq numGatesOpen 3) $ do
    openGate 4
  once 718394 (walkIn "NearGate5" `andP` varEq numGatesOpen 4) $ do
    narrate "Sophia runs off." -- FIXME
    openGate 5
    setAreaCleared WhistlingWoods True

-------------------------------------------------------------------------------
