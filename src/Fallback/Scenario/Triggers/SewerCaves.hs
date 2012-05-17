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

module Fallback.Scenario.Triggers.SewerCaves
  (compileSewerCaves)
where

import Control.Applicative ((<$>))
import Control.Monad (when)

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals (Globals(..), newDoorDevices)
import Fallback.Scenario.Triggers.Script
import Fallback.State.Creature (MonsterTownAI(ChaseAI))
import Fallback.State.Resources (SoundTag(SndLever))
import Fallback.State.Tags
  (AreaTag(Holmgare, SewerCaves), ItemTag(InertItemTag), InertItemTag(IronKey),
   MonsterTag(Dactylid))
import Fallback.State.Tileset (TileTag(..))

-------------------------------------------------------------------------------

compileSewerCaves :: Globals -> CompileScenario ()
compileSewerCaves globals = compileArea SewerCaves Nothing $ do

  makeExit Holmgare [Rect 29 42 7 2] (Point 32 40)

  ironKeyLockedBasaltDoor <- do
    let tryOpen _ _ = do
          hasKey <- doesPartyHaveItem (InertItemTag IronKey)
          setMessage $ if hasKey then "The door is locked, but you are able to\
                                      \ unlock it with the iron key."
                       else "The door is locked, and you don't have the key."
          when hasKey playDoorUnlockSound
          return hasKey
    let tryClose _ _ = return True
    fst <$> newDoorDevices 298322 BasaltDoorClosedTile BasaltDoorOpenTile
                           tryOpen tryClose

  onStartDaily 894928 $ do
    addDevice_ ironKeyLockedBasaltDoor (Point 2 34)
    addDevice_ (gBasaltDoor globals) (Point 4 36)
    addDevice_ (gBasaltDoor globals) (Point 12 22)
    addDevice_ (gBasaltDoor globals) (Point 12 36)
    addDevice_ ironKeyLockedBasaltDoor (Point 14 34)

  gateOpen <- newPersistentVar 189342 False
  uniqueDevice 023433 (Point 15 23) 1 $ \_ _ -> do
    isOpen <- readVar gateOpen
    if isOpen then setMessage "The gate has already been opened." else do
    playSound SndLever
    writeVar gateOpen True
    narrate "You turn the wheel to lift the gate, then wedge it so it will\
      \ stay open.  Now you can get in and out of here easily."
  daily 984354 (varTrue gateOpen) $ do
    tile <- getTerrainTile BasaltGateOpenTile
    setTerrain [(Point 16 22, tile)]

  once 874564 (walkIn (Rect 27 27 10 9)) $ do
    narrate "Phew, it stinks in here.  This is apparently the cave where the\
      \ citzens of Holmgare dispose of all their garbage and sewage.  You're\
      \ just glad it's so cold; the smell would be almost unbearable\
      \ otherwise.\n\n\
      \The cave doesn't appear to extend back very far, but according to\
      \ Sophia, there should be a secret passage hidden somewhere back there,\
      \ behind the muck."

  dactylidDead <- newPersistentVar 982452 False
  once 029345 (walkIn (Rect 1 25 15 9)) $ do
    narrate "FIXME Boss time!"
    do tile <- getTerrainTile BasaltGateClosedTile
       setTerrain [(Point 9 24, tile)]
    bossStartPos <- flip Point 33 <$> getRandomR 1 15
    addBasicEnemyMonster bossStartPos Dactylid (Just dactylidDead) ChaseAI
    startCombatWithTopleft (Point 0 23)
  once 923982 (varTrue dactylidDead) $ do
    do tile <- getTerrainTile BasaltGateOpenTile
       setTerrain [(Point 9 24, tile)]
    narrate "FIXME Yay, you won!"

-------------------------------------------------------------------------------
