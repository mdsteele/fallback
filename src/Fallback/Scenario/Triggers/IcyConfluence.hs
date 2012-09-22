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

module Fallback.Scenario.Triggers.IcyConfluence
  (compileIcyConfluence)
where

import Control.Applicative ((<$>))

import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Resources (SoundTag(SndLever))
import Fallback.State.Simple (FaceDir(..), Remains(Bones))
import Fallback.State.Tags (AreaTag(..), MonsterTag(..))
import Fallback.State.Tileset (TileTag(..))

-------------------------------------------------------------------------------

compileIcyConfluence :: Globals -> CompileScenario ()
compileIcyConfluence globals = do

  compileArea IcyConfluence Nothing $ do

    makeExit PerilousRoad ["ToPerilousRoad"] "FromPerilousRoad"
    makeExit Marata ["ToMarata"] "FromMarata"
    makeExit WhistlingWoods ["ToWhistlingWoods"] "FromWhistlingWoods"

    onStartDaily 591745 $ do
      addUnlockedDoors globals

    -- Gates:
    gatesOpen <- newPersistentVar 298019 False
    uniqueDevice 785914 "GateWheel" 1 $ \_ _ -> do
      nowOpen <- not <$> readVar gatesOpen
      playSound SndLever
      writeVar gatesOpen nowOpen
      setAreaCleared IcyConfluence nowOpen
      if nowOpen then do
        narrate "Finally, you reach the chain wheel that controls the bridge\
          \ gates.  You grab the handles and start turning it.  The mechanism\
          \ is old and creaky, but after six or seven full turns, all the\
          \ gates are completely lifted up.  Now it will be much easier to get\
          \ through this area."
      else do
        narrate "You spin the wheel the other way, closing all the gates\
          \ again.  You will no longer be able to cross the river."
    trigger 975917 (varFalse gatesOpen) $ do
      setTerrain StoneGateClosedTile =<< lookupTerrainMark "Gate"
    trigger 762098 (varTrue gatesOpen) $ do
      setTerrain StoneGateOpenTile =<< lookupTerrainMark "Gate"

    -- Stairs:

    trigger 109830 (walkOn "StairsA") $ do
      teleportToMark IcyConfluenceB "FromStairsA"

    foundStairsB <- newPersistentVar 979194 False

    trigger 789140 (walkOn "StairsB" `andP` varTrue foundStairsB) $ do
      teleportToMark IcyConfluenceB "FromStairsB"

    trigger 408904 (varFalse foundStairsB `andP` walkIn "NearStairsB" `andP`
                    varTrue (gLearnedAboutIcyConfluenceStairs globals)) $ do
      writeVar foundStairsB True
      narrate "Remembering what Sophia's father, Anton Vrell, told you in\
        \ Holmgare, you spend some time digging around in the snow here. \
        \ After several minutes, you finally hit something that sounds like\
        \ wood.  Clearing the snow aside, it turns out to be a trapdoor.  You\
        \ lift it up to reveal stairs leading underground.\n\n\
        \If Anton is to be believe, this passageway should lead you underneath\
        \ the guardhouse.  Eventually."

    trigger 280184 (varTrue foundStairsB) $ do
      setTerrain StairsDownEastTile =<< lookupTerrainMark "StairsB"

    -- Coming from Whistling Woods:

    once 579809 (areaCleared WhistlingWoods `andP`
                 (walkIn "PastEntrance1" `orP` walkIn "PastEntrance2")) $ do
      narrate "You pursue Sophia all the way down the road to near the river\
        \ here, at which point you lose sight of her.  She's quite the\
        \ sprightly youth, and unlike you, isn't burdened down by heavy armor\
        \ and equipment.  She must have crossed over the river ahead; if you\
        \ hurry, you may be able to catch up with her."

    -- Guardhouse and bridges:

    once 204890 (walkIn "NearSoutheastGate" `andP`
                 areaCleared WhistlingWoods) $ do
      narrate "You've reached a dead end for now--the narrow bridge across the\
        \ river is blocked by a heavy iron gate.  The bars of the gate are\
        \ somewhat widely spaced; Sophia looked small and skinny enough that\
        \ she may have been able to squeeze through and cross the river.  It'd\
        \ be a tight fit even for her, but she must have done it.  Of course,\
        \ there's no way that you could fit through.\n\n\
        \The guardhouse is right next to you, and from here you can plainly\
        \ see the wheel inside that likely controls these gates.  No one is\
        \ inside--in fact, it looks like no guards have been posted here for a\
        \ while now--but you can see the door on the other side of the\
        \ building.  Perhaps you can get inside and raise the gates."

    once 750982 (walkIn "NearIceWall") $ do
      narrate "You round the bend to where the guardhouse door is, only to\
        \ find that it has been neatly blocked off with a giant, impenetrable\
        \ wall of ice.  No wonder this place is abandoned.\n\n\
        \The ice wall looks just like the one you saw in the mountain pass\
        \ leading into Holmgare.  It's as if someone--specifically, someone\
        \ with incredible magical power--is trying to cut the various towns in\
        \ this region off from each other."

  compileArea IcyConfluenceB Nothing $ do

    onStartDaily 829801 $ do
      addUnlockedDoors globals
      mapM_ (addRemains Bones) =<< lookupTerrainMark "Bones"

    -- Stairs:
    trigger 401984 (walkOn "StairsA") $ do
      teleportToMark IcyConfluence "FromStairsA"
    trigger 740810 (walkOn "StairsB") $ do
      teleportToMark IcyConfluence "FromStairsB"

    simpleEnemy_ 448092 "BatA1" CaveBat MindlessAI
    simpleEnemy_ 294001 "BatA2" CaveBat MindlessAI

    simpleEnemy_ 550982 "RousB1" Rous MindlessAI
    simpleEnemy_ 802992 "RousB2" Rous MindlessAI
    simpleEnemy_ 582181 "RousB3" Rous MindlessAI

    simpleEnemy_ 760283 "BatC1" CaveBat MindlessAI
    simpleEnemy_ 287164 "BatC2" CaveBat MindlessAI
    simpleEnemy_ 409481 "RousC1" Rous MindlessAI
    simpleEnemy_ 019371 "RousC2" Rous MindlessAI
    simpleEnemy_ 319917 "RousC3" Rous MindlessAI

    once 582019 (walkIn "NearStatues") $ do
      narrate "You reach for your weapons to attack what look like more giant\
        \ rats, but then you realize that what you are seeing are two\
        \ (startlingly realistic) giant rat {i}statues{_}.  They are very\
        \ life-like; other than the stone texture, they look just like the\
        \ real thing.  One of them is standing, the other seems to be in\
        \ mid-lunge, as though attacking.\n\n\
        \Why a master sculptor would create the perfect likeness of two\
        \ unusually-sized rodents and then leave those statues down here in\
        \ this tunnel is not clear."
    simpleEnemy_ 750188 "Basilisk" Basilisk MindlessAI

    (drakeKey, drakeDead) <-
      scriptedMonster 479179 "Drake" BlueDrake False
                      (GuardAI 1 "Drake" FaceRight)
    once 530181 (walkIn "NearDrake" `andP` varFalse drakeDead) $ do
      narrate "...Ah.  Well.  Now you know where that room full of bones came\
        \ from.  They were the remains from the many meals of the blue drake\
        \ whose lair you have just stumbled into.  It does not seem very\
        \ pleased that you have invaded its home, and presently, it starts\
        \ lumbering over to you to express its displeasure."
      setMonsterTownAI (GuardAI 12 "Drake" FaceRight) =<< readVar drakeKey

-------------------------------------------------------------------------------
