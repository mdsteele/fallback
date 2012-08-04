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

module Fallback.Scenario.Triggers.Icehold
  (compileIcehold)
where

import Control.Monad (forM_, when)

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Resources (SoundTag(SndLever))
import Fallback.State.Tags
  (AreaTag(..), InertItemTag(BrassKey), ItemTag(InertItemTag), MonsterTag(..))
import Fallback.State.Tileset (TileTag(..))

-------------------------------------------------------------------------------

compileIcehold :: Globals -> CompileScenario ()
compileIcehold globals = do

  -- The gateWheelSetting should always be 0, 1, or 2:
  --   0 means the south gate on 2F and the west gate on 1F are open
  --   1 means the west gate on 2F and the west gate on 1F are open
  --   2 means the east gate on 2F and the east/southeast gates on 1F are open
  gateWheelSetting <- newGlobalVar 794921 (0 :: Int)

  waterPumpedUp <- newGlobalVar 092981 False
  waterReleased <- newGlobalVar 338402 False
  lavaPumpedUp <- newGlobalVar 019384 False
  lavaReleased <- newGlobalVar 813453 False

  brassKeyLockedDoor <- do
    let tryOpen _ _ = do
          hasKey <- doesPartyHaveItem (InertItemTag BrassKey)
          setMessage $ if hasKey then "The door is locked, but you are able\
                                      \ to unlock it with the brass key."
                       else "The door is locked, and you don't have the key."
          when hasKey playDoorUnlockSound
          return hasKey
    let tryClose _ _ = return True
    newDoorDevice 867422 tryOpen tryClose

  compileArea Icehold Nothing $ do

    makeExit Duskwood ["ToDuskwood"] "FromDuskwood"

    onStartDaily 789321 $ do
      addUnlockedDoors globals
      addDeviceOnMarks brassKeyLockedDoor "BrassDoor"

    -- Stairs up to second floor:
    trigger 182832 (walkOn "StairsUp") $ do
      teleport Icehold2 (Point 21 27)
    -- Stairs back up from hidden treasure room:
    trigger 987237 (walkOn "SecretStairsUp") $ do
      teleport Icehold2 (Point 24 8)

    -- Triggers to adjust contents of water tank:
    trigger 427921 (varFalse waterPumpedUp) $ do
      setTerrain WaterAnimTile . prectPositions =<<
        demandTerrainRect "WaterTank"
    trigger 987492 (varTrue waterPumpedUp) $ do
      setTerrain WhiteTileFloorTile . prectPositions =<<
        demandTerrainRect "WaterTank"

    -- Triggers to adjust contents of lava tank:
    trigger 493742 (varFalse lavaPumpedUp) $ do
      setTerrain LavaAnimTile . prectPositions =<< demandTerrainRect "FireTank"
    trigger 592874 (varTrue lavaPumpedUp) $ do
      setTerrain WhiteTileFloorTile . prectPositions =<<
        demandTerrainRect "FireTank"

    -- Triggers to adjust contents of center tank:
    trigger 489293 (varTrue waterReleased `andP` varFalse lavaReleased) $ do
      setTerrain WaterAnimTile =<< lookupTerrainMark "Mix"
    trigger 792974 (varFalse waterReleased `andP` varTrue lavaReleased) $ do
      setTerrain LavaAnimTile =<< lookupTerrainMark "Mix"
    trigger 947242 (varFalse waterReleased `xorP` varTrue lavaReleased) $ do
      setTerrain WhiteTileFloorTile =<< lookupTerrainMark "Mix"

    -- Triggers to adjust gates:
    let westGatePos = Point 18 13
        eastGatePos = Point 32 13
        southeastGatePos = Point 36 27
    trigger 872321 (varEq gateWheelSetting 2) $ do
      setTerrain StoneGateClosedTile [westGatePos]
      setTerrain StoneGateOpenTile [eastGatePos, southeastGatePos]
    trigger 642913 (varNeq gateWheelSetting 2) $ do
      setTerrain StoneGateOpenTile [westGatePos]
      setTerrain StoneGateClosedTile [eastGatePos, southeastGatePos]

    -- Wheel that controls gates:
    uniqueDevice 239987 "GateWheel" 1 $ \_ _ -> conversation $ do
      let settingName 0 = "SOUTH"
          settingName 1 = "WEST"
          settingName 2 = "EAST"
          settingName _ = "???"
      let setTo setting' = do
            playSound SndLever
            writeVar gateWheelSetting setting'
      setting <- readVar gateWheelSetting
      convText ("There is a large wheel here, with chains disappearing into\
        \ the floor.  On the mounting is a metal placard with a diagram of\
        \ what looks like the upstairs landing and its three gates, each\
        \ labelled with its compass direction, and a large arrow that points\
        \ to the rim of the wheel.  The wheel itself also has three labels on\
        \ it, spaced evenly around the ring, which read \"SOUTH\", \"WEST\",\
        \ and \"EAST.\"  The wheel is currently positioned so that the arrow\
        \ points to the " ++ settingName setting ++ " label.\n\n\
        \Presumably, this wheel controls which of the gates is opened.")
      convChoice (return ()) ("Leave the wheel in the " ++
                              settingName setting ++ " position.")
      forM_ [2, 1, 0] $ \n -> when (setting /= n) $ do
        convChoice (setTo n) ("Turn the wheel to the " ++ settingName n ++
                              " position.")
      convNode $ return ()

    uniqueDevice 749827 "HeaterSign" signRadius $ \_ _ -> do
      narrate "The sign mounted on the wall reads:\n\n\
        \      {b}HEATER INSPECTION ROOM{_}"
    uniqueDevice 234897 "FamineSign" signRadius $ \_ _ -> do
      narrate "The sign mounted on the wall reads:\n\n\
        \      {b}FAMINE ROOM{_}"

  compileArea Icehold2 Nothing $ do

    onStartDaily 729891 $ do
      addUnlockedDoors globals
      addDeviceOnMarks brassKeyLockedDoor "BrassDoor"
      addDeviceOnMarks brassKeyLockedDoor "SteamDoor" -- FIXME

    -- Stairs up to third floor:
    trigger 094082 (walkOn "StairsUp") $ do
      teleport Icehold3 (Point 15 25)
    -- Stairs down to hidden treasure room:
    trigger 895279 (walkOn "SecretStairsDown") $ do
      teleport Icehold (Point 31 8)
    -- Stairs down to main floor:
    trigger 982111 (walkOn "StairsDown") $ do
      teleport Icehold (Point 24 27)

    -- Triggers to adjust gates:
    let southGatePos = Point 20 31
        westGatePos = Point 15 27
        eastGatePos = Point 25 27
    trigger 874291 (varEq gateWheelSetting 0) $ do
      setTerrain StoneGateOpenTile [southGatePos]
      setTerrain StoneGateClosedTile [westGatePos, eastGatePos]
    trigger 479821 (varEq gateWheelSetting 1) $ do
      setTerrain StoneGateOpenTile [westGatePos]
      setTerrain StoneGateClosedTile [eastGatePos, southGatePos]
    trigger 792814 (varEq gateWheelSetting 2) $ do
      setTerrain StoneGateOpenTile [eastGatePos]
      setTerrain StoneGateClosedTile [southGatePos, westGatePos]

    -- Water pump control:
    uniqueDevice 561938 "WaterWheel" 1 $ \_ _ -> conversation $ do
      convText "There is a control wheel here, with a placard that\
        \ reads:\n\n\
        \      {b}WEST PUMP CONTROL{_}"
      convChoice (return ()) "Leave it alone."
      flip convChoice "Turn the wheel." $ do
        whenP (varTrue waterPumpedUp) $ convNode $ do
          convReset
          convText "The wheel is still locked in place; you can't move it in\
            \ either direction."
          whenP (varFalse waterReleased) $ convText "  If there's any way to\
            \ release the water back down to the ground floor, it's not clear\
            \ that you can do it from here."
        whenP (varFalse waterPumpedUp) $ do
          playSound SndLever
          narrate "You start turning the wheel.  At first, nothing happens,\
            \ but after a few rotations you hear the grinding sound of some\
            \ kind of machinery coming to life.  You hear a faint gurgling\
            \ sound, which grows closer and louder until you see water\
            \ spilling out into the narrow chamber to the south.  Apparently\
            \ it is being pumped upwards from the tank you saw on the ground\
            \ floor.\n\n\
            \Finally, the noise stops, and with a click the control wheel\
            \ locks into place.  The chamber on this floor is now full of\
            \ water.  Hopefully, this will somehow be helpful to you."
          writeVar waterPumpedUp True
      convNode $ return ()

    -- Lava pump control:
    uniqueDevice 529850 "FireWheel" 1 $ \_ _ -> conversation $ do
      convText "There is a control wheel here, with a placard that\
        \ reads:\n\n\
        \      {b}EAST PUMP CONTROL{_}"
      convChoice (return ()) "Leave it alone."
      flip convChoice "Turn the wheel." $ do
        whenP (varTrue lavaPumpedUp) $ convNode $ do
          convReset
          convText "The wheel is still locked in place; you can't move it in\
            \ either direction."
          whenP (varFalse lavaReleased) $ convText "  If there's any way to\
            \ release the fire back down to the ground floor, it's not clear\
            \ that you can do it from here."
        whenP (varFalse lavaPumpedUp) $ do
          playSound SndLever
          narrate "You start turning the wheel.  At first, nothing happens,\
            \ but after a few rotations you hear the grinding sound of some\
            \ kind of machinery coming to life.  You hear a sort of roaring,\
            \ faint at first, but which soon grows closer and louder. \
            \ Suddenly, you feel a wave of heat coming out of the observation\
            \ window to the south, and a moment latersome kind of firey,\
            \ burning mass sweeps into the narrow chamber.  Apparently it is\
            \ being pumped upwards from the ground floor.\n\n\
            \Finally, the noise stops, and with a click the control wheel\
            \ locks into place.  The chamber on this floor is now full of\
            \ lava, or burning oil, or whatever it is."
          writeVar lavaPumpedUp True
      convNode $ return ()

    -- Water release control:
    trigger 827153 (varTrue waterReleased) $ do
      setTerrain LeverRightTile =<< lookupTerrainMark "WaterLever"
    uniqueDevice 837104 "WaterLever" 1 $ \_ _ -> conversation $ do
      convText "There is a lever in the floor here, with no label to suggest\
        \ what it might control."
      convChoice (return ()) "Leave it alone."
      whenP (varTrue waterReleased) $ do
        flip convChoice "Push the lever." $ do
          narrate "You try to push the lever, but it seems to be firmly locked\
            \ into place.  You are unable to push it back to its original\
            \ position."
      whenP (varFalse waterReleased) $ do
        flip convChoice "Pull the lever." $ do
          whenP (varFalse waterPumpedUp) $ do
            narrate "You try to pull the lever, but it seems to be locked in\
              \ its current position for now.  There must be something else\
              \ you need to do first."
          whenP (varTrue waterPumpedUp) $ do
            playSound SndLever
            narrate "You give the lever a pull.  You have to apply a fair bit\
              \ of strength to lift it, but once you do it slams into place on\
              \ the other side, and you immediately hear a loud rushing\
              \ noise.  The water in the narrow chamber to the north is\
              \ draining down into the large chamber the first floor."
            writeVar waterReleased True

    -- Lava release control:
    trigger 982742 (varTrue lavaReleased) $ do
      setTerrain LeverLeftTile =<< lookupTerrainMark "FireLever"
    uniqueDevice 579140 "FireLever" 1 $ \_ _ -> conversation $ do
      convText "There is a lever in the floor here, with no label to suggest\
        \ what it might control."
      convChoice (return ()) "Leave it alone."
      whenP (varTrue lavaReleased) $ do
        flip convChoice "Push the lever." $ do
          narrate "You try to push the lever, but it seems to be firmly locked\
            \ into place.  You are unable to push it back to its original\
            \ position."
      whenP (varFalse lavaReleased) $ do
        flip convChoice "Pull the lever." $ do
          whenP (varFalse lavaPumpedUp) $ do
            narrate "You try to pull the lever, but it seems to be locked in\
              \ its current position for now.  There must be something else\
              \ you need to do first."
          whenP (varTrue lavaPumpedUp) $ do
            playSound SndLever
            narrate "You give the lever a pull.  You have to apply a fair bit\
              \ of strength to lift it, but once you do it slams into place on\
              \ the other side, and you immediately hear a loud roaring\
              \ noise.  The firey liquid in the narrow chamber to the north is\
              \ draining down into the large chamber the first floor."
            writeVar lavaReleased True

    once 749829 (varTrue waterReleased `andP` varTrue lavaReleased) $ do
      return () -- FIXME steam

    uniqueDevice 727739 "ContemplationSign" signRadius $ \_ _ -> do
      narrate "The sign mounted on the wall reads:\n\n\
        \      {b}CONTEMPLATION ROOM{_}"

  compileArea Icehold3 Nothing $ do

    onStartDaily 559839 $ do
      addDeviceOnMarks brassKeyLockedDoor "BrassDoor"

    -- Stairs back down to second floor:
    trigger 472987 (walkOn "StairsDown") $ do
      teleport Icehold2 (Point 16 8)

    -- Boss fight:
    (vhaegystKey, vhaegystDead) <-
      scriptedMonster 209103 "Vhaegyst" Vhaegyst True ImmobileAI
    once 729892 (walkIn "BossRoom") $ do
      -- TODO conversation and so forth
      setMonsterIsAlly False =<< readVar vhaegystKey
      setTerrain BasaltGateClosedTile =<< lookupTerrainMark "SouthGate"
      startBossFight "BossRoom"
    trigger 099022 (varTrue vhaegystDead) $ do
      setLevelCap 16
      setAreaCleared Icehold True
      setTerrain BasaltGateOpenTile =<< lookupTerrainMark "SouthGate"
      setTerrain BasaltGateOpenTile =<< lookupTerrainMark "NorthGate"

    -- Sunrod pedestal:
    uniqueDevice 977299 "SunPedestal" 1 $ \_ _ -> do
      narrate "FIXME You get a Sunrod, yay!"

    uniqueDevice 449872 "SunroomSign" signRadius $ \_ _ -> do
      narrate "The sign mounted on the wall reads:\n\n\
        \      {b}SUNROOM{_}"

-------------------------------------------------------------------------------
