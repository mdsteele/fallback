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

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)

import Fallback.Constants (combatArenaSize)
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Creature (Monster(..), MonsterTownAI(..), makeMonster)
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

  brassKeyLockedStoneDoor <- do
    let tryOpen _ _ = do
          hasKey <- doesPartyHaveItem (InertItemTag BrassKey)
          setMessage $ if hasKey then "The door is locked, but you are able\
                                      \ to unlock it with the brass key."
                       else "The door is locked, and you don't have the key."
          when hasKey playDoorUnlockSound
          return hasKey
    let tryClose _ _ = return True
    fst <$> newDoorDevices 867422 StoneDoorClosedTile StoneDoorOpenTile
                           tryOpen tryClose

  compileArea Icehold Nothing $ do

    makeExit Duskwood [Rect 0 42 50 2] (Point 25 40)

    onStartDaily 789321 $ do
      addDevice_ (gStoneDoor globals) (Point 2 20)
      addDevice_ (gStoneDoor globals) (Point 2 29)
      addDevice_ (gStoneDoor globals) (Point 3 5)
      addDevice_ (gStoneDoor globals) (Point 3 11)
      addDevice_ (gStoneDoor globals) (Point 3 35)
      addDevice_ (gStoneDoor globals) (Point 6 2)
      addDevice_ (gStoneDoor globals) (Point 6 8)
      addDevice_ (gStoneDoor globals) (Point 6 23)
      addDevice_ (gStoneDoor globals) (Point 6 29)
      addDevice_ (gStoneDoor globals) (Point 8 11)
      addDevice_ (gStoneDoor globals) (Point 9 5)
      addDevice_ (gStoneDoor globals) (Point 11 17)
      addDevice_ (gStoneDoor globals) (Point 11 31)
      addDevice_ (gStoneDoor globals) (Point 13 2)
      addDevice_ (gStoneDoor globals) (Point 14 19)
      addDevice_ (gStoneDoor globals) (Point 16 35)
      addDevice_ brassKeyLockedStoneDoor (Point 17 15)
      addDevice_ brassKeyLockedStoneDoor (Point 20 7)
      addDevice_ brassKeyLockedStoneDoor (Point 20 24)
      addDevice_ (gStoneDoor globals) (Point 20 27)
      addDevice_ brassKeyLockedStoneDoor (Point 22 2)
      addDevice_ brassKeyLockedStoneDoor (Point 23 31)
      addDevice_ brassKeyLockedStoneDoor (Point 28 2)
      addDevice_ brassKeyLockedStoneDoor (Point 30 27)
      addDevice_ (gStoneDoor globals) (Point 36 31)
      addDevice_ (gStoneDoor globals) (Point 38 25)
      addDevice_ (gStoneDoor globals) (Point 38 33)
      addDevice_ brassKeyLockedStoneDoor (Point 40 17)
      addDevice_ (gStoneDoor globals) (Point 40 20)
      addDevice_ (gStoneDoor globals) (Point 40 28)
      addDevice_ (gStoneDoor globals) (Point 40 31)
      addDevice_ (gStoneDoor globals) (Point 42 11)
      addDevice_ brassKeyLockedStoneDoor (Point 43 3)
      addDevice_ (gStoneDoor globals) (Point 43 8)
      addDevice_ (gStoneDoor globals) (Point 44 13)
      addDevice_ (gStoneDoor globals) (Point 47 11)
      addDevice_ (gStoneDoor globals) (Point 47 19)
      addDevice_ (gStoneDoor globals) (Point 47 26)
      addDevice_ (gStoneDoor globals) (Point 47 33)

    -- Stairs up to second floor:
    trigger 182832 (walkOn (Point 25 27)) $ do
      teleport Icehold2 (Point 21 27)
    -- Stairs back up from hidden treasure room:
    trigger 987237 (walkOn (Point 30 8)) $ do
      teleport Icehold2 (Point 24 8)

    -- Triggers to adjust contents of water tank:
    let waterTankRect = Rect 11 12 5 3 :: PRect
    trigger 427921 (varFalse waterPumpedUp) $ do
      massSetTerrain WaterAnimTile $ prectPositions waterTankRect
    trigger 987492 (varTrue waterPumpedUp) $ do
      massSetTerrain WhiteTileFloorTile $ prectPositions waterTankRect

    -- Triggers to adjust contents of lava tank:
    let lavaTankRect = Rect 35 12 5 3 :: PRect
    trigger 493742 (varFalse lavaPumpedUp) $ do
      massSetTerrain LavaAnimTile $ prectPositions lavaTankRect
    trigger 592874 (varTrue lavaPumpedUp) $ do
      massSetTerrain WhiteTileFloorTile $ prectPositions lavaTankRect

    -- Triggers to adjust contents of center tank:
    let centerTankPositions = [Point 21 13, Point 29 13] ++
          prectPositions (Rect 22 12 2 3) ++ prectPositions (Rect 27 12 2 3) ++
          prectPositions (Rect 24 11 3 5)
    trigger 489293 (varTrue waterReleased `andP` varFalse lavaReleased) $ do
      massSetTerrain WaterAnimTile centerTankPositions
    trigger 792974 (varFalse waterReleased `andP` varTrue lavaReleased) $ do
      massSetTerrain LavaAnimTile centerTankPositions
    trigger 947242 (varFalse waterReleased `xorP` varTrue lavaReleased) $ do
      massSetTerrain WhiteTileFloorTile centerTankPositions

    -- Triggers to adjust gates:
    let westGatePos = Point 18 13
        eastGatePos = Point 32 13
        southeastGatePos = Point 36 27
    trigger 872321 (varEq gateWheelSetting 2) $ do
      massSetTerrain StoneGateClosedTile [westGatePos]
      massSetTerrain StoneGateOpenTile [eastGatePos, southeastGatePos]
    trigger 642913 (varNeq gateWheelSetting 2) $ do
      massSetTerrain StoneGateOpenTile [westGatePos]
      massSetTerrain StoneGateClosedTile [eastGatePos, southeastGatePos]

    -- Wheel that controls gates:
    uniqueDevice 239987 (Point 25 19) 1 $ \_ _ -> conversation $ do
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

    uniqueDevice 749827 (Point 40 16) signRadius $ \_ _ -> do
      narrate "The sign mounted on the wall reads:\n\n\
        \      {b}HEATER INSPECTION ROOM{_}"
    uniqueDevice 234897 (Point 43 2) signRadius $ \_ _ -> do
      narrate "The sign mounted on the wall reads:\n\n\
        \      {b}FAMINE ROOM{_}"

  compileArea Icehold2 Nothing $ do

    onStartDaily 729891 $ do
      addDevice_ (gStoneDoor globals) (Point 2 8)
      addDevice_ (gStoneDoor globals) (Point 2 13)
      addDevice_ (gStoneDoor globals) (Point 3 17)
      addDevice_ (gStoneDoor globals) (Point 5 19)
      addDevice_ (gStoneDoor globals) (Point 8 2)
      addDevice_ (gStoneDoor globals) (Point 8 6)
      addDevice_ (gStoneDoor globals) (Point 8 29)
      addDevice_ (gStoneDoor globals) (Point 8 35)
      addDevice_ (gStoneDoor globals) (Point 12 2)
      addDevice_ (gStoneDoor globals) (Point 12 29)
      addDevice_ (gStoneDoor globals) (Point 12 35)
      addDevice_ (gStoneDoor globals) (Point 14 18)
      addDevice_ brassKeyLockedStoneDoor (Point 17 8)
      addDevice_ brassKeyLockedStoneDoor (Point 20 17)
      addDevice_ (gStoneDoor globals) (Point 28 2)
      addDevice_ (gStoneDoor globals) (Point 30 20)
      addDevice_ (gStoneDoor globals) (Point 30 23)
      addDevice_ (gStoneDoor globals) (Point 32 2)
      addDevice_ (gStoneDoor globals) (Point 34 20)
      addDevice_ (gStoneDoor globals) (Point 34 23)
      addDevice_ (gStoneDoor globals) (Point 36 32)
      addDevice_ (gStoneDoor globals) (Point 38 8)
      addDevice_ (gStoneDoor globals) (Point 38 15)

    -- Stairs up to third floor:
    trigger 094082 (walkOn (Point 15 8)) $ do
      teleport Icehold3 (Point 15 25)
    -- Stairs down to hidden treasure room:
    trigger 895279 (walkOn (Point 25 8)) $ do
      teleport Icehold (Point 31 8)
    -- Stairs down to main floor:
    trigger 982111 (walkOn (Point 20 27)) $ do
      teleport Icehold (Point 24 27)

    -- Triggers to adjust gates:
    let southGatePos = Point 20 31
        westGatePos = Point 15 27
        eastGatePos = Point 25 27
    trigger 874291 (varEq gateWheelSetting 0) $ do
      massSetTerrain StoneGateOpenTile [southGatePos]
      massSetTerrain StoneGateClosedTile [westGatePos, eastGatePos]
    trigger 479821 (varEq gateWheelSetting 1) $ do
      massSetTerrain StoneGateOpenTile [westGatePos]
      massSetTerrain StoneGateClosedTile [eastGatePos, southGatePos]
    trigger 792814 (varEq gateWheelSetting 2) $ do
      massSetTerrain StoneGateOpenTile [eastGatePos]
      massSetTerrain StoneGateClosedTile [southGatePos, westGatePos]

    -- Water pump control:
    uniqueDevice 561938 (Point 15 11) 1 $ \_ _ -> conversation $ do
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
    uniqueDevice 529850 (Point 25 11) 1 $ \_ _ -> conversation $ do
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
    let waterLeverPos = Point 13 15
    uniqueDevice 837104 waterLeverPos 1 $ \_ _ -> conversation $ do
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
    trigger 827153 (varTrue waterReleased) $ do
      massSetTerrain LeverRightTile [waterLeverPos]

    -- Lava release control:
    let lavaLeverPos = Point 27 15
    uniqueDevice 579140 lavaLeverPos 1 $ \_ _ -> conversation $ do
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
    trigger 982742 (varTrue lavaReleased) $ do
      massSetTerrain LeverLeftTile [lavaLeverPos]

    once 749829 (varTrue waterReleased `andP` varTrue lavaReleased) $ do
      return () -- FIXME steam

    uniqueDevice 727739 (Point 8 3) signRadius $ \_ _ -> do
      narrate "The sign mounted on the wall reads:\n\n\
        \      {b}CONTEMPLATION ROOM{_}"

  compileArea Icehold3 Nothing $ do

    onStartDaily 559839 $ do
      addDevice_ (gBasaltDoor globals) (Point 4 4)
      addDevice_ brassKeyLockedStoneDoor (Point 11 3)

    -- Stairs back down to second floor:
    trigger 472987 (walkOn (Point 16 25)) $ do
      teleport Icehold2 (Point 16 8)

    -- Boss fight:
    let bossChamberTopleft = Point 1 9
    let bossChamberRect = makeRect bossChamberTopleft combatArenaSize
    vhaegystDead <- newPersistentVar 459822 False
    vhaegystKey <- newTransientVar 109250 Grid.nilKey
    onStartDaily 209103 $ do
      whenP (varFalse vhaegystDead) $ do
        mbEntry <- tryAddMonster (Point 9 13) (makeMonster Vhaegyst)
          { monstDeadVar = Just vhaegystDead,
            monstIsAlly = True, -- don't attack yet
            monstTownAI = ImmobileAI }
        maybe (fail "failed to place Vhaegyst")
              (writeVar vhaegystKey . Grid.geKey) mbEntry
    once 729892 (walkIn bossChamberRect) $ do
      -- TODO conversation and so forth
      setMonsterIsAlly False =<< readVar vhaegystKey
      massSetTerrain BasaltGateClosedTile [Point 9 22, Point 10 22]
      startCombatWithTopleft (Point 1 9)
    trigger 099022 (varTrue vhaegystDead) $ do
      setAreaCleared Icehold True
      massSetTerrain BasaltGateOpenTile [Point 9 22, Point 10 22, Point 9 8]

    -- Sunrod pedestal:
    uniqueDevice 977299 (Point 16 3) 1 $ \_ _ -> do
      narrate "FIXME You get a Sunrod, yay!"

    uniqueDevice 449872 (Point 43 2) signRadius $ \_ _ -> do
      narrate "The sign mounted on the wall reads:\n\n\
        \      {b}SUNROOM{_}"

-------------------------------------------------------------------------------
