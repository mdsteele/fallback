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

{-# LANGUAGE DoRec #-}

module Fallback.Scenario.Triggers.Globals
  (Globals(..), compileGlobals, signRadius)
where

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.State.Area (Device)
import Fallback.State.Progress (splitVarSeed)
import Fallback.State.Resources (SoundTag(..))
import Fallback.State.Tileset (TileTag(..))

-------------------------------------------------------------------------------

data Globals = Globals
  { gAdobeDoor :: Device,
    gBasaltDoor :: Device,
    gStoneDoor :: Device }

-- | The standard interaction radius for signs/placards:
signRadius :: Int
signRadius = 3

-------------------------------------------------------------------------------

compileGlobals :: CompileScenario Globals
compileGlobals = do

  let newUnlockedDoor vseed cTag oTag = do
        let (cSeed, oSeed) = splitVarSeed vseed
        rec closed <- newDevice cSeed 1 $ \ge _ -> do
              tile <- getTerrainTile oTag
              setTerrain [(rectTopleft $ Grid.geRect ge, tile)]
              replaceDevice ge open
              playSound SndDoorOpen
            open <- newDevice oSeed 1 $ \ge _ -> do
              tile <- getTerrainTile cTag
              setTerrain [(rectTopleft $ Grid.geRect ge, tile)]
              replaceDevice ge closed
              playSound SndDoorShut
        return closed
  stoneDoor <- newUnlockedDoor 398282 StoneDoorClosedTile StoneDoorOpenTile
  basaltDoor <- newUnlockedDoor 349783 BasaltDoorClosedTile BasaltDoorOpenTile
  adobeDoor <- newUnlockedDoor 109823 AdobeDoorClosedTile AdobeDoorOpenTile

  return Globals { gAdobeDoor = adobeDoor, gBasaltDoor = basaltDoor,
                   gStoneDoor = stoneDoor }

-------------------------------------------------------------------------------
