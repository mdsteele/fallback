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
  (Globals(..), compileGlobals, newDoorDevices, signRadius)
where

import Control.Applicative ((<$>))
import Control.Monad.Fix (MonadFix)

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Script (massSetTerrain)
import Fallback.State.Area (AreaEffect, Device)
import Fallback.State.Progress (VarSeed, splitVarSeed)
import Fallback.State.Resources (SoundTag(..))
import Fallback.State.Simple (CharacterNumber)
import Fallback.State.Tileset (TileTag(..))
import Fallback.Utility (whenM)

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
        let succeed _ _ = return True
        fst <$> newDoorDevices vseed cTag oTag succeed succeed
  stoneDoor <- newUnlockedDoor 398282 StoneDoorClosedTile StoneDoorOpenTile
  basaltDoor <- newUnlockedDoor 349783 BasaltDoorClosedTile BasaltDoorOpenTile
  adobeDoor <- newUnlockedDoor 109823 AdobeDoorClosedTile AdobeDoorOpenTile

  return Globals { gAdobeDoor = adobeDoor, gBasaltDoor = basaltDoor,
                   gStoneDoor = stoneDoor }

-------------------------------------------------------------------------------

newDoorDevices :: (DefineDevice m, MonadFix m) => VarSeed -> TileTag -> TileTag
               -> (Grid.Entry Device -> CharacterNumber ->
                   Script AreaEffect Bool)
               -> (Grid.Entry Device -> CharacterNumber ->
                   Script AreaEffect Bool)
               -> m (Device, Device)
newDoorDevices vseed cTag oTag tryOpen tryClose = do
  (cSeed, oSeed) <- splitVarSeed vseed
  rec closed <- newDevice cSeed 1 $ \ge charNum -> do
        whenM (tryOpen ge charNum) $ do
          massSetTerrain oTag $ prectPositions $ Grid.geRect ge
          replaceDevice ge open
          playSound SndDoorOpen
      open <- newDevice oSeed 1 $ \ge charNum -> do
        -- TODO: don't allow door to be closed if enemies are nearby
        whenM (tryClose ge charNum) $ do
          massSetTerrain cTag $ prectPositions $ Grid.geRect ge
          replaceDevice ge closed
          playSound SndDoorShut
  return (closed, open)

-------------------------------------------------------------------------------
