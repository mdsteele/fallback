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
  (Globals(..), compileGlobals, newDoorDevice, addUnlockedDoors, signRadius)
where

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Script (addDeviceOnMarks, massSetTerrain)
import Fallback.State.Area
import Fallback.State.Progress (VarSeed)
import Fallback.State.Resources (SoundTag(..), rsrcTileset)
import Fallback.State.Simple (CharacterNumber)
import Fallback.State.Tileset (TileTag(..), tilesetGet, ttId)
import Fallback.State.Terrain (terrainGetTile)
import Fallback.Utility (firstJust, maybeM, whenM)

-------------------------------------------------------------------------------

data Globals = Globals
  { gUnlockedDoor :: Device }

-- | The standard interaction radius for signs/placards:
signRadius :: Int
signRadius = 3

-------------------------------------------------------------------------------

compileGlobals :: CompileScenario Globals
compileGlobals = do
  unlockedDoor <- do
    let succeed _ _ = return True
    newDoorDevice 978249 succeed succeed
  return Globals { gUnlockedDoor = unlockedDoor }

-------------------------------------------------------------------------------

newDoorDevice :: (DefineDevice m) => VarSeed
               -> (Grid.Entry Device -> CharacterNumber ->
                   Script AreaEffect Bool)
               -> (Grid.Entry Device -> CharacterNumber ->
                   Script AreaEffect Bool)
               -> m Device
newDoorDevice vseed tryOpen tryClose = do
  newDevice vseed 1 $ \ge charNum -> do
    let pairs = [(AdobeDoorClosedTile, AdobeDoorOpenTile),
                 (BasaltDoorClosedTile, BasaltDoorOpenTile),
                 (StoneDoorClosedTile, StoneDoorOpenTile),
                 (WhitestoneDoorClosedTile, WhitestoneDoorOpenTile),
                 (WoodDoorClosedTile, WoodDoorOpenTile)]
    let pos = rectTopleft $ Grid.geRect ge
    mbOpenOther <- do
      tid <- areaGet (ttId . terrainGetTile pos . arsTerrain)
      tileset <- areaGet (rsrcTileset . arsResources)
      return $ flip firstJust pairs $ \(cTag, oTag) ->
        if ttId (tilesetGet cTag tileset) == tid then Just (False, oTag)
        else if ttId (tilesetGet oTag tileset) == tid then Just (True, cTag)
             else Nothing
    maybeM mbOpenOther $ \(isOpen, other) -> do
      let toggleTile = massSetTerrain other $ prectPositions $ Grid.geRect ge
      if isOpen then do
        -- TODO: Don't allow door to be closed if enemies are nearby, or if
        --       space is occupied, or if we're in combat.
        whenM (tryClose ge charNum) $ do
          toggleTile
          playSound SndDoorShut
      else do
        whenM (tryOpen ge charNum) $ do
          toggleTile
          playSound SndDoorOpen

-- | Add an unlocked door device to all positions marked in the terrain with
-- the string \"UD\".
addUnlockedDoors :: Globals -> Script TownEffect ()
addUnlockedDoors globals = do
  addDeviceOnMarks (gUnlockedDoor globals) "UD"

-------------------------------------------------------------------------------
