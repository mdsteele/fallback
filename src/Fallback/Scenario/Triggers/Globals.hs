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
  (Globals(..), compileGlobals,
   newDoorDevice, addUnlockedDoors, uniqueDevice,
   simpleMonster, simpleEnemy, simpleEnemy_, scriptedMonster,
   simpleTownsperson, scriptedTownsperson,
   signRadius)
where

import Control.Monad (unless, void)

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Script
  (addDeviceOnMarks, demandOneTerrainMark, setTerrain)
import Fallback.State.Area
import Fallback.State.Creature (Monster(..), MonsterTownAI, makeMonster)
import Fallback.State.Progress (VarSeed, splitVarSeed)
import Fallback.State.Resources (SoundTag(..), rsrcTileset)
import Fallback.State.Simple (CharacterNumber)
import Fallback.State.Tags (MonsterTag)
import Fallback.State.Tileset (TileTag(..), tilesetGet, ttId)
import Fallback.State.Terrain (terrainGetTile)
import Fallback.Utility (firstJust, maybeM, whenM)

-------------------------------------------------------------------------------

data Globals = Globals
  { gTimesThroughLongvale :: Var Int,
    gUnlockedDoor :: Device }

compileGlobals :: CompileScenario Globals
compileGlobals = do
  timesThroughLongvale <- newGlobalVar 092343 0
  unlockedDoor <- do
    let succeed _ _ = return True
    newDoorDevice 978249 succeed succeed
  return Globals { gTimesThroughLongvale = timesThroughLongvale,
                   gUnlockedDoor = unlockedDoor }

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
      let toggleTile = setTerrain other $ prectPositions $ Grid.geRect ge
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

-- | Create a device with the given interaction radius and script function, and
-- place it at the position marked in the terrain with the given string; there
-- must be exactly one such position.
uniqueDevice :: VarSeed -> String -> Int
             -> (Grid.Entry Device -> CharacterNumber -> Script AreaEffect ())
             -> CompileArea ()
uniqueDevice vseed key radius sfn = do
  (vseed', vseed'') <- splitVarSeed vseed
  device <- newDevice vseed' radius sfn
  onStartDaily vseed'' $ do
    addDevice_ device =<< demandOneTerrainMark key

-------------------------------------------------------------------------------

-- TODO: deprecated (use simpleEnemy_ instead)
simpleMonster :: VarSeed -> MonsterTag -> String -> MonsterTownAI
              -> CompileArea ()
simpleMonster vseed tag mark ai = void $ simpleEnemy vseed mark tag ai

simpleEnemy :: VarSeed -> String -> MonsterTag -> MonsterTownAI
            -> CompileArea (Var Bool)
simpleEnemy vseed mark tag ai = do
  (vseed', vseed'') <- splitVarSeed vseed
  isDeadVar <- newPersistentVar vseed' False
  onStartDaily vseed'' $ do
    isDead <- readVar isDeadVar
    unless isDead $ do
      pos <- demandOneTerrainMark mark
      addBasicEnemyMonster pos tag (Just isDeadVar) ai
  return isDeadVar

simpleEnemy_ :: VarSeed -> String -> MonsterTag -> MonsterTownAI
             -> CompileArea ()
simpleEnemy_ vseed mark tag ai = void $ simpleEnemy vseed mark tag ai

scriptedMonster :: VarSeed -> String -> MonsterTag -> Bool -> MonsterTownAI
                -> CompileArea (Var (Grid.Key Monster), Var Bool)
scriptedMonster vseed mark tag ally townAi = do
  scriptedMonster' vseed mark (makeMonster tag)
    { monstIsAlly = ally, monstTownAI = townAi }

-- TODO:
--   immortalTownsperson vseed mark tag name ai sfn (but no isDeadVar)
--     Use this for most cities/towns; define in terms of immortalTownspersonIf
--   immortalTownspersonIf (as above, but takes predicate)
--     Use this for Holmgare, and for e.g. taverngoers that are only around
--     in certain cases.
--   mortalTownsperson (same as above, but has and returns isDeadVar)
--     Use this for NPCs that can actually die.
--   scriptedTownsperson (returns isDeadVar and keyVar)
--     Use this for NPCs that we need to control from triggers.

simpleTownsperson :: VarSeed -> MonsterTag -> String -> MonsterTownAI
                  -> (Grid.Entry Monster -> Script TownEffect ())
                  -> CompileArea ()
simpleTownsperson vseed tag key ai sfn = do
  (vseed', vseed'') <- splitVarSeed vseed
  scriptId <- newMonsterScript vseed' sfn
  onStartDaily vseed'' $ do
    pos <- demandOneTerrainMark key
    void $ tryAddMonster pos (makeMonster tag)
      { monstIsAlly = True,
        monstScript = Just scriptId,
        monstTownAI = ai }

scriptedTownsperson :: VarSeed -> String -> MonsterTag -> MonsterTownAI
                    -> (Grid.Entry Monster -> Script TownEffect ())
                    -> CompileArea (Var (Grid.Key Monster), Var Bool)
scriptedTownsperson vseed mark tag townAi scriptFn = do
  (vseed', vseed'') <- splitVarSeed vseed
  scriptId <- newMonsterScript vseed' scriptFn
  scriptedMonster' vseed'' mark (makeMonster tag)
    { monstIsAlly = True, monstScript = Just scriptId, monstTownAI = townAi }

scriptedMonster' :: VarSeed -> String -> Monster
                 -> CompileArea (Var (Grid.Key Monster), Var Bool)
scriptedMonster' vseed mark monst = do
  (vseed', vseed'') <- splitVarSeed vseed
  isDeadVar <- newPersistentVar vseed' False
  keyVar <- newTransientVar vseed'' $ do
    isDead <- readVar isDeadVar
    if isDead then return Grid.nilKey else do
      pos <- demandOneTerrainMark mark
      mbEntry <- tryAddMonster pos monst { monstDeadVar = Just isDeadVar }
      maybe (fail $ "Failed to place " ++ show (monstTag monst) ++
             " at " ++ show mark) (return . Grid.geKey) mbEntry
  return (keyVar, isDeadVar)

-------------------------------------------------------------------------------

-- | The standard interaction radius for signs/placards:
signRadius :: Int
signRadius = 3

-------------------------------------------------------------------------------
