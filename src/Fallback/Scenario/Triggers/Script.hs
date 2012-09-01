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

{-# LANGUAGE GADTs, KindSignatures, ScopedTypeVariables #-}

-- | This module contains 'Script' functions relevant to scripting area
-- triggers.
module Fallback.Scenario.Triggers.Script
  (-- * Conversation
   TalkEffect(..), conversation, convText, convChoice, convNode, convReset,
   -- * Terrain
   setTerrain, resetTerrain,
   lookupTerrainMark, demandOneTerrainMark, isOnTerrainMark, demandTerrainRect,
   -- * Miscellaneous
   addDeviceOnMarks, doesPartyHaveItem, playDoorUnlockSound, setAreaCleared,
   setLevelCap, setQuestStatus, startBossFight, startScriptedBattle,
   startShopping)
where

import qualified Data.Set as Set (member, toList)

import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Party (partyHasItem)
import Fallback.State.Progress (BattleId)
import Fallback.State.Resources (SoundTag(SndUnlock), rsrcTileset)
import Fallback.State.Simple (Ingredient, MarkKey, QuestStatus, RectKey)
import Fallback.State.Tags (AreaTag, ItemTag, QuestTag)
import Fallback.State.Terrain
  (terrainMap, tmapGet, tmapLookupMark, tmapLookupRect)
import Fallback.State.Tileset (TileTag, tilesetGet)

-------------------------------------------------------------------------------
-- Conversation:

data TalkEffect :: (* -> *) -> * -> * where
  EffTalkLift :: f a -> TalkEffect f a
  EffAddText :: String -> TalkEffect f ()
  EffAddChoice :: String -> Script (TalkEffect f) () -> TalkEffect f ()
  EffTalkNode :: TalkEffect f ()
  EffTalkReset :: TalkEffect f ()

instance (FromAreaEffect f) => FromAreaEffect (TalkEffect f) where
  fromAreaEffect = EffTalkLift . fromAreaEffect
  isWaitEffect (EffTalkLift eff) fn =
    case isWaitEffect eff return of
      Just script -> Just (mapEffect EffTalkLift script >>= fn)
      Nothing -> Nothing
  isWaitEffect _ _ = Nothing

instance (FromTownEffect f) => FromTownEffect (TalkEffect f) where
  fromTownEffect = EffTalkLift . fromTownEffect

conversation :: forall f a. (FromAreaEffect f) => Script (TalkEffect f) a
             -> Script f a
conversation = doTalk initState where
  doTalk :: (String, [(String, Script (TalkEffect f) ())])
         -> Script (TalkEffect f) a -> Script f a
  doTalk state@(text, choices) talkScript =
    case execScript talkScript of
      ResultFinal value -> return value
      ResultEffect eff sfn ->
        case eff of
          EffTalkLift eff' -> emitEffect eff' >>= doTalk state . sfn
          EffAddText text' -> doTalk (text ++ text', choices) $ sfn ()
          EffAddChoice str action ->
            doTalk (text, (str, action) : choices) $ sfn ()
          EffTalkNode -> do
            if null choices then do
              narrate text
              doTalk initState $ sfn ()
            else do
              -- TODO: this is kinda ugly
              idx <- forcedChoice text $ zip (map fst choices) [0..]
              let (before, (_, action) : after) = splitAt idx choices
              doTalk ("", before ++ after) (action >>= sfn)
          EffTalkReset -> doTalk initState $ sfn ()
      ResultFailure message -> fail message
  initState = ("", [])

convText :: (FromAreaEffect f) => String -> Script (TalkEffect f) ()
convText = emitEffect . EffAddText

convChoice :: (FromAreaEffect f) => Script (TalkEffect f) () -> String
           -> Script (TalkEffect f) ()
convChoice action string = emitEffect $ EffAddChoice string action

convNode :: Script (TalkEffect f) () -> Script (TalkEffect f) ()
convNode = (>> emitEffect EffTalkNode)

convReset :: Script (TalkEffect f) ()
convReset = emitEffect EffTalkReset

-------------------------------------------------------------------------------
-- Terrain:

setTerrain :: (FromAreaEffect f) => TileTag -> [Position] -> Script f ()
setTerrain tileTag positions = do
  tile <- areaGet (tilesetGet tileTag . rsrcTileset . arsResources)
  emitAreaEffect $ EffSetTerrain $ map (flip (,) tile) positions

resetTerrain :: (FromAreaEffect f) => [Position] -> Script f ()
resetTerrain positions = do
  tmap <- areaGet (terrainMap . arsTerrain)
  let update pos = (pos, tmapGet tmap pos)
  emitAreaEffect $ EffSetTerrain $ map update positions

lookupTerrainMark :: (FromAreaEffect f) => MarkKey -> Script f [Position]
lookupTerrainMark key =
  areaGet (Set.toList . tmapLookupMark key . terrainMap . arsTerrain)

demandOneTerrainMark :: (FromAreaEffect f) => MarkKey -> Script f Position
demandOneTerrainMark key = do
  positions <- lookupTerrainMark key
  case positions of
    [pos] -> return pos
    _ -> fail ("demandOneTerrainMark: " ++ show key ++ " yields " ++
               show positions)

isOnTerrainMark :: (FromAreaEffect f) => MarkKey -> Position -> Script f Bool
isOnTerrainMark key pos =
  areaGet (Set.member pos . tmapLookupMark key . terrainMap . arsTerrain)

lookupTerrainRect :: (FromAreaEffect f) => RectKey -> Script f (Maybe PRect)
lookupTerrainRect key = areaGet (tmapLookupRect key . terrainMap . arsTerrain)

demandTerrainRect :: (FromAreaEffect f) => RectKey -> Script f PRect
demandTerrainRect key =
  maybe (fail $ "demandTerrainRect: no such rect: " ++ show key) return =<<
  lookupTerrainRect key

-------------------------------------------------------------------------------
-- Miscellaneous:

-- | Add a device to all positions marked in the terrain with the given string.
addDeviceOnMarks :: (FromAreaEffect f) => Device -> String -> Script f ()
addDeviceOnMarks dev key = mapM_ (addDevice_ dev) =<< lookupTerrainMark key

doesPartyHaveItem :: (FromAreaEffect f) => ItemTag -> Script f Bool
doesPartyHaveItem tag = areaGet (partyHasItem tag . arsParty)

-- | Play the sound used for unlocking doors, and wait a short bit for the
-- sound to complete.  This is generally called just before returning from a
-- @tryOpen@ script passed to @newDoorDevices@.
playDoorUnlockSound :: (FromAreaEffect f) => Script f ()
playDoorUnlockSound = playSound SndUnlock >> wait 6

setAreaCleared :: (FromAreaEffect f) => AreaTag -> Bool -> Script f ()
setAreaCleared tag cleared = emitAreaEffect $ EffSetAreaCleared tag cleared

setLevelCap :: (FromAreaEffect f) => Int -> Script f ()
setLevelCap cap = do
  emitAreaEffect $ EffSetLevelCap cap
  grantExperience 0 -- tickle level up, if we were previously maxed out

setQuestStatus :: (FromAreaEffect f) => QuestTag -> QuestStatus -> Script f ()
setQuestStatus tag status = emitAreaEffect $ EffSetQuestStatus tag status

-- | Start combat, with running away disallowed, and with the combat arena
-- centered on the specified terrain rect.
startBossFight :: (FromTownEffect f) => String -> Script f ()
startBossFight key = do
  rect <- demandTerrainRect key
  emitTownEffect $ EffStartCombat False rect

startScriptedBattle :: (FromTownEffect f) => BattleId -> Script f ()
startScriptedBattle = emitTownEffect . EffStartScriptedBattle

startShopping :: (FromTownEffect f) => [Either Ingredient ItemTag]
              -> Script f ()
startShopping = emitTownEffect . EffShop

-------------------------------------------------------------------------------
