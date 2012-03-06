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

module Fallback.Scenario.Save
  (SavedGameSummary(..), SavedGame(..), savedGameLocation,
   loadSavedGameSummaries, loadSavedGame, saveGame)
where

import Control.Applicative ((<$>))
import Control.Monad (filterM, forM)
import Data.Char (isAlphaNum)
import Data.List (find)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set
import Data.Time (formatTime, getCurrentTime)
import Data.Traversable (traverse)
import System.Directory
import System.FilePath (combine)
import System.Locale (defaultTimeLocale)
import Text.Read

import Fallback.Control.Error (IOEO, onlyIO)
import Fallback.Data.Clock (initClock)
import Fallback.Draw (Sprite, loadSprite, runDraw)
import Fallback.Scenario.Areas (createMinimap)
import Fallback.Scenario.Triggers
  (getAreaDevice, getAreaTriggers, scenarioTriggers)
import Fallback.State.Area (devId, emptyDoodads, triggerId)
import Fallback.State.Camera (makeCameraWithCenter)
import Fallback.State.Party (Party(..))
import Fallback.State.Region (RegionState(..))
import Fallback.State.Resources (Resources)
import Fallback.State.Tags (areaName, regionName)
import Fallback.State.Terrain (loadTerrainMap, positionCenter, tmapName)
import Fallback.State.Town (TownPhase(WalkingPhase), TownState(..), tsParty)
import Fallback.Utility (sortKey)

-------------------------------------------------------------------------------

data SavedGameSummary = SavedGameSummary
  { sgsDirPath :: FilePath,
    sgsLocation :: String,
    sgsName :: String,
    sgsScreenshot :: Sprite,
    sgsTimeSaved :: String }

data SavedGame = SavedRegionState RegionState
               | SavedTownState TownState

-- | Get the location string for a saved game; that is, the name of the area or
-- region that the party is currently in.
savedGameLocation :: SavedGame -> String
savedGameLocation (SavedRegionState rs) = regionName $ rsRegion rs
savedGameLocation (SavedTownState ts) =
  areaName $ partyCurrentArea $ tsParty ts

-------------------------------------------------------------------------------

loadFromString :: ReadS a -> String -> Maybe a
loadFromString reader string = fmap fst $ listToMaybe $ reader string

loadFromFile :: ReadS a -> FilePath -> IO (Maybe a)
loadFromFile reader filepath = loadFromString reader <$> readFile filepath

saveToFile :: FilePath -> ShowS -> IO ()
saveToFile filepath writer = writeFile filepath (writer "")

getSaveDirectory :: IO FilePath
getSaveDirectory = do
  dataDir <- getAppUserDataDirectory "fallback-save-data"
  let saveDir = combine dataDir "saved-games"
  createDirectoryIfMissing True saveDir
  return saveDir

-------------------------------------------------------------------------------

loadSavedGameSummaries :: IO [SavedGameSummary]
loadSavedGameSummaries = do
  saveDir <- getSaveDirectory
  entries <- filterM doesDirectoryExist =<< map (combine saveDir) .
             filter (all isAlphaNum) <$> getDirectoryContents saveDir
  fmap (sortKey sgsName . catMaybes) $ forM entries $ \path -> do
    mbInfo <- loadFromFile reads (combine path "summary")
    case mbInfo of
      Just (name, loc, time) -> do
        --screenshot <- runDraw $ loadSprite (combine path "screenshot.png")
        screenshot <- runDraw $ loadSprite "/Users/mdsteele/Projects/fallback/data/images/characters.png" -- FIXME
        return $ Just SavedGameSummary
          { sgsDirPath = path,
            sgsLocation = loc,
            sgsName = name,
            sgsScreenshot = screenshot,
            sgsTimeSaved = time }
      Nothing -> return Nothing

loadSavedGame :: Resources -> SavedGameSummary -> IOEO SavedGame
loadSavedGame resources sgs = do
  result <- onlyIO $ loadFromFile reads (combine (sgsDirPath sgs) "state")
  case result of
    Nothing -> fail "Failed to parse save data."
    Just (Left w) ->
      return $ SavedRegionState $ unwrapRegionState w resources
    Just (Right w) -> do
      ts <- unwrapTownState w resources
      return $ SavedTownState ts

saveGame :: String -> Sprite -> SavedGame -> IOEO SavedGameSummary
saveGame name screenshot savedGame = onlyIO $ do
  summaries <- loadSavedGameSummaries
  dirPath <- maybe allocSaveDir (return . sgsDirPath) $
             find ((name ==) . sgsName) summaries
  let loc = savedGameLocation savedGame
  time <- formatTime defaultTimeLocale "%-d %b %Y %-I:%M %p" <$> getCurrentTime
  saveToFile (combine dirPath "screenshot.png") $ shows () -- FIXME
  saveToFile (combine dirPath "summary") $ shows (name, loc, time)
  saveToFile (combine dirPath "state") $ shows $
             case savedGame of
               SavedRegionState rs -> Left $ ShowRegionState rs
               SavedTownState ts -> Right $ ShowTownState ts
  let summary = SavedGameSummary
        { sgsDirPath = dirPath,
          sgsLocation = loc,
          sgsName = name,
          sgsScreenshot = screenshot,
          sgsTimeSaved = time }
  return summary

allocSaveDir :: IO FilePath
allocSaveDir = do
  saveDir <- getSaveDirectory
  entries <- getDirectoryContents saveDir
  let dirPath = combine saveDir $ show $
                until (flip notElem entries . show) (1+) (0 :: Int)
  createDirectoryIfMissing True dirPath
  return dirPath

-------------------------------------------------------------------------------

newtype ReadRegionState = ReadRegionState
  { unwrapRegionState :: Resources -> RegionState }

instance Read ReadRegionState where
  readPrec = do
    (party, region, selectedArea, prevArea) <- readPrec
    return $ ReadRegionState $ \resources -> RegionState
      { rsClock = initClock,
        rsParty = unwrapParty party resources,
        rsPreviousArea = prevArea,
        rsRegion = region,
        rsSelectedArea = selectedArea }

newtype ShowRegionState = ShowRegionState RegionState

instance Show ShowRegionState where
  showsPrec p (ShowRegionState rs) = showsPrec p $
    (ShowParty (rsParty rs), rsRegion rs, rsSelectedArea rs, rsPreviousArea rs)

-------------------------------------------------------------------------------

newtype ReadTownState = ReadTownState
  { unwrapTownState :: Resources -> IOEO TownState }

instance Read ReadTownState where
  readPrec = do
    (activeCharacter, deviceIds, fields, wrappedParty, partyAnim,
     partyFaceDir, partyPosition, terrainName, triggersFiredIds) <- readPrec
    return $ ReadTownState $ \resources -> do
      let party = unwrapParty wrappedParty resources
      let getDevice di =
            maybe (fail $ "Unknown device ID: " ++ show di) return $
            getAreaDevice scenarioTriggers (partyCurrentArea party) di
      devices <- traverse getDevice deviceIds
      terrain <- loadTerrainMap resources terrainName
      minimap <- onlyIO $ createMinimap terrain party
      let triggers = getAreaTriggers scenarioTriggers (partyCurrentArea party)
      return TownState
        { tsActiveCharacter = activeCharacter,
          tsCamera = makeCameraWithCenter (positionCenter partyPosition),
          tsClock = initClock,
          tsDevices = devices,
          tsDoodads = emptyDoodads,
          tsFields = fields,
          tsMessage = Nothing,
          tsMinimap = minimap,
          tsMonsters = error "FIXME",
          tsParty = party,
          tsPartyAnim = partyAnim,
          tsPartyPosition = partyPosition,
          tsPartyFaceDir = partyFaceDir,
          tsPhase = WalkingPhase,
          tsTerrain = terrain,
          tsTriggersFired =
            filter (flip Set.member triggersFiredIds . triggerId) triggers,
          tsTriggersReady =
            filter (flip Set.notMember triggersFiredIds . triggerId) triggers,
          tsVisible = Set.empty }

newtype ShowTownState = ShowTownState TownState

instance Show ShowTownState where
  showsPrec p (ShowTownState ts) = showsPrec p $
    (tsActiveCharacter ts, fmap devId (tsDevices ts), tsFields ts,
     ShowParty (tsParty ts), tsPartyAnim ts, tsPartyFaceDir ts,
     tsPartyPosition ts, tmapName (tsTerrain ts),
     Set.fromList $ map triggerId $ tsTriggersFired ts)

-------------------------------------------------------------------------------

newtype ReadParty = ReadParty { unwrapParty :: Resources -> Party }

instance Read ReadParty where
  readPrec = do
    (characters, clearedAreas, coins, currentArea, difficulty, experience,
     exploredMaps, foundAreas, ingredients, items, level, progress) <- readPrec
    return $ ReadParty $ \resources -> Party
      { partyCharacters = characters,
        partyClearedAreas = clearedAreas,
        partyCoins = coins,
        partyCurrentArea = currentArea,
        partyDifficulty = difficulty,
        partyExperience = experience,
        partyExploredMaps = exploredMaps,
        partyFoundAreas = foundAreas,
        partyIngredients = ingredients,
        partyItems = items,
        partyLevel = level,
        partyProgress = progress,
        partyResources = resources }

newtype ShowParty = ShowParty Party

instance Show ShowParty where
  showsPrec p (ShowParty party) = showsPrec p $
    (partyCharacters party, partyCoins party,
     partyClearedAreas party, partyCurrentArea party, partyDifficulty party,
     partyExperience party, partyExploredMaps party, partyFoundAreas party,
     partyIngredients party, partyItems party, partyLevel party,
     partyProgress party)

-------------------------------------------------------------------------------
{-
newtype ReadCharacter = ReadCharacter { unwrapCharacter :: Character }

instance Read ReadCharacter where
  readPrec = do
    (abilities, adrenaline, appearance, baseStats, cclass, equipment, health,
     mana, name, skillPoints, statPoints, status) <- readPrec
    return $ ReadCharacter $ Character
      { chrAbilities = abilities,
        chrAdrenaline = adrenaline,
        chrAppearance = appearance,
        chrBaseStats = baseStats,
        chrClass = cclass,
        chrEquipment = equipment,
        chrHealth = health,
        chrMana = mana,
        chrName = name,
        chrSkillPoints = skillPoints,
        chrStatPoints = statPoints,
        chrStatus = status }

newtype ShowCharacter = ShowCharacter Character

instance Show ShowCharacter where
  showsPrec p (ShowCharacter char) = showsPrec p $
    (chrAbilities char, chrAdrenaline char, chrAppearance char,
     chrBaseStats char, chrClass char, chrEquipment char, chrHealth char,
     chrMana char, chrName char, chrSkillPoints char, chrStatPoints char,
     chrStatus char)
-}
-------------------------------------------------------------------------------
