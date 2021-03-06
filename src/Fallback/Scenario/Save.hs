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
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Time (formatTime, getCurrentTime)
import Data.Traversable (traverse)
import System.Directory
  (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.FilePath (combine)
import System.Locale (defaultTimeLocale)
import Text.Read

import Fallback.Control.Error (IOEO, onlyIO)
import Fallback.Data.Clock (initClock)
import Fallback.Data.Point (Position)
import Fallback.Draw (Sprite, loadSprite)
import Fallback.Resource (getGameDataDir, readFromFile, saveToFile)
import Fallback.Scenario.Triggers
  (getAreaDevice, getAreaTriggers, scenarioTriggers)
import Fallback.State.Area (AreaCommonState(..), arsCurrentArea, devId)
import Fallback.State.Camera (makeCameraWithCenter)
import Fallback.State.Doodad (emptyDoodads)
import Fallback.State.Minimap (newMinimapFromTerrain)
import Fallback.State.Party (Party(..), partyExploredMap)
import Fallback.State.Region (RegionState(..))
import Fallback.State.Resources (Resources, rsrcTileset)
import Fallback.State.Tags (areaName, regionName)
import Fallback.State.Terrain
  (Terrain(..), loadTerrainMap, positionCenter, tmapName, ttId)
import Fallback.State.Tileset (tilesetLookup)
import Fallback.State.Town (TownPhase(WalkingPhase), TownState(..))
import Fallback.State.Trigger (distillTriggers, reconstituteTriggers)
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
savedGameLocation (SavedTownState ts) = areaName $ arsCurrentArea ts

-------------------------------------------------------------------------------

getSaveDirectory :: IO FilePath
getSaveDirectory = do
  dataDir <- getGameDataDir
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
    mbInfo <- readFromFile (combine path "summary")
    case mbInfo of
      Just (name, loc, time) -> do
        --screenshot <- loadSprite (combine path "screenshot.png")
        screenshot <- loadSprite "/Users/mdsteele/Projects/fallback/data/images/characters.png" -- FIXME
        return $ Just SavedGameSummary
          { sgsDirPath = path,
            sgsLocation = loc,
            sgsName = name,
            sgsScreenshot = screenshot,
            sgsTimeSaved = time }
      Nothing -> return Nothing

loadSavedGame :: Resources -> SavedGameSummary -> IOEO SavedGame
loadSavedGame resources sgs = do
  result <- onlyIO $ readFromFile (combine (sgsDirPath sgs) "state")
  case result of
    Nothing -> fail "Failed to parse save data."
    Just (Left wrappedRegionState) ->
      return $ SavedRegionState $ unwrapRegionState wrappedRegionState
    Just (Right wrappedTownState) -> do
      ts <- unwrapTownState wrappedTownState resources
      return $ SavedTownState ts

saveGame :: String -> Sprite -> SavedGame -> IOEO SavedGameSummary
saveGame name screenshot savedGame = do
  dirPath <- onlyIO $ do
    summaries <- loadSavedGameSummaries
    maybe allocSaveDir (return . sgsDirPath) $
      find ((name ==) . sgsName) summaries
  let loc = savedGameLocation savedGame
  time <- formatTime defaultTimeLocale "%-d %b %Y %-I:%M %p" <$>
          onlyIO getCurrentTime
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

newtype ReadRegionState = ReadRegionState { unwrapRegionState :: RegionState }

instance Read ReadRegionState where
  readPrec = do
    (party, region, selectedArea, prevArea) <- readPrec
    return $ ReadRegionState $ RegionState
      { rsClock = initClock,
        rsParty = party,
        rsPreviousArea = prevArea,
        rsRegion = region,
        rsSelectedArea = selectedArea,
        rsUnsaved = False }

newtype ShowRegionState = ShowRegionState RegionState

instance Show ShowRegionState where
  showsPrec p (ShowRegionState rs) = showsPrec p $
    (rsParty rs, rsRegion rs, rsSelectedArea rs, rsPreviousArea rs)

-------------------------------------------------------------------------------

newtype ReadTownState = ReadTownState
  { unwrapTownState :: Resources -> IOEO TownState }

instance Read ReadTownState where
  readPrec = do
    (activeCharacter, wrappedCommon, partyPose, partyPosition,
     triggersFiredIds) <- readPrec
    return $ ReadTownState $ \resources -> do
      acs <- unwrapAreaCommonState wrappedCommon resources partyPosition
      return TownState
        { tsActiveCharacter = activeCharacter,
          tsCommon = acs,
          tsPartyPose = partyPose,
          tsPartyPosition = partyPosition,
          tsPhase = WalkingPhase,
          tsTriggers = reconstituteTriggers triggersFiredIds $
                       getAreaTriggers scenarioTriggers $
                       partyCurrentArea $ acsParty acs }

newtype ShowTownState = ShowTownState TownState

instance Show ShowTownState where
  showsPrec p (ShowTownState ts) = showsPrec p $
    (tsActiveCharacter ts, ShowAreaCommonState (tsCommon ts),
     tsPartyPose ts, tsPartyPosition ts, distillTriggers (tsTriggers ts))

-------------------------------------------------------------------------------

newtype ReadAreaCommonState = ReadAreaCommonState
  { unwrapAreaCommonState :: Resources -> Position -> IOEO AreaCommonState }

instance Read ReadAreaCommonState where
  readPrec = do
    (deviceIds, fields, monsters, party, remains, wrappedTerrain) <- readPrec
    return $ ReadAreaCommonState $ \resources partyPosition -> do
      let getDevice di =
            maybe (fail $ "Unknown device ID: " ++ show di) return $
            getAreaDevice scenarioTriggers (partyCurrentArea party) di
      devices <- traverse getDevice deviceIds
      terrain <- unwrapTerrain wrappedTerrain resources
      minimap <- onlyIO $ newMinimapFromTerrain terrain $
                 partyExploredMap terrain party
      return AreaCommonState
        { acsCamera = makeCameraWithCenter (positionCenter partyPosition),
          acsClock = initClock,
          acsDevices = devices,
          acsDoodads = emptyDoodads,
          acsFields = fields,
          acsMessage = Nothing,
          acsMinimap = minimap,
          acsMonsters = monsters,
          acsParty = party,
          acsRemains = remains,
          acsResources = resources,
          acsTerrain = terrain,
          acsVisible = Set.empty }

newtype ShowAreaCommonState = ShowAreaCommonState AreaCommonState

instance Show ShowAreaCommonState where
  showsPrec p (ShowAreaCommonState acs) = showsPrec p $
    (fmap devId (acsDevices acs), acsFields acs, acsMonsters acs, acsParty acs,
     acsRemains acs, ShowTerrain (acsTerrain acs))

-------------------------------------------------------------------------------

newtype ReadTerrain = ReadTerrain
  { unwrapTerrain :: Resources -> IOEO Terrain }

instance Read ReadTerrain where
  readPrec = do
    (terrainName, overrideIds) <- readPrec
    return $ ReadTerrain $ \resources -> do
      tmap <- loadTerrainMap resources terrainName
      let getTile ti =
            maybe (fail $ "Unknown terrain tile ID: " ++ show ti) return $
            tilesetLookup ti $ rsrcTileset resources
      overrides <- traverse getTile overrideIds
      return Terrain { terrainMap = tmap, terrainOverrides = overrides }

newtype ShowTerrain = ShowTerrain Terrain

instance Show ShowTerrain where
  showsPrec p (ShowTerrain terrain) = showsPrec p $
    (tmapName (terrainMap terrain), ttId <$> terrainOverrides terrain)

-------------------------------------------------------------------------------
