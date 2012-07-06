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

module Fallback.Preferences
  (Preferences(..), loadPreferencesFromDisk, getPreferences, setPreferences)
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import System.FilePath (combine)
import System.IO.Unsafe (unsafePerformIO)

import Fallback.Control.Error (runEO, runIOEO)
import Fallback.Resource (getGameDataDir, readFromFile, saveToFile)

-------------------------------------------------------------------------------

data Preferences = Preferences
  { prefEnableMusic :: Bool,
    prefEnableSound :: Bool,
    prefFullscreen :: Bool }
  deriving (Read, Show)

-- | Load and set the global preferences from disk.
loadPreferencesFromDisk :: IO Preferences
loadPreferencesFromDisk = do
  prefsFile <- getPrefsFilePath
  -- TODO use parseFromFile/weaveBracesCommas/meshKeyDefault here
  mbPreferences <- readFromFile prefsFile
  let prefs = fromMaybe defaultPreferences mbPreferences
  writeIORef globalPrefs prefs
  return prefs

-- | Get the current global preferences.  If neither 'loadPreferencesFromDisk'
-- nor 'setPreferences' has yet been called, this will simply return the
-- default preferences.
getPreferences :: IO Preferences
getPreferences = readIORef globalPrefs

-- | Set the global preferences, and also save them to disk.
setPreferences :: Preferences -> IO ()
setPreferences prefs = do
  writeIORef globalPrefs prefs
  prefsFile <- getPrefsFilePath
  eo <- runIOEO $ saveToFile prefsFile (shows prefs)
  case runEO eo of
    Left errors -> mapM_ putStrLn errors
    Right () -> return ()

-------------------------------------------------------------------------------

{-# NOINLINE globalPrefs #-} -- needed for unsafePerformIO
globalPrefs :: IORef Preferences
globalPrefs = unsafePerformIO $ newIORef defaultPreferences

defaultPreferences :: Preferences
defaultPreferences = Preferences
  { prefEnableMusic = True,
    prefEnableSound = True,
    prefFullscreen = False }

getPrefsFilePath :: IO FilePath
getPrefsFilePath = do
  dataDir <- getGameDataDir
  return $ combine dataDir "preferences"

-------------------------------------------------------------------------------
