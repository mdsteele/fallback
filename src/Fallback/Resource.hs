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

module Fallback.Resource
  (tryGetResourcePath, getResourcePath,
   getGameDataDir, readFromFile, parseFromFile, saveToFile)
where

import Control.Exception (IOException, evaluate, handle)
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import qualified System.FilePath as FilePath (combine)
import System.IO (IOMode(ReadMode), hGetContents, withFile)
import qualified System.MacOSX.Bundle as Bundle (tryGetResourcePath)

import Fallback.Control.Error (EO, IOEO, onlyEO, onlyIO)
import Fallback.Control.Parse (Parser, parseRead, tryParse)

-------------------------------------------------------------------------------

-- | Return the path of a game resource with the given name in the given
-- directory.  Return an error on failure.
tryGetResourcePath :: String {-^directory-} -> String {-^filename-}
                   -> IOEO FilePath
tryGetResourcePath dir name =
  fmap (maybe (fail "getResourcePath failed") id) $
  onlyIO $ Bundle.tryGetResourcePath (FilePath.combine dir name)

-- | Return the path of a game resource with the given name in the given
-- directory.  Throw an exception on failure.
getResourcePath :: String {-^directory-} -> String {-^filename-}
                -> IO FilePath
getResourcePath dir name =
  fmap (maybe (fail "getResourcePath failed") id) $
  Bundle.tryGetResourcePath (FilePath.combine dir name)

-------------------------------------------------------------------------------

-- | Get the path to the directory containing persistant data for the game, and
-- create the directory if it doesn't already exist.
getGameDataDir :: IO FilePath
getGameDataDir = do
  dataDir <- getAppUserDataDirectory "fallback-save-data"
  createDirectoryIfMissing True dataDir
  return dataDir

-- | Attempt to load the contents of a file as a string.  Return 'Nothing' if
-- the file doesn't exist or isn't readable.
loadFromFile :: FilePath -> IO (Maybe String)
loadFromFile filepath = do
  let handler :: IOException -> IO (Maybe a)
      handler _ = return Nothing
  handle handler $ do
    withFile filepath ReadMode $ \fd -> do
      string <- hGetContents fd
      _ <- evaluate (length string) -- force entire file into memory
      return (Just string)

-- | Attempt to load and parse data from a file.  Return 'Nothing' if the file
-- doesn't exist, isn't readable, or contains unparsable data.
readFromFile :: (Read a) => FilePath -> IO (Maybe a)
readFromFile filepath = parseFromFile filepath parseRead

-- | Attempt to load and parse data from a file.  Return 'Nothing' if the file
-- doesn't exist, isn't readable, or contains unparsable data.
parseFromFile :: FilePath -> Parser a -> IO (Maybe a)
parseFromFile filepath parser = do
  mbString <- loadFromFile filepath
  return (tryParse parser =<< mbString)

-- | Save string data to a file, overwriting the file if it already exists.
-- Fail with an error if the file isn't writable.
saveToFile :: FilePath -> ShowS -> IOEO ()
saveToFile filepath writer = do
  let handler :: IOException -> IO (EO ())
      handler err = return $ fail $ show err
  eo <- onlyIO $ handle handler $ do
    writeFile filepath (writer "")
    return $ return ()
  onlyEO eo

-------------------------------------------------------------------------------
