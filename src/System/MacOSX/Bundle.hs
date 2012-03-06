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

{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module System.MacOSX.Bundle
  (--Bundle, getMainBundle, bundleResourcesDirectory, bundleResourcePath,
   tryGetResourcePath)
where

import Data.Word (Word8)
import Foreign.C (CChar, CLong, peekCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal (allocaArray)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import System.FilePath (combine)

-------------------------------------------------------------------------------

data BundleStruct
type Bundle = Ptr BundleStruct

-------------------------------------------------------------------------------

getMainBundle :: IO (Maybe Bundle)
getMainBundle = do
  ptr <- cfBundleGetMainBundle
  return $ if ptr == nullPtr then Nothing else Just ptr

foreign import ccall unsafe "CFBundleGetMainBundle"
  cfBundleGetMainBundle :: IO Bundle

-------------------------------------------------------------------------------

bundleResourcesDirectory :: Bundle -> IO (Maybe FilePath)
bundleResourcesDirectory bundle = do
  ptr <- cfBundleCopyResourcesDirectoryUrl bundle
  if ptr == nullPtr then return Nothing else do
    url <- makeCfForeignPtr ptr
    withForeignPtr url $ \urlPtr -> do
      let bufferSize = 1000 -- Ugh, C programmer's disease.
      allocaArray bufferSize $ \buffer -> do
        success <- cfUrlGetFileSystemRepresentation
                     urlPtr True buffer (fromIntegral bufferSize)
        if not success then return Nothing else do
          -- Note the type ascriptions below, to verify that castPtr is safe.
          path <- peekCString (castPtr (buffer :: Ptr Word8) :: Ptr CChar)
          return $ Just path

data UrlStruct

foreign import ccall unsafe "CFBundleCopyResourcesDirectoryURL"
  cfBundleCopyResourcesDirectoryUrl :: Bundle -> IO (Ptr UrlStruct)

foreign import ccall unsafe "CFURLGetFileSystemRepresentation"
  cfUrlGetFileSystemRepresentation :: Ptr UrlStruct -> Bool -> Ptr Word8
                                   -> CLong -> IO Bool

-------------------------------------------------------------------------------

bundleResourcePath :: Bundle -> FilePath -> IO (Maybe FilePath)
bundleResourcePath bundle relativePath = do
  mbDir <- bundleResourcesDirectory bundle
  return $ fmap (`combine` relativePath) mbDir

tryGetResourcePath :: FilePath -> IO (Maybe FilePath)
tryGetResourcePath relativePath = do
  mbBundle <- getMainBundle
  case mbBundle of
    Nothing -> return Nothing
    Just bundle -> bundleResourcePath bundle relativePath

-------------------------------------------------------------------------------
-- Private utility functions:

makeCfForeignPtr :: Ptr a -> IO (ForeignPtr a)
makeCfForeignPtr = newForeignPtr cfRelease

foreign import ccall unsafe "&CFRelease"
  cfRelease :: FunPtr (Ptr a -> IO ())

-------------------------------------------------------------------------------
