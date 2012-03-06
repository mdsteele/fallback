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

module Fallback.Sound
  (Sound, loadSound, playSound, playSounds,
   {-SoundDone, playSoundNotify, soundDone,-}
   loopMusic, stopMusic, fadeOutMusic)
where

--import qualified Control.Concurrent.SampleVar as SV
--import qualified Data.HashTable as HT
import Data.IORef (IORef, newIORef, writeIORef)
import Data.List (nub)
--import qualified Graphics.UI.SDL.Extras as SDLx
import qualified Graphics.UI.SDL.Mixer as SDLm
import System.IO.Unsafe (unsafePerformIO)

import Fallback.Resource (getResourcePath)
--import Fallback.Utility (maybeM)

-------------------------------------------------------------------------------

newtype Sound = Sound SDLm.Chunk deriving Eq

loadSound :: String -> IO Sound
loadSound name = fmap Sound (getResourcePath "sounds" name >>= SDLm.loadWAV)

playSound :: Sound -> IO ()
playSound (Sound chunk) = SDLm.tryPlayChannel (-1) chunk 0 >> return ()

playSounds :: [Sound] -> IO ()
playSounds = mapM_ playSound . nub

-------------------------------------------------------------------------------

{-# NOINLINE currentMusic #-} -- needed for unsafePerformIO
currentMusic :: IORef (Maybe SDLm.Music)
currentMusic = unsafePerformIO $ newIORef Nothing

loopMusic :: String -> IO ()
loopMusic name = do
  music <- SDLm.loadMUS =<< getResourcePath "music" name
  writeIORef currentMusic (Just music)
  SDLm.playMusic music (negate 1)

-- | Fade out the currently playing music over the given number of seconds, or
-- do nothing if no music is currently playing.
fadeOutMusic :: Double -> IO ()
fadeOutMusic seconds = do
  -- We use SDLm.tryFadeOutMusic instead of SDLm.fadeOutMusic because the
  -- latter apparently throws an exception if the music is not currently
  -- playing.
  _ <- SDLm.tryFadeOutMusic $ round $ 1000 * seconds
  return ()

stopMusic :: IO ()
stopMusic = do
  SDLm.haltMusic
  writeIORef currentMusic Nothing

-------------------------------------------------------------------------------
{-
newtype SoundDone = SoundDone (SV.SampleVar ())

playSoundNotify :: Sound -> IO SoundDone
playSoundNotify (Sound chunk) = do
  chan <- SDLm.tryPlayChannel (-1) chunk 0
  if chan < 0 then fmap SoundDone (SV.newSampleVar ()) else do
    done <- fmap SoundDone SV.newEmptySampleVar
    HT.insert doneTable chan done
    return done

soundDone :: SoundDone -> IO Bool
soundDone (SoundDone sv) = fmap not (SV.isEmptySampleVar sv)

{-# NOINLINE doneTable #-} -- needed for unsafePerformIO
doneTable :: HT.HashTable SDLm.Channel SoundDone
doneTable = unsafePerformIO $ do
  SDLx.setChannelFinishedAction $ Just $ \chan -> do
    mbDone <- HT.lookup doneTable chan
    maybeM mbDone $ \(SoundDone sv) -> do
      SV.writeSampleVar sv ()
      HT.delete doneTable chan
  HT.new (==) HT.hashInt
-}
-------------------------------------------------------------------------------
