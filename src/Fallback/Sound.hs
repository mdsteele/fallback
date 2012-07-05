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
   loopMusic, stopMusic, fadeOutMusic)
where

import Control.Monad (void, when)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.List (nub)
import qualified Graphics.UI.SDL.Mixer as SDLm
import System.IO.Unsafe (unsafePerformIO)

import Fallback.Resource (getResourcePath)
import Fallback.Preferences (getPreferences, prefEnableSound)

-------------------------------------------------------------------------------

newtype Sound = Sound SDLm.Chunk deriving Eq

loadSound :: String -> IO Sound
loadSound name = fmap Sound (getResourcePath "sounds" name >>= SDLm.loadWAV)

playSound :: Sound -> IO ()
playSound (Sound chunk) = do
  prefs <- getPreferences
  when (prefEnableSound prefs) $ do
    SDLm.tryPlayChannel (-1) chunk 0 >> return ()

playSounds :: [Sound] -> IO ()
playSounds = mapM_ playSound . nub

-------------------------------------------------------------------------------

-- We store the currently playing music object in a global ref, so that it
-- isn't garbage-collected while the music is playing (which it otherwise might
-- be, which would be bad).
{-# NOINLINE currentMusic #-} -- needed for unsafePerformIO
currentMusic :: IORef (Maybe SDLm.Music)
currentMusic = unsafePerformIO $ newIORef Nothing

loopMusic :: String -> IO ()
loopMusic name = do
  -- TODO: Pay attention to prefEnableMusic here.  Problem: what to do when the
  -- user turns music back on?  We should probably keep track of what music we
  -- _would_ be playing, and start it at that moment.
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
  void $ SDLm.tryFadeOutMusic $ round $ 1000 * seconds

stopMusic :: IO ()
stopMusic = do
  SDLm.haltMusic
  writeIORef currentMusic Nothing

-------------------------------------------------------------------------------
