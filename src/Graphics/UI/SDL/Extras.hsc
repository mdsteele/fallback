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

{-# LANGUAGE ForeignFunctionInterface #-}

#include <SDL/SDL.h>
#ifdef main
#undef main
#endif

module Graphics.UI.SDL.Extras
  (getKeyState, pixelFormatGetRmask, pixelFormatGetGmask, pixelFormatGetBmask,
   pixelFormatGetAmask, glSetVsyncEnabled, setChannelFinishedAction)
where

import Control.Monad (when)
import Foreign (FunPtr, Ptr, Storable(peekByteOff, peekElemOff), Word8, Word32,
                nullFunPtr, nullPtr, withForeignPtr)
import Foreign.C.Types (CInt)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as SDLm
import qualified Graphics.UI.SDL.Utilities as SDLu

-------------------------------------------------------------------------------

-- | Return 'True' if the given key is currently pressed, 'False' otherwise.
getKeyState :: SDL.SDLKey -> IO Bool
getKeyState key = fmap (0 /=) $ sdlGetKeyState nullPtr >>=
                  (flip peekElemOff $ fromIntegral $ SDLu.fromEnum key)

-- Uint8 *SDL_GetKeyState(int *numkeys);
foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState ::
  Ptr CInt -> IO (Ptr Word8)

-------------------------------------------------------------------------------

pixelFormatGetRmask :: SDL.PixelFormat -> IO Word32
pixelFormatGetRmask format =
  withForeignPtr format #{peek SDL_PixelFormat, Rmask}

pixelFormatGetGmask :: SDL.PixelFormat -> IO Word32
pixelFormatGetGmask format =
  withForeignPtr format #{peek SDL_PixelFormat, Gmask}

pixelFormatGetBmask :: SDL.PixelFormat -> IO Word32
pixelFormatGetBmask format =
  withForeignPtr format #{peek SDL_PixelFormat, Bmask}

pixelFormatGetAmask :: SDL.PixelFormat -> IO Word32
pixelFormatGetAmask format =
  withForeignPtr format #{peek SDL_PixelFormat, Amask}

-------------------------------------------------------------------------------

-- | Attempt to enable/disable VSYNC for OpenGL.
glSetVsyncEnabled :: Bool -> IO ()
#if SDL_VERSION_ATLEAST(1,3,0)
glSetVsyncEnabled enabled =
  do status <- sdlGlSetSwapInterval $ if enabled then 1 else 0
     when (status /= 0) $ do
       putStrLn "glSetVsyncEnabled: SDL_GL_SetSwapInteral failed"
foreign import ccall unsafe "SDL_GL_SetSwapInteral" sdlGlSetSwapInterval ::
  CInt -> IO CInt
#elif SDL_VERSION_ATLEAST(1,2,10)
glSetVsyncEnabled enabled =
  do ok <- SDL.tryGLSetAttribute #{const SDL_GL_SWAP_CONTROL} $
           if enabled then 1 else 0
     when (not ok) $ putStrLn "glSetVsyncEnabled: SDL_GL_SetAttribute failed"
#else
glSetVsyncEnabled _ =
  putStrLn "glSetVsyncEnabled: failed because SDL version is too low"
#endif

-------------------------------------------------------------------------------

-- | Set a callback to be called each time a channel finishes playing a sound.
setChannelFinishedAction :: Maybe (SDLm.Channel -> IO ()) -> IO ()
setChannelFinishedAction mbFn = do
  wrapped <- maybe (return nullFunPtr)
                   (\fn -> wrapChannelFinished (fn . fromIntegral)) mbFn
  sdlMixChannelFinished wrapped

-- void Mix_ChannelFinished(void (*channel_finished)(int channel));
foreign import ccall "Mix_ChannelFinished"
  sdlMixChannelFinished :: FunPtr (CInt -> IO ()) -> IO ()
foreign import ccall "wrapper"
  wrapChannelFinished :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))

-------------------------------------------------------------------------------
