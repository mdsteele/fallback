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

module Fallback.Draw.Texture
  (Texture, textureWidth, textureHeight, newTexture, withTexture)
where

import Data.IORef (IORef, newIORef, readIORef)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import System.Mem.Weak (addFinalizer)

import Fallback.Utility (delayFinalizers)

-------------------------------------------------------------------------------

data Texture = Texture
  { textureName :: !(IORef GL.TextureObject),
    textureWidth :: !Int,
    textureHeight :: !Int }

newTexture :: Int -> Int -> IO () -> IO Texture
newTexture width height action = do
  [texName] <- GL.genObjectNames 1
  ref <- newIORef texName
  GL.textureBinding GL.Texture2D $= Just texName
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  action
  addFinalizer ref $ GL.deleteObjectNames [texName]
  return Texture { textureName = ref, textureWidth = width,
                   textureHeight = height }

withTexture :: Texture -> IO a -> IO a
withTexture texture action = delayFinalizers (textureName texture) $ do
  texName <- readIORef (textureName texture)
  GL.textureBinding GL.Texture2D $= Just texName
  action

-------------------------------------------------------------------------------
