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
  (Texture, textureWidth, textureHeight, newTexture, withTexture,
   preservingTextures)
where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)
import qualified Data.HashTable as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes)
import Data.Unique (Unique, hashUnique, newUnique)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (Weak, deRefWeak, mkWeak)

import Fallback.Utility (delayFinalizers)

-------------------------------------------------------------------------------

-- | Represents OpenGL texture data.
data Texture = Texture
  { textureName :: !(IORef GL.TextureObject),
    textureWidth :: !Int,
    textureHeight :: !Int }

-- | Allocate a new 'Texture' object of the given size, using the given action
-- to populate its OpenGL texture data.  This will do the work of allocating an
-- OpenGL texture name, and ensuring that that texture name is delete when the
-- 'Texture' object is garbage-collected.
newTexture :: Int {-^width-} -> Int {-^height-} -> IO () -> IO Texture
newTexture width height action = do
  ref <- do
    [texName] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just texName
    newIORef texName
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  action
  key <- newUnique
  let texture = Texture { textureName = ref, textureWidth = width,
                          textureHeight = height }
  weak <- mkWeak ref texture $ Just $ do
    HT.delete weakTextureSet key
    texName <- readIORef (textureName texture)
    GL.deleteObjectNames [texName]
  HT.insert weakTextureSet key weak
  return texture

-- | Perform an action with the given texture bound to OpenGL, and ensure that
-- the texture is not garbage-collected while the action is still running.
withTexture :: Texture -> IO a -> IO a
withTexture texture action = delayFinalizers (textureName texture) $ do
  texName <- readIORef (textureName texture)
  GL.textureBinding GL.Texture2D $= Just texName
  action

-- | Copy all texture data into temporary memory, perform the given action,
-- then restore all the texture data.  This allows 'Texture' objects to remain
-- valid across operations that wipe out the current OpenGL context
-- (e.g. switching between windowed and fullscreen mode).
preservingTextures :: IO a -> IO a
preservingTextures action = do
  textures <- do
    weaks <- map snd <$> HT.toList weakTextureSet
    catMaybes <$> mapM deRefWeak weaks
  textureData <- forM textures $ \texture -> do
    dataPtr <- mallocBytes (4 * textureWidth texture * textureHeight texture)
    withTexture texture $ do
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      GL.getTexImage (Left GL.Texture2D) 0
                     (GL.PixelData GL.RGBA GL.UnsignedByte dataPtr)
    GL.deleteObjectNames . (:[]) =<< readIORef (textureName texture)
    return (texture, dataPtr)
  result <- action
  forM_ textureData $ \(texture, dataPtr) -> do
    [texName] <- GL.genObjectNames 1
    writeIORef (textureName texture) texName
    withTexture texture $ do
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA'
          (GL.TextureSize2D (fromIntegral $ textureWidth texture)
                            (fromIntegral $ textureHeight texture))
          0 (GL.PixelData GL.RGBA GL.UnsignedByte dataPtr)
    free dataPtr
  return result

{-# NOINLINE weakTextureSet #-} -- needed for unsafePerformIO
weakTextureSet :: HT.HashTable Unique (Weak Texture)
weakTextureSet = unsafePerformIO $ HT.new (==) (HT.hashInt . hashUnique)

-------------------------------------------------------------------------------
