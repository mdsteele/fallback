{- ============================================================================
| Copyright 2010 Matthew D. Steele <mdsteele@alum.mit.edu>                    |
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

module Fallback.Draw.Base
  (-- * Setting up the screen
   initializeScreen,
   -- * The Draw monad
   Draw, runDraw, paintDraw, debugDraw,
   -- * Reference cells
   DrawRef, newDrawRef, readDrawRef, writeDrawRef, modifyDrawRef,
   -- * The Paint monad
   Paint, paintScreen,
   Canvas, canvasWidth, canvasHeight, canvasSize, canvasRect,
   withSubCanvas,
   -- * Textures
   Texture, loadTexture,
   -- * Sprites
   Sprite, makeSprite, makeSubSprite, Strip, Sheet,
   -- ** Creating
   takeScreenshot,
   -- ** Loading
   loadSprite, loadSubSprite, loadVStrip, loadSheet,
   -- ** Querying
   spriteWidth, spriteHeight, spriteSize, stripLength, (!),
   -- ** Blitting
   blitTopleft, blitLoc, blitLocTinted, blitStretch, blitStretchTinted,
   blitRepeat, blitRepeatTinted, blitRotate, blitRotateTinted,
   -- * Geometric primitives
   tintRect, tintCanvas, drawLine, drawRect,
   drawLineChain, drawPolygon, tintPolygon, gradientPolygon, tintRing,
   -- * Fonts and text
   Font, loadFont, drawText, {-renderText,-} textRenderSize, textRenderWidth,
   -- * Minimaps
   Minimap, minimapMapSize, minimapBlitSize,
   newMinimap, alterMinimap, blitMinimap, minimapScale)
where

import Control.Applicative ((<$>), (<*>), Applicative, pure)
import Control.Arrow ((&&&), (***))
import Control.Monad ((<=<), forM_, when)
import Data.Array ((!), Array, bounds, listArray, range)
import qualified Data.HashTable as HT
import Data.Int (Int32)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word32)
import Foreign (allocaBytes, pokeElemOff, withArray, withForeignPtr)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Extras as SDLx
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.TTF as SDLt
import System.IO.Unsafe (unsafePerformIO)

import Fallback.Constants (screenHeight, screenRect, screenWidth)
import Fallback.Data.Color (Color(Color), Tint(Tint), whiteTint)
import Fallback.Data.Point
import Fallback.Draw.Texture
import Fallback.Resource (getResourcePath)
import Fallback.Utility (ceilDiv, flip3)

-------------------------------------------------------------------------------

debugResources :: Bool
debugResources = False

-------------------------------------------------------------------------------
-- Setting up the screen:

initializeScreen :: Bool -> IO ()
initializeScreen fullscreen = do
  SDL.glSetAttribute SDL.glDoubleBuffer 1
  SDLx.glSetVsyncEnabled True
  _ <- SDL.setVideoMode screenWidth screenHeight 32 $ SDL.OpenGL :
                        (if fullscreen then [SDL.Fullscreen] else [])
  -- Turn off the depth buffer:
  GL.depthFunc $= Nothing
  GL.depthMask $= GL.Disabled
  -- Set up blending:
  GL.blend $= GL.Enabled
  GL.blendEquation $= GL.FuncAdd
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  -- Turn on antialiasing:
  GL.lineSmooth $= GL.Enabled
  GL.pointSmooth $= GL.Enabled
  GL.polygonSmooth $= GL.Enabled
  -- Enable textures:
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureFunction $= GL.Modulate
  -- Set the view:
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth)
                                           (fromIntegral screenHeight))
  GL.ortho 0 (fromIntegral screenWidth) (fromIntegral screenHeight) 0 (-1) 1

-------------------------------------------------------------------------------
-- Sprites:

data Sprite = Sprite
  { spriteTexture :: !Texture,
    spriteTexRect :: !(Rect GL.GLdouble),
    spriteWidth :: !Int,
    spriteHeight :: !Int }

makeSprite :: Texture -> Sprite
makeSprite texture =
  Sprite { spriteTexture = texture, spriteTexRect = Rect 0 0 1 1,
           spriteWidth = textureWidth texture,
           spriteHeight = textureHeight texture }

makeSubSprite :: IRect -> Texture -> Sprite
makeSubSprite rect texture =
  let tw = toGLdouble (textureWidth texture)
      th = toGLdouble (textureHeight texture)
      Rect sx sy sw sh = toGLdouble <$> rect
  in Sprite { spriteTexture = texture,
              spriteTexRect = Rect (sx / tw) (sy / th) (sw / tw) (sh / th),
              spriteWidth = rectW rect, spriteHeight = rectH rect }

{-

data Sprite = Sprite
  { spriteTexture :: !GL.TextureObject,
    -- | Return the width of the 'Sprite', in pixels.
    spriteWidth :: !Int,
    -- | Return the height of the 'Sprite', in pixels.
    spriteHeight :: !Int }
-- -}


-- | Return the width and height of the 'Sprite', in pixels.
spriteSize :: Sprite -> (Int, Int)
spriteSize = spriteWidth &&& spriteHeight

-- subSprite :: Sprite -> IRect -> Draw z Sprite
-- subSprite sprite rect = newSprite (rectSize rect) $ do
--   blitTopleft sprite $ pNeg $ rectTopleft rect
-- subSprite sprite rect@(Rect x y w h) = Draw $ do
--   let { width = spriteWidth sprite; height = spriteHeight sprite }
--   when (x + w > width || y + h > height) $ do
--     fail ("subSprite: " ++ show rect ++ " outside of " ++ show (width, height))
--   allocaBytes (4 * width * height) $ \ptr -> do
--     GL.textureBinding GL.Texture2D $= Just (spriteTexture sprite)
--     delayFinalizers sprite $ do
--       GL.getTexImage (Left GL.Texture2D) 0
--                      (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
--     forM_ [0 .. h - 1] $ \row -> do
--       moveBytes (ptr `plusPtr` (4 * w * row))
--                 (ptr `plusPtr` (4 * x + 4 * width * (y + row)))
--                 (4 * w)
--     makeSpriteFromIO w h $ do
--       GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA'
--           (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
--           0 (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

type Strip = Array Int Sprite

stripLength :: Strip -> Int
stripLength = subtract 1 . snd . bounds

type Sheet = Array (Int, Int) Sprite

-------------------------------------------------------------------------------
-- The Draw monad:

newtype Draw a b = Draw { fromDraw :: IO b }

instance Applicative (Draw a) where
  pure = Draw . pure
  (Draw f) <*> (Draw g) = Draw (f <*> g)

instance Functor (Draw a) where
  fmap fn m = Draw (fmap fn (fromDraw m))

instance Monad (Draw a) where
  return = Draw . return
  draw >>= fn = Draw $ fromDraw draw >>= (fromDraw . fn)
  fail = Draw . fail

runDraw :: Draw () a -> IO a
runDraw = fromDraw

paintDraw :: Draw () a -> Paint a
paintDraw = Draw . fromDraw

debugDraw :: String -> Draw a ()
debugDraw = Draw . putStrLn

-------------------------------------------------------------------------------
-- The Paint monad:

data Canvas

type Paint a = Draw Canvas a

paintScreen :: Paint () -> IO ()
paintScreen draw = do
  GL.clear [GL.ColorBuffer]
  fromDraw draw
  GL.flush -- Are the flush and finish at all necessary?  I'm not sure.
  GL.finish
  SDL.glSwapBuffers

canvasWidth :: Paint Int
canvasWidth = fmap fst canvasSize

canvasHeight :: Paint Int
canvasHeight = fmap snd canvasSize

canvasSize :: Paint (Int, Int)
canvasSize = Draw $ do
  scissor <- GL.get GL.scissor
  return $ case scissor of
    Nothing -> (screenWidth, screenHeight)
    Just (_, GL.Size w h) -> (fromIntegral w, fromIntegral h)

canvasRect :: Paint IRect
canvasRect = do
  (width, height) <- canvasSize
  return $ Rect 0 0 width height

-------------------------------------------------------------------------------
-- Reference cells:

newtype DrawRef a = DrawRef (IORef a)

newDrawRef :: a -> Draw z (DrawRef a)
newDrawRef = Draw . fmap DrawRef . newIORef

readDrawRef :: DrawRef a -> Draw z a
readDrawRef (DrawRef ref) = Draw (readIORef ref)

writeDrawRef :: DrawRef a -> a -> Draw z ()
writeDrawRef (DrawRef ref) value = Draw (writeIORef ref value)

modifyDrawRef :: DrawRef a -> (a -> a) -> Draw z ()
modifyDrawRef (DrawRef ref) fn = Draw (modifyIORef ref fn)

-------------------------------------------------------------------------------
-- Creating new sprites:
{-
newSprite :: (Int, Int) -> Paint () -> Draw a Sprite
newSprite (width, height) paint = Draw $ do
  oldBuffer <- GL.get GL.drawBuffer
  let newBuffer = case oldBuffer of GL.AuxBuffer i -> GL.AuxBuffer (i + 1)
                                    _ -> GL.AuxBuffer 0
  GL.drawBuffer $= newBuffer
  GL.clear [GL.ColorBuffer]
  oldScissor <- GL.get GL.scissor
  GL.scissor $= Just (GL.Position 0 0,
                      GL.Size (fromIntegral width) (fromIntegral height))
  GL.preservingMatrix $ do
    -- See note [New Sprite] below for why we do this little dance here.
    GL.scale 1 (-1) (1 :: GL.GLdouble)
    GL.translate $ GL.Vector3 0 (negate $ toGLdouble screenHeight) 0
    oldZoom <- GL.get GL.pixelZoom
    GL.pixelZoom $= (1, 1)
    fromDraw paint
    GL.pixelZoom $= oldZoom
  GL.scissor $= oldScissor
  makeSpriteFromIO width height $ do
    GL.readBuffer $= newBuffer
    GL.copyTexImage2D Nothing 0 GL.RGBA' (GL.Position 0 0)
        (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0
    GL.drawBuffer $= oldBuffer
-}
-- Note [New Sprite]:
--   Unfortunately, GL.copyTexImage2D doesn't seem to let us specify that the
--   start position is the top-left rather than the bottom-right.  So, we need
--   to carefully scale and translate the screen so that we draw everything
--   upside-down, and then when we do the GL.copyTexImage2D, we pretend that
--   the start position really is the top-left, and everything comes out fine.
--   However, since we're drawing everything upside-down, we also need to
--   temporarily set the pixelZoom to (1, 1) rather than (1, -1), so that if we
--   call GL.drawPixels the rastered pixels will come out the right way.

-- | Make a copy of the current state of a region of the screen.
takeScreenshot :: IRect -> IO Sprite
takeScreenshot (Rect left top width height) =
  allocaBytes (width * height * 4) $ \pixelsPtr -> do
    let pixelData = GL.PixelData GL.RGB GL.UnsignedByte pixelsPtr
    GL.pixelZoom $= (1, 1)
    GL.readPixels (GL.Position (toGLint left) (toGLint (top + height)))
                  (GL.Size (fromIntegral width) (fromIntegral height))
                  pixelData
    fmap makeSprite $ newTexture width height $ do
      GL.texImage2D Nothing GL.NoProxy 0 GL.RGB'
          (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
          0 pixelData

-------------------------------------------------------------------------------
-- Blitting sprites:

blitTopleft :: (Axis a) => Sprite -> Point a -> Paint ()
blitTopleft = blitTopleftTinted whiteTint

blitTopleftTinted :: (Axis a) => Tint -> Sprite -> Point a -> Paint ()
blitTopleftTinted tint sprite (Point x y) =
  blitStretchTinted tint sprite $
  (Rect x y (fromIntegral $ spriteWidth sprite)
            (fromIntegral $ spriteHeight sprite))

blitLoc :: (Axis a) => Sprite -> LocSpec a -> Paint ()
blitLoc = blitLocTinted whiteTint

blitLocTinted :: (Axis a) => Tint -> Sprite -> LocSpec a -> Paint ()
blitLocTinted tint sprite loc =
  blitTopleftTinted tint sprite $ locTopleft loc $
  (fromIntegral *** fromIntegral) $ spriteSize sprite

blitStretch :: (Axis a) => Sprite -> Rect a -> Paint ()
blitStretch = blitStretchTinted whiteTint

blitStretchTinted :: (Axis a) => Tint -> Sprite -> Rect a -> Paint ()
blitStretchTinted tint sprite rect = Draw $ do
  withTexture (spriteTexture sprite) $ do
    setTint tint
    GL.renderPrimitive GL.Quads $ do
      let Rect tx ty tw th = spriteTexRect sprite
      let Rect rx ry rw rh = toGLdouble <$> rect
      GL.texCoord $ GL.TexCoord2 tx ty
      glVertex rx ry
      GL.texCoord $ GL.TexCoord2 (tx + tw) ty
      glVertex (rx + rw) ry
      GL.texCoord $ GL.TexCoord2 (tx + tw) (ty + th)
      glVertex (rx + rw) (ry + rh)
      GL.texCoord $ GL.TexCoord2 tx (ty + th)
      glVertex rx (ry + rh)

blitRotate :: (Axis a) => Sprite {-^sprite-} -> Point a {-^center-}
           -> Double {-^ angle (in radians) -} -> Paint ()
blitRotate = blitRotateTinted whiteTint

blitRotateTinted :: (Axis a) => Tint {-^tint-} -> Sprite {-^sprite-}
                 -> Point a {-^center-} -> Double {-^ angle (in radians) -}
                 -> Paint ()
blitRotateTinted tint sprite (Point cx cy) radians = Draw $ do
  withTexture (spriteTexture sprite) $ do
    setTint tint
    GL.preservingMatrix $ do
      GL.translate $ GL.Vector3 (toGLdouble cx) (toGLdouble cy) 0
      GL.rotate (toGLdouble (radians * (180 / pi)))
                (GL.Vector3 0 0 (1 :: GL.GLdouble))
      GL.renderPrimitive GL.Quads $ do
        let Rect tx ty tw th = spriteTexRect sprite
        let sw = toGLdouble (spriteWidth sprite) / 2
            sh = toGLdouble (spriteHeight sprite) / 2
        GL.texCoord $ GL.TexCoord2 tx ty
        glVertex (negate sw) (negate sh)
        GL.texCoord $ GL.TexCoord2 (tx + tw) ty
        glVertex sw (negate sh)
        GL.texCoord $ GL.TexCoord2 (tx + tw) (ty + th)
        glVertex sw sh
        GL.texCoord $ GL.TexCoord2 tx (ty + th)
        glVertex (negate sw) sh

blitRepeat :: Sprite -> IPoint -> IRect -> Paint ()
blitRepeat = blitRepeatTinted whiteTint

blitRepeatTinted :: Tint -> Sprite -> IPoint -> IRect -> Paint ()
blitRepeatTinted tint sprite (Point ox oy) rect = withSubCanvas rect $ do
  let width = spriteWidth sprite
      height = spriteHeight sprite
  let startCol = negate (ox `ceilDiv` width)
      startRow = negate (oy `ceilDiv` height)
  let endCol = (rectW rect - ox) `div` width
      endRow = (rectH rect - oy) `div` height
  forM_ [startRow .. endRow] $ \row -> do
    forM_ [startCol .. endCol] $ \col -> do
      blitTopleftTinted tint sprite $
          Point (ox + col * width) (oy + row * height)

{-
blitRepeatTinted :: (Axis a) => Tint -> Sprite -> Point a -> Rect a -> Paint ()
blitRepeatTinted tint sprite offset rect =
  let width = toGLdouble (spriteWidth sprite)
      height = toGLdouble (spriteHeight sprite)
      (Point ox oy) = fmap toGLdouble offset
      toRect = fmap toGLdouble rect
      texRect = Rect (negate ox / width) (negate oy / height)
                     (rectW toRect / width) (rectH toRect / height)
  in blitGeneralized sprite tint toRect texRect

blitGeneralized :: Sprite -> Tint -> Rect GL.GLdouble -> Rect GL.GLdouble
                -> Paint ()
blitGeneralized sprite tint (Rect rx ry rw rh) (Rect tx ty tw th) = do
  Draw $ delayFinalizers sprite $ do
    GL.textureBinding GL.Texture2D $= Just (spriteTexture sprite)
    setTint tint
    GL.renderPrimitive GL.Quads $ do
      GL.texCoord $ GL.TexCoord2 tx ty
      glVertex rx ry
      GL.texCoord $ GL.TexCoord2 (tx + tw) ty
      glVertex (rx + rw) ry
      GL.texCoord $ GL.TexCoord2 (tx + tw) (ty + th)
      glVertex (rx + rw) (ry + rh)
      GL.texCoord $ GL.TexCoord2 tx (ty + th)
      glVertex rx (ry + rh)

blitRotate :: (Axis a) => Sprite {-^sprite-} -> Point a {-^center-}
           -> Double {-^ angle (in radians) -} -> Paint ()
blitRotate sprite (Point cx cy) radians =
  Draw $ delayFinalizers sprite $ do
    GL.textureBinding GL.Texture2D $= Just (spriteTexture sprite)
    setTint whiteTint
    GL.preservingMatrix $ do
      GL.translate $ GL.Vector3 (toGLdouble cx) (toGLdouble cy) 0
      GL.rotate (realToFrac (radians * (180 / pi))) $
        GL.Vector3 0 0 (1 :: GL.GLfloat)
      GL.renderPrimitive GL.Quads $ do
        let sw = toGLdouble (spriteWidth sprite) / 2
            sh = toGLdouble (spriteHeight sprite) / 2
        GL.texCoord $ GL.TexCoord2 0 (0 :: GL.GLfloat)
        glVertex (negate sw) (negate sh)
        GL.texCoord $ GL.TexCoord2 1 (0 :: GL.GLfloat)
        glVertex sw (negate sh)
        GL.texCoord $ GL.TexCoord2 1 (1 :: GL.GLfloat)
        glVertex sw sh
        GL.texCoord $ GL.TexCoord2 0 (1 :: GL.GLfloat)
        glVertex (negate sw) sh
-}
-------------------------------------------------------------------------------
-- Geometric primitives:

-- | Draw an antialiased line onto the canvas.
drawLine :: (Axis a) => Tint {-^color-} -> Point a {-^start-}
         -> Point a {-^end-} -> Paint ()
drawLine tint start end = Draw $ do
  drawPrimitive GL.Lines tint $ pointVertex' start >> pointVertex' end

drawRect :: (Axis a) => Tint -> Rect a -> Paint ()
drawRect tint (Rect x y w h) = Draw $ do
  drawPrimitive GL.LineLoop tint $ do
    axisVertex' x y
    axisVertex' (x + w - 1) y
    axisVertex' (x + w - 1) (y + h - 1)
    axisVertex' x (y + h - 1)

-- | Tint a subrectangle of the canvas uniformly.
tintRect :: (Axis a) => Tint -> Rect a -> Paint ()
tintRect tint (Rect x y w h) = Draw $ do
  drawPrimitive GL.Quads tint $ do
    axisVertex x y
    axisVertex (x + w) y
    axisVertex (x + w) (y + h)
    axisVertex x (y + h)

-- | Tint the whole canvas uniformly.
tintCanvas :: Tint -> Paint ()
tintCanvas tint = canvasRect >>= tintRect tint

-- -- | Draw an antialiased circle onto the canvas.
-- drawCircle :: (Axis a) => Tint {-^color-} -> Point a {-^center-}
--            -> a {-^radius-} -> Paint ()
-- drawCircle tint (Point cx cy) radius = Draw $ do
--   GL.preservingMatrix $ do
--     GL.translate $ GL.Vector3 (toGLdouble cx) (toGLdouble cy) 0
--     let rad = toGLdouble radius
--         thetaStep = 2 / rad
--     drawPrimitive GL.LineLoop tint $ do
--       untilM 0 (>= 2 * pi) (+thetaStep) $ \theta -> do
--         GL.vertex $ GL.Vertex3 (rad * cos theta) (rad * sin theta) 0

-- -- | Draw an antialiased oval onto the canvas.
-- drawOval :: (Axis a) => Tint -> Rect a -> Paint ()
-- drawOval = strokeOval GL.LineLoop

-- tintOval :: (Axis a) => Tint -> Rect a -> Paint ()
-- tintOval = strokeOval GL.Polygon

-- strokeOval :: (Axis a) => GL.PrimitiveMode -> Tint -> Rect a -> Paint ()
-- strokeOval mode tint (Rect x y w h) = Draw $ when (w > 0 && h > 0) $ do
--   let hRad = toGLdouble w / 2
--       vRad = toGLdouble h / 2
--   let cx = toGLdouble x + hRad
--       cy = toGLdouble y + vRad
--   GL.preservingMatrix $ do
--     GL.translate $ GL.Vector3 cx cy 0
--     let thetaStep = 2 / max hRad vRad
--     drawPrimitive mode tint $ do
--       untilM 0 (>= 2 * pi) (+thetaStep) $ \theta -> do
--         GL.vertex $ GL.Vertex3 (hRad * cos theta) (vRad * sin theta) 0

-- drawTriangle :: Tint {-^color-} -> Point {-^vertex 1-}
--              -> Point {-^vertex 2-} -> Point {-^vertex 3-} -> Paint ()
-- drawTriangle tint point1 point2 point3 =
--   drawPolygon tint [point1, point2, point3]

-- drawFilledTriangle :: Tint {-^color-} -> Point {-^vertex 1-}
--                    -> Point {-^vertex 2-} -> Point {-^vertex 3-} -> Paint ()
-- drawFilledTriangle tint point1 point2 point3 = Draw $ do
--   drawPrimitive GL.Triangles tint $ do
--     pointVertex' point1
--     pointVertex' point2
--     pointVertex' point3

drawLineChain :: (Axis a) => Tint -> [Point a] -> Paint ()
drawLineChain tint points = Draw $ do
  drawPrimitive GL.LineStrip tint $ mapM_ pointVertex' points

drawPolygon :: (Axis a) => Tint -> [Point a] -> Paint ()
drawPolygon tint points = Draw $ do
  drawPrimitive GL.LineLoop tint $ mapM_ pointVertex' points

tintPolygon :: (Axis a) => Tint -> [Point a] -> Paint ()
tintPolygon tint points = Draw $ do
  drawPrimitive GL.Polygon tint $ mapM_ pointVertex' points

gradientPolygon :: (Axis a) => [(Tint, Point a)] -> Paint ()
gradientPolygon pointTints = Draw $ do
  GL.textureBinding GL.Texture2D $= Nothing
  let doVertex (tint, point) = setTint tint >> pointVertex point
  GL.renderPrimitive GL.Polygon $ mapM_ doVertex pointTints

tintRing :: (Axis a) => Tint -> a -> Point a -> a -> a -> Paint ()
tintRing tint thickness (Point cx cy) hRad vRad = Draw $ do
  GL.preservingMatrix $ do
    GL.translate $ GL.Vector3 (toGLdouble cx) (toGLdouble cy) 0
    drawPrimitive GL.TriangleStrip tint $ do
      let { hr = toGLdouble hRad; vr = toGLdouble vRad }
      let st = toGLdouble thickness / 2
      let { ihr = hr - st; ivr = vr - st; ohr = hr + st; ovr = vr + st }
      let step = toGLdouble (pi / 24 :: Double) -- TODO
      forM_ [0, (2 * step) .. 2 * (pi - step)] $ \theta -> do
        let theta' = theta + step
        GL.vertex $ GL.Vertex3 (ihr * cos theta) (ivr * sin theta) 0
        GL.vertex $ GL.Vertex3 (ohr * cos theta') (ovr * sin theta') 0
      GL.vertex $ GL.Vertex3 (ihr * cos 0) (ivr * sin 0) 0
      GL.vertex $ GL.Vertex3 (ohr * cos step) (ovr * sin step) 0

-------------------------------------------------------------------------------
-- Miscellaneous canvas functions:

withSubCanvas :: IRect -> Paint a -> Paint a
withSubCanvas rect draw = Draw $ GL.preservingMatrix $ do
  GL.translate $ GL.Vector3 (toGLdouble $ rectX rect)
                            (toGLdouble $ rectY rect) 0
  oldScissor <- GL.get GL.scissor
  let fromScissor (GL.Position x y, GL.Size w h) =
        Rect (fromIntegral x) (screenHeight - fromIntegral y - fromIntegral h)
             (fromIntegral w) (fromIntegral h)
  let oldRect = maybe screenRect fromScissor oldScissor
  let rect' = oldRect `rectIntersection` (rect `rectPlus` rectTopleft oldRect)
  GL.scissor $= Just
      (GL.Position (fromIntegral $ rectX rect')
                   (fromIntegral $ screenHeight - rectY rect' - rectH rect'),
       GL.Size (fromIntegral $ rectW rect') (fromIntegral $ rectH rect'))
  result <- fromDraw draw
  GL.scissor $= oldScissor
  return result

-------------------------------------------------------------------------------
-- Fonts and text:

newtype Font = Font SDLt.Font
  deriving (Eq)

-- | Draw text with the given font and color onto the screen at the specified
-- location.
drawText :: (Axis a) => Font -> Color -> LocSpec a -> String -> Paint ()
drawText font color spec string = Draw $ do
  surface <- renderText' font color string
  blitSurface 1 surface spec
{-
-- | Create a new sprite containing text with the given font and color.
renderText :: Font -> Color -> String -> Draw z Sprite
renderText font color string = Draw $ do
  renderText' font color string >>= makeSpriteFromSurface
-}
-- | Create a new SDL surface containing text with the given font and color.
renderText' :: Font -> Color -> String -> IO SDL.Surface
renderText' (Font font) color string =
  if null string then SDL.createRGBSurfaceEndian [SDL.SWSurface] 0 0 32
  else SDLt.renderTextBlended font string (toSDLColor color)

-- | Determine the width and height that a given string would have when
-- rendered in the given font.
textRenderSize :: Font -> String -> (Int, Int)
textRenderSize (Font font) str =
  -- Note: SDLt.textSize has no side effects, but is in the IO monad because
  -- SDL font objects are mutable.  However, our abstraction in this module
  -- prevents mutation of font objects, so in fact this function is
  -- referentially transparent.  Thus, the use of unsafePerformIO is justified.
  unsafePerformIO $ SDLt.textSize font str

-- | Determine the width that a given string would have when rendered in the
-- given font.
textRenderWidth :: Font -> String -> Int
textRenderWidth = (fst .) . textRenderSize

-------------------------------------------------------------------------------
-- Minimaps:
-- {-
-- | A 'Minimap' is a mutable image intended for drawing terrain minimaps.
newtype Minimap = Minimap Texture

-- | Return the original dimensions passed to 'newMinimap'.
minimapMapSize :: Minimap -> (Int, Int)
minimapMapSize (Minimap texture) =
  (textureWidth texture, textureHeight texture)

-- | Return the size of the image that will be drawn by 'blitMinimap'.
minimapBlitSize :: Minimap -> (Int, Int)
minimapBlitSize (Minimap texture) =
  (textureWidth texture * minimapScale, textureHeight texture * minimapScale)

-- | Create a new minimap for a map with the given dimensions, initially all
-- black.
newMinimap :: (Int, Int) -> IO Minimap
newMinimap (width, height) = do
  texture <- newTexture width height $ do
    withArray (replicate (width * height) (0 :: Word32)) $ \pixelsPtr -> do
      GL.texImage2D Nothing GL.NoProxy 0 GL.RGB'
          (GL.TextureSize2D (toGLint width) (toGLint height)) 0
          (GL.PixelData GL.RGB GL.UnsignedByte pixelsPtr)
  return (Minimap texture)

-- | Mutate the color of some of the map locations on the minimap.
alterMinimap :: Minimap -> [(IPoint, Color)] -> IO ()
alterMinimap (Minimap texture) pixels = withTexture texture $ do
  allocaBytes 4 $ \pixelPtr -> do
    pokeElemOff pixelPtr 3 255
    forM_ pixels $ \(Point x y, Color r g b) -> do
      pokeElemOff pixelPtr 0 r
      pokeElemOff pixelPtr 1 g
      pokeElemOff pixelPtr 2 b
      GL.texSubImage2D Nothing 0 (GL.TexturePosition2D (toGLint x) (toGLint y))
         (GL.TextureSize2D 1 1) (GL.PixelData GL.RGBA GL.UnsignedByte pixelPtr)

-- | Draw the minimap to the screen.
blitMinimap :: (Axis a) => Minimap -> LocSpec a -> Paint ()
blitMinimap mm@(Minimap texture) loc = Draw $ withTexture texture $ do
  setTint whiteTint
  GL.renderPrimitive GL.Quads $ do
    let (w, h) = minimapBlitSize mm
    let Rect rx ry rw rh = toGLdouble <$> locRect loc (fromIntegral w,
                                                       fromIntegral h)
    GL.texCoord $ GL.TexCoord2 0 (0 :: GL.GLdouble)
    glVertex rx ry
    GL.texCoord $ GL.TexCoord2 1 (0 :: GL.GLdouble)
    glVertex (rx + rw) ry
    GL.texCoord $ GL.TexCoord2 1 (1 :: GL.GLdouble)
    glVertex (rx + rw) (ry + rh)
    GL.texCoord $ GL.TexCoord2 0 (1 :: GL.GLdouble)
    glVertex rx (ry + rh)

-- -}

{-
-- | A 'Minimap' is a mutable image intended for drawing terrain minimaps.
newtype Minimap = Minimap SDL.Surface

-- | Return the original dimensions passed to 'newMinimap'.
minimapMapSize :: Minimap -> (Int, Int)
minimapMapSize (Minimap surface) =
  (SDL.surfaceGetWidth surface, SDL.surfaceGetHeight surface)

-- | Return the size of the image that will be drawn by 'blitMinimap'.
minimapBlitSize :: Minimap -> (Int, Int)
minimapBlitSize (Minimap surface) =
  (SDL.surfaceGetWidth surface * minimapScale,
   SDL.surfaceGetHeight surface * minimapScale)

-- | Create a new minimap for a map with the given dimensions, initially all
-- black.
newMinimap :: (Int, Int) -> IO Minimap
newMinimap (width, height) = Minimap <$>
  SDL.createRGBSurfaceEndian [SDL.SWSurface] width height 24

-- | Mutate the color of some of the map locations on the minimap.
alterMinimap :: Minimap -> [(IPoint, Color)] -> IO ()
alterMinimap (Minimap surface) pixels = do
  forM_ pixels $ \(Point x y, Color r g b) -> do
    pixel <- SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b
    SDL.fillRect surface (Just $ SDL.Rect x y 1 1) pixel

-- | Draw the minimap to the screen.
blitMinimap :: (Axis a) => Minimap -> LocSpec a -> Paint ()
blitMinimap (Minimap surface) spec = Draw $ do
  blitSurface minimapScale surface spec
-}

-- | The width/height of each minimap tile, in pixels.
minimapScale :: Int
minimapScale = 2

-------------------------------------------------------------------------------
-- Loading resources:

-- | Load a font of a particular size.
loadFont :: String -> Int -> IO Font
loadFont name size = do
  when debugResources $ do
    putStrLn ("loading font " ++ name ++ " " ++ show size)
  path <- getResourcePath "fonts" name
  fmap Font $ SDLt.openFont path size

{-
loadSprite :: FilePath -> Draw z Sprite
loadSprite = drawWeakCached HT.hashString $ \name -> Draw $ do
  when debugResources $ do
    putStrLn ("loading sprite " ++ name)
  loadSurface name >>= makeSpriteFromSurface

loadSubSprite :: FilePath -> IRect -> Draw z Sprite
loadSubSprite = curry $ drawWeakCached hash $ \(name, rect) -> Draw $ do
  when debugResources $ do
    putStrLn ("loading subsprite " ++ name ++ " " ++ show rect)
  loadSurface name >>= subSurface rect >>= makeSpriteFromSurface
  where hash (name, Rect x y w h) =
          HT.hashString name ## HT.hashInt x ## HT.hashInt y ##
          HT.hashInt w ## HT.hashInt h

loadVStrip :: FilePath -> Int -> Draw z Strip
loadVStrip = curry $ drawWeakCached hash $ \(name, size) -> Draw $ do
  when debugResources $ do
    putStrLn ("loading vstrip " ++ name ++ " " ++ show size)
  surface <- loadSurface name
  let (height, extra) = SDL.surfaceGetHeight surface `divMod` size
  when (extra /= 0) $ fail ("bad vstrip size " ++ show size ++ " for " ++ name)
  let width = SDL.surfaceGetWidth surface
  let slice n = subSurface (Rect 0 (n * height) width height) surface >>=
                makeSpriteFromSurface
  listArray (0, size - 1) <$> mapM slice [0 .. size - 1]
  where hash (name, size) = HT.hashString name ## HT.hashInt size

loadSheet :: FilePath -> (Int, Int) -> Draw z Sheet
loadSheet = curry $ drawWeakCached hash $ \(name, (rows, cols)) -> Draw $ do
  when debugResources $ do
    putStrLn ("loading sheet " ++ name ++ " " ++ show (rows, cols))
  surface <- loadSurface name
  let (height, extraH) = SDL.surfaceGetHeight surface `divMod` rows
  let (width, extraW) = SDL.surfaceGetWidth surface `divMod` cols
  when (extraH /= 0 || extraW /= 0) $ do
    fail ("bad sheet size " ++ show (rows, cols) ++ " for " ++ name)
  let slice (row, col) =
        subSurface (Rect (col * width) (row * height) width height) surface >>=
        makeSpriteFromSurface
  let bound = ((0, 0), (rows - 1, cols - 1))
  listArray bound <$> mapM slice (range bound)
  where hash (n, (w, h)) = HT.hashString n ## HT.hashInt w ## HT.hashInt h
-}

---------

loadSprite :: String -> Draw z Sprite
loadSprite name = Draw $ makeSprite <$> loadTexture name

loadSubSprite :: String -> IRect -> Draw z Sprite
loadSubSprite name rect = Draw $ makeSubSprite rect <$> loadTexture name

loadVStrip :: String -> Int -> Draw z Strip
loadVStrip name size = Draw $ do
  texture <- loadTexture name
  let (height, extra) = textureHeight texture `divMod` size
  when (extra /= 0) $ fail ("bad vstrip size " ++ show size ++ " for " ++ name)
  let width = textureWidth texture
  let slice n = makeSubSprite (Rect 0 (n * height) width height) texture
  return $ listArray (0, size - 1) $ map slice [0 .. size - 1]

loadSheet :: FilePath -> (Int, Int) -> Draw z Sheet
loadSheet name (rows, cols) = Draw $ do
  texture <- loadTexture name
  let (height, extraH) = textureHeight texture `divMod` rows
  let (width, extraW) = textureWidth texture `divMod` cols
  when (extraH /= 0 || extraW /= 0) $ do
    fail ("bad sheet size " ++ show (rows, cols) ++ " for " ++ name)
  let slice (row, col) =
        makeSubSprite (Rect (col * width) (row * height) width height) texture
  let bound = ((0, 0), (rows - 1, cols - 1))
  return $ listArray bound $ map slice (range bound)

-- | Load an image from disk as a 'Texture' object.
loadTexture :: String -> IO Texture
loadTexture = ioEverCached HT.hashString $ \name -> do
  when debugResources $ do
    putStrLn ("loading texture " ++ name)
  (newTextureFromSurface <=< loadSurface) name

--------------


-- | Load an image from disk as an SDL surface.
loadSurface :: String -> IO SDL.Surface
loadSurface name = do -- = ioWeakCached HT.hashString $ \name -> do
  when debugResources $ do
    putStrLn ("loading surface " ++ name)
  surface <- getResourcePath "images" name >>= SDLi.load
  _ <- SDL.setAlpha surface [] 255 -- Turn off the SDL.SrcAlpha flag, if set.
  return surface
{-
subSurface :: IRect -> SDL.Surface -> IO SDL.Surface
subSurface rect@(Rect x y w h) surface = do
  let width = SDL.surfaceGetWidth surface
      height = SDL.surfaceGetHeight surface
  when (x < 0 || y < 0 || w < 0 || h < 0 || x + w > width || y + h > height) $
    fail ("subSurface: " ++ show rect ++ " outside " ++ show (width, height))
  surface' <- SDL.createRGBSurfaceEndian [SDL.SWSurface] w h 32
  _ <- SDL.blitSurface surface (Just $ SDL.Rect x y w h) surface' Nothing
  return surface'

-- | Wrap an 'IO' function with a weak-value hash table cache.
{-# NOINLINE ioWeakCached #-} -- needed for unsafePerformIO
ioWeakCached :: (Eq a) => (a -> Int32) -> (a -> IO b) -> (a -> IO b)
ioWeakCached hash fn = unsafePerformIO $ do
  table <- HT.new (==) hash
  return $ \key -> do
    mbWeak <- HT.lookup table key
    mbValue <- maybe (return Nothing) deRefWeak mbWeak
    flip3 maybe return mbValue $ do
      value <- fn key
      weak <- mkWeakPtr value $ Just $ HT.delete table key
      HT.insert table key weak
      return value

-- | Wrap a 'Draw' function with a weak-value hash table cache.
drawWeakCached :: (Eq a) => (a -> Int32) -> (a -> Draw () b) -> (a -> Draw z b)
drawWeakCached hash fn = Draw . ioWeakCached hash (runDraw . fn)
-}
-- | Wrap an 'IO' function with a strong-value hash table cache.
{-# NOINLINE ioEverCached #-} -- needed for unsafePerformIO
ioEverCached :: (Eq a) => (a -> Int32) -> (a -> IO b) -> (a -> IO b)
ioEverCached hash fn = unsafePerformIO $ do
  table <- HT.new (==) hash
  return $ \key -> do
    mbValue <- HT.lookup table key
    flip3 maybe return mbValue $ do
      value <- fn key
      HT.insert table key value
      return value
{-
-- | Wrap a 'Draw' function with a strong-value hash table cache.
drawEverCached :: (Eq a) => (a -> Int32) -> (a -> Draw () b) -> (a -> Draw z b)
drawEverCached hash fn = Draw . ioEverCached hash (runDraw . fn)
-}
-------------------------------------------------------------------------------
-- Private utility functions:

-- | Blit an SDL surface to the screen.
blitSurface :: (Axis a) => Int -> SDL.Surface -> LocSpec a -> IO ()
blitSurface zoom surface spec = do
  (format, _) <- surfaceFormats surface
  let width = SDL.surfaceGetWidth surface
      height = SDL.surfaceGetHeight surface
  let (Point x y) = locTopleft spec (fromIntegral (zoom * width),
                                     fromIntegral (zoom * height))
  -- We draw "upside-down" relative to how GL normally draws things (i.e. we
  -- have y increasing downwards rather than upwards), so we negate the
  -- vertical zoom, so that when we use GL.drawPixels, the raster position will
  -- correspond to the top-left of the pixel array rather than the bottom left.
  GL.pixelZoom $= let z = fromIntegral zoom in (z, negate z)
  GL.rasterPos (GL.Vertex3 (toGLdouble x) (toGLdouble y) 0)
  GL.textureBinding GL.Texture2D $= Nothing
  pixelsPtr <- SDL.surfaceGetPixels surface
  GL.drawPixels (GL.Size (fromIntegral width) (fromIntegral height))
                (GL.PixelData format GL.UnsignedByte pixelsPtr)
{-
-- | Make a 'Sprite', given a size and an IO action that will populate a
--   freshly generated OpenGL texture (e.g. using @GL.texImage2D@).
makeSpriteFromIO :: Int -> Int -> IO () -> IO Sprite
makeSpriteFromIO width height action = do
  [texName] <- GL.genObjectNames 1
  when debugResources $ do
    putStrLn ("alloc tex " ++ show texName)
  GL.textureBinding GL.Texture2D $= Just texName
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  action
  let sprite = Sprite { spriteTexture = texName,
                        spriteWidth = width,
                        spriteHeight = height }
  addFinalizer sprite $ GL.deleteObjectNames [texName]
  return sprite

-- | Turn an SDL surface into a 'Sprite'.
makeSpriteFromSurface :: SDL.Surface -> IO Sprite
makeSpriteFromSurface surface = do
  (format, format') <- surfaceFormats surface
  let width = SDL.surfaceGetWidth surface
      height = SDL.surfaceGetHeight surface
  makeSpriteFromIO width height $ do
    withForeignPtr surface $ const $ do
      pixelsPtr <- SDL.surfaceGetPixels surface
      GL.texImage2D Nothing GL.NoProxy 0 format'
          (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
          0 (GL.PixelData format GL.UnsignedByte pixelsPtr)
-}
-- | Turn an SDL surface into a 'Texture'.
newTextureFromSurface :: SDL.Surface -> IO Texture
newTextureFromSurface surface = do
  (format, format') <- surfaceFormats surface
  let width = SDL.surfaceGetWidth surface
      height = SDL.surfaceGetHeight surface
  newTexture width height $ do
    withForeignPtr surface $ const $ do
      pixelsPtr <- SDL.surfaceGetPixels surface
      GL.texImage2D Nothing GL.NoProxy 0 format'
          (GL.TextureSize2D (toGLint width) (toGLint height))
          0 (GL.PixelData format GL.UnsignedByte pixelsPtr)

-- | Determine the appropriate OpenGL pixel formats to use when interpreting
-- the raw pixel data of the given SDL surface.
surfaceFormats :: SDL.Surface -> IO (GL.PixelFormat, GL.PixelInternalFormat)
surfaceFormats surface = do
  let pixelFormat = SDL.surfaceGetPixelFormat surface
  bmask <- SDLx.pixelFormatGetBmask pixelFormat
  let bgr = bmask == ntohl 0xff000000 -- Are we in BGR order or RGB order?
  numColors <- SDL.pixelFormatGetBytesPerPixel pixelFormat
  case numColors of
    4 -> return (if bgr then GL.BGRA else GL.RGBA, GL.RGBA')
    3 -> return (if bgr then GL.BGR  else GL.RGB,  GL.RGB')
    _ -> fail ("numColors = " ++ show numColors)

-- | Convert a big-endian word to native endianness.
foreign import ccall unsafe "netinet/in.h" ntohl :: Word32 -> Word32

setTint :: Tint -> IO ()
setTint (Tint r g b a) =
  GL.color $ GL.Color4 (fromWord r) (fromWord g) (fromWord b) (fromWord a)

fromWord :: Word8 -> GL.GLdouble
fromWord = (* recip 255) . fromIntegral

glVertex :: GL.GLdouble -> GL.GLdouble -> IO ()
glVertex x y = GL.vertex $ GL.Vertex3 x y 0

axisVertex :: (Axis a) => a -> a -> IO ()
axisVertex x y = GL.vertex $ GL.Vertex3 (toGLdouble x) (toGLdouble y) 0

axisVertex' :: (Axis a) => a -> a -> IO ()
axisVertex' x y =
  GL.vertex $ GL.Vertex3 (toGLdouble x + 0.5) (toGLdouble y + 0.5) 0

pointVertex :: (Axis a) => Point a -> IO ()
pointVertex (Point x y) = axisVertex x y

pointVertex' :: (Axis a) => Point a -> IO ()
pointVertex' (Point x y) = axisVertex' x y

toSDLColor :: Color -> SDL.Color
toSDLColor (Color r g b) = SDL.Color r g b

drawPrimitive :: GL.PrimitiveMode -> Tint -> IO () -> IO ()
drawPrimitive mode tint action = do
  GL.textureBinding GL.Texture2D $= Nothing
  setTint tint
  GL.renderPrimitive mode action

toGLdouble :: (Axis a) => a -> GL.GLdouble
toGLdouble = toFloating

toGLint :: Int -> GL.GLint
toGLint = fromIntegral
{-
-- | Combine two hash codes.
(##) :: Int32 -> Int32 -> Int32
a ## b = (a + b `shiftL` 5) `xor` b
-}
-------------------------------------------------------------------------------
