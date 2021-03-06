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

{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Fallback.Draw.Base
  (-- * Setting up the screen
   initializeScreen, setFullscreen,
   -- * The Draw monad
   Draw, MonadDraw(..), debugDraw,
   -- * The Handler monad
   Handler, MonadHandler(..), handleScreen,
   canvasWidth, canvasHeight, canvasSize, canvasRect,
   -- * The Paint monad
   Paint, paintScreen,
   -- * Reference cells
   DrawRef, newDrawRef, readDrawRef, writeDrawRef, modifyDrawRef,
   -- * Keyboard/mouse input
   getKeyState, getMouseButtonState, getRelativeMousePos, withInputsSuppressed,
   -- * Textures
   Texture, loadTexture,
   -- * Sprites
   Sprite, makeSprite, makeSubSprite, Strip, Sheet,
   -- ** Creating
   takeScreenshot,
   -- ** Loading
   loadSprite, loadSubSprite, loadVStrip, loadSheet, loadSheetWithTileSize,
   -- ** Querying
   spriteWidth, spriteHeight, spriteSize, stripLength, (!),
   -- ** Blitting
   blitTopleft, blitTopleftTinted, blitLoc, blitLocTinted,
   blitStretch, blitStretchTinted, blitRepeat, blitRepeatTinted,
   blitRotate, blitRotateTinted,
   -- * Geometric primitives
   tintRect, tintCanvas, drawLine, drawRect,
   drawLineChain, drawPolygon, tintPolygon, gradientPolygon,
   tintRing, gradientRing, gradientFan,
   -- * Fonts and text
   Font, loadFont, drawText, {-renderText,-} textRenderSize, textRenderWidth,
   -- * Minimaps
   Minimap, minimapMapSize, minimapBlitSize,
   newMinimap, alterMinimap, blitMinimap, minimapScale)
where

import Control.Applicative ((<$>), (<*), Applicative)
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
import Fallback.Event (Key, getKeyStateIO, getMouseButtonStateIO)
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

setFullscreen :: Bool -> IO ()
setFullscreen = preservingTextures . initializeScreen

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

-- | Return the width and height of the 'Sprite', in pixels.
spriteSize :: Sprite -> (Int, Int)
spriteSize = spriteWidth &&& spriteHeight

type Strip = Array Int Sprite

stripLength :: Strip -> Int
stripLength = subtract 1 . snd . bounds

type Sheet = Array (Int, Int) Sprite

-------------------------------------------------------------------------------
-- The Draw monad:

newtype Draw a = Draw { fromDraw :: IO a }
  deriving (Applicative, Functor, Monad)

class (Applicative m, Monad m) => MonadDraw m where
  runDraw :: Draw a -> m a

instance MonadDraw Draw where runDraw = id
instance MonadDraw IO where runDraw = fromDraw

debugDraw :: (MonadDraw m) => String -> m ()
debugDraw = drawIO . putStrLn

-------------------------------------------------------------------------------
-- The Handler monad:

newtype Handler a = Handler { fromHandler :: IO a }
  deriving (Applicative, Functor, Monad, MonadDraw)

class (MonadDraw m) => MonadHandler m where
  runHandler :: Handler a -> m a
  withSubCanvas :: IRect -> m a -> m a

instance MonadHandler Handler where
  runHandler = id
  withSubCanvas subRect action = Handler $ do
    oldRect <- readIORef canvasRectRef
    let newRect = subRect `rectPlus` rectTopleft oldRect
    writeIORef canvasRectRef newRect
    fromHandler action <* writeIORef canvasRectRef oldRect

handleScreen :: Handler a -> IO a
handleScreen action = withAbsoluteCanvas screenRect (fromHandler action)

canvasWidth :: (MonadHandler m) => m Int
canvasWidth = handlerIO (rectW <$> readIORef canvasRectRef)

canvasHeight :: (MonadHandler m) => m Int
canvasHeight = handlerIO (rectH <$> readIORef canvasRectRef)

canvasSize :: (MonadHandler m) => m (Int, Int)
canvasSize = handlerIO (rectSize <$> readIORef canvasRectRef)

canvasRect :: (MonadHandler m) => m IRect
canvasRect = do
  (width, height) <- canvasSize
  return $ Rect 0 0 width height

{-# NOINLINE canvasRectRef #-} -- needed for unsafePerformIO
canvasRectRef :: IORef IRect
canvasRectRef = unsafePerformIO (newIORef screenRect)

withAbsoluteCanvas :: IRect -> IO a -> IO a
withAbsoluteCanvas newRect action = do
  oldRect <- readIORef canvasRectRef
  writeIORef canvasRectRef newRect
  action <* writeIORef canvasRectRef oldRect

-------------------------------------------------------------------------------
-- The Paint monad:

newtype Paint a = Paint { fromPaint :: IO a }
  deriving (Applicative, Functor, Monad, MonadDraw)

instance MonadHandler Paint where
  runHandler = Paint . fromHandler
  withSubCanvas = paintWithSubCanvas

paintWithSubCanvas :: IRect -> Paint a -> Paint a
paintWithSubCanvas subRect paint = Paint $ GL.preservingMatrix $ do
  let fromScissor (GL.Position x y, GL.Size w h) =
        Rect (fromIntegral x) (screenHeight - fromIntegral y - fromIntegral h)
             (fromIntegral w) (fromIntegral h)
  let toScissor (Rect x y w h) =
        (GL.Position (toGLint x) (toGLint (screenHeight - y - h)),
         GL.Size (toGLsizei w) (toGLsizei h))
  -- Change the canvas rect:
  oldRect <- readIORef canvasRectRef
  let newRect = subRect `rectPlus` rectTopleft oldRect
  writeIORef canvasRectRef newRect
  -- Change the GL coordinates and scissor:
  GL.translate $ GL.Vector3 (toGLdouble $ rectX subRect)
                            (toGLdouble $ rectY subRect) 0
  oldScissor <- GL.get GL.scissor
  let oldScissorRect = maybe screenRect fromScissor oldScissor
  let newScissorRect = oldScissorRect `rectIntersection` newRect
  GL.scissor $= Just (toScissor newScissorRect)
  -- Perform the inner paint:
  result <- fromPaint paint
  -- Reset to previous state before returning:
  GL.scissor $= oldScissor
  writeIORef canvasRectRef oldRect
  return result

paintScreen :: Paint () -> IO ()
paintScreen paint = do
  GL.clear [GL.ColorBuffer]
  withAbsoluteCanvas screenRect (fromPaint paint)
  GL.flush -- Are the flush and finish at all necessary?  I'm not sure.
  GL.finish
  SDL.glSwapBuffers

-------------------------------------------------------------------------------
-- Reference cells:

newtype DrawRef a = DrawRef (IORef a)

newDrawRef :: (MonadDraw m) => a -> m (DrawRef a)
newDrawRef = drawIO . fmap DrawRef . newIORef

readDrawRef :: (MonadDraw m) => DrawRef a -> m a
readDrawRef (DrawRef ref) = drawIO (readIORef ref)

writeDrawRef :: (MonadDraw m) => DrawRef a -> a -> m ()
writeDrawRef (DrawRef ref) value = drawIO (writeIORef ref value)

modifyDrawRef :: (MonadDraw m) => DrawRef a -> (a -> a) -> m ()
modifyDrawRef (DrawRef ref) fn = drawIO (modifyIORef ref fn)

-------------------------------------------------------------------------------
-- Keyboard/mouse input:

getKeyState :: (MonadHandler m) => Key -> m Bool
getKeyState key = handlerIO $ do
  suppressed <- readIORef inputsSuppressed
  if suppressed then return False else getKeyStateIO key

getMouseButtonState :: (MonadHandler m) => m Bool
getMouseButtonState = handlerIO $ do
  suppressed <- readIORef inputsSuppressed
  if suppressed then return False else getMouseButtonStateIO

getRelativeMousePos :: (MonadHandler m) => m (Maybe IPoint)
getRelativeMousePos = handlerIO $ do
  suppressed <- readIORef inputsSuppressed
  if suppressed then return Nothing else do
  (absoluteMouseX, absoluteMouseY, _) <- SDL.getMouseState
  rect <- readIORef canvasRectRef
  return $ Just $ Point (absoluteMouseX - rectX rect)
                        (absoluteMouseY - rectY rect)

withInputsSuppressed :: (MonadHandler m) => m a -> m a
withInputsSuppressed action = do
  suppressed <- handlerIO (readIORef inputsSuppressed <*
                           writeIORef inputsSuppressed True)
  action <* handlerIO (writeIORef inputsSuppressed suppressed)

{-# NOINLINE inputsSuppressed #-} -- needed for unsafePerformIO
inputsSuppressed :: IORef Bool
inputsSuppressed = unsafePerformIO (newIORef False)

-------------------------------------------------------------------------------
-- Creating new sprites:

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
blitStretchTinted tint sprite rect = Paint $ do
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
blitRotateTinted tint sprite (Point cx cy) radians = Paint $ do
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

-------------------------------------------------------------------------------
-- Geometric primitives:

-- | Draw an antialiased line onto the canvas.
drawLine :: (Axis a) => Tint {-^color-} -> Point a {-^start-}
         -> Point a {-^end-} -> Paint ()
drawLine tint start end = Paint $ do
  drawPrimitive GL.Lines tint $ pointVertex' start >> pointVertex' end

drawRect :: (Axis a) => Tint -> Rect a -> Paint ()
drawRect tint (Rect x y w h) = Paint $ do
  drawPrimitive GL.LineLoop tint $ do
    axisVertex' x y
    axisVertex' (x + w - 1) y
    axisVertex' (x + w - 1) (y + h - 1)
    axisVertex' x (y + h - 1)

-- | Tint a subrectangle of the canvas uniformly.
tintRect :: (Axis a) => Tint -> Rect a -> Paint ()
tintRect tint (Rect x y w h) = Paint $ do
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
drawLineChain tint points = Paint $ do
  drawPrimitive GL.LineStrip tint $ mapM_ pointVertex' points

drawPolygon :: (Axis a) => Tint -> [Point a] -> Paint ()
drawPolygon tint points = Paint $ do
  drawPrimitive GL.LineLoop tint $ mapM_ pointVertex' points

tintPolygon :: (Axis a) => Tint -> [Point a] -> Paint ()
tintPolygon tint points = Paint $ do
  drawPrimitive GL.Polygon tint $ mapM_ pointVertex' points

gradientPolygon :: (Axis a) => [(Tint, Point a)] -> Paint ()
gradientPolygon pointTints = Paint $ do
  GL.textureBinding GL.Texture2D $= Nothing
  let doVertex (tint, point) = setTint tint >> pointVertex point
  GL.renderPrimitive GL.Polygon $ mapM_ doVertex pointTints

tintRing :: (Axis a) => Tint -> a -> Point a -> a -> a -> Paint ()
tintRing tint thickness (Point cx cy) hRad vRad = Paint $ do
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

gradientRing :: (Double -> (Double, Tint)) -> (Double -> (Double, Tint))
             -> DPoint {-^center-} -> Double -> Double -> Paint ()
gradientRing innerFn outerFn (Point cx cy) hRad vRad = Paint $ do
  GL.preservingMatrix $ do
    GL.translate $ GL.Vector3 (toGLdouble cx) (toGLdouble cy) 0
    GL.textureBinding GL.Texture2D $= Nothing
    GL.renderPrimitive GL.TriangleStrip $ do
      let step = pi / 24 :: Double -- TODO
      let doPt fn theta = do
            let (rad, tint) = fn theta
            setTint tint
            axisVertex (rad * hRad * cos theta) (rad * vRad * sin theta)
      forM_ [0, (2 * step) .. 2 * (pi - step)] $ \theta -> do
        doPt innerFn theta
        doPt outerFn (theta + step)
      doPt innerFn 0
      doPt outerFn step

gradientFan :: DPoint -> Tint -> (Double -> (Double, Tint)) -> Paint ()
gradientFan (Point cx cy) centerTint fn = Paint $ do
  GL.preservingMatrix $ do
    GL.translate $ GL.Vector3 (toGLdouble cx) (toGLdouble cy) 0
    GL.textureBinding GL.Texture2D $= Nothing
    GL.renderPrimitive GL.TriangleFan $ do
      setTint centerTint
      glVertex 0 0
      let step = pi / 24 :: Double -- TODO
      let doPt theta = do
            let (rad, tint) = fn theta
            setTint tint
            axisVertex (rad * cos theta) (rad * sin theta)
      mapM_ doPt [0, step .. 2 * pi - step]
      doPt 0

-------------------------------------------------------------------------------
-- Fonts and text:

newtype Font = Font SDLt.Font
  deriving (Eq)

-- | Draw text with the given font and color onto the screen at the specified
-- location.
drawText :: (Axis a) => Font -> Color -> LocSpec a -> String -> Paint ()
drawText font color spec string = Paint $ do
  surface <- renderText' font color string
  blitSurface 1 surface spec

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
          (GL.TextureSize2D (toGLsizei width) (toGLsizei height)) 0
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
blitMinimap mm@(Minimap texture) loc = Paint $ withTexture texture $ do
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

loadSprite :: (MonadDraw m) => String -> m Sprite
loadSprite name = drawIO (makeSprite <$> loadTexture name)

loadSubSprite :: (MonadDraw m) => String -> IRect -> m Sprite
loadSubSprite name rect = drawIO (makeSubSprite rect <$> loadTexture name)

loadVStrip :: (MonadDraw m) => String -> Int -> m Strip
loadVStrip name size = drawIO $ do
  texture <- loadTexture name
  let (height, extra) = textureHeight texture `divMod` size
  when (extra /= 0) $ fail ("bad vstrip size " ++ show size ++ " for " ++ name)
  let width = textureWidth texture
  let slice n = makeSubSprite (Rect 0 (n * height) width height) texture
  return $ listArray (0, size - 1) $ map slice [0 .. size - 1]

loadSheet :: (MonadDraw m) => FilePath -> (Int, Int) -> m Sheet
loadSheet name (rows, cols) = drawIO $ do
  texture <- loadTexture name
  let (height, extraH) = textureHeight texture `divMod` rows
  let (width, extraW) = textureWidth texture `divMod` cols
  when (extraH /= 0 || extraW /= 0) $ do
    fail ("bad sheet size " ++ show (rows, cols) ++ " for " ++ name)
  return $ makeSheet texture width height cols rows

loadSheetWithTileSize :: (Int, Int) -> FilePath -> IO Sheet
loadSheetWithTileSize (width, height) name = do
  texture <- loadTexture name
  let (rows, extraH) = textureHeight texture `divMod` height
  let (cols, extraW) = textureWidth texture `divMod` width
  when (extraH /= 0 || extraW /= 0) $ do
    fail ("bad tile size " ++ show (width, height) ++ " for " ++ name)
  return $ makeSheet texture width height cols rows

makeSheet :: Texture -> Int -> Int -> Int -> Int -> Sheet
makeSheet texture w h cols rows = listArray bound $ map slice (range bound)
  where slice (row, col) = makeSubSprite (Rect (col * w) (row * h) w h) texture
        bound = ((0, 0), (rows - 1, cols - 1))

-- | Load an image from disk as a 'Texture' object.
loadTexture :: String -> IO Texture
loadTexture = ioEverCached HT.hashString $ \name -> do
  when debugResources $ do
    putStrLn ("loading texture " ++ name)
  (newTextureFromSurface <=< loadSurface) name

-- | Load an image from disk as an SDL surface.
loadSurface :: String -> IO SDL.Surface
loadSurface name = do -- = ioWeakCached HT.hashString $ \name -> do
  when debugResources $ do
    putStrLn ("loading surface " ++ name)
  surface <- getResourcePath "images" name >>= SDLi.load
  _ <- SDL.setAlpha surface [] 255 -- Turn off the SDL.SrcAlpha flag, if set.
  return surface

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
          (GL.TextureSize2D (toGLsizei width) (toGLsizei height))
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

toGLsizei :: Int -> GL.GLsizei
toGLsizei = fromIntegral

drawIO :: (MonadDraw m) => IO a -> m a
drawIO = runDraw . Draw

handlerIO :: (MonadHandler m) => IO a -> m a
handlerIO = runHandler . Handler

-------------------------------------------------------------------------------
