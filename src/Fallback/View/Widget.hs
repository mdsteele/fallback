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

module Fallback.View.Widget
  (-- * Displaying text
   makeLabel, makeLabel_, newStaticTextWrapColorPaint, newStaticTextWrapView,
   newDynamicTextWrapView, newTooltipView,
   -- * Buttons
   ButtonState(..), ButtonStatus(..), enabledIf,
   newButton, newTextButton, newSimpleTextButton, newFlashingTextButton,
   newIconButton,
   -- * Editing text
   newTextBox,
   -- * Scrolling
   newScrollBar, newScrollZone)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (second)
import Control.Monad (when, zipWithM_)
import Data.List (inits)
import Data.Maybe (isJust)

import Fallback.Data.Clock (Clock, clockZigzag)
import Fallback.Data.Color
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.State.Resources (FontTag(..), Resources, rsrcFont)
import Fallback.State.Text (TextLine, paintTextLine, wrapText)
import Fallback.View.Base

-------------------------------------------------------------------------------

makeLabel :: (Axis a) => Font -> Color -> ((Int, Int) -> LocSpec a)
          -> View String b
makeLabel font color locFn = inertView $ \string -> do
  size <- canvasSize
  drawText font color (locFn size) string

makeLabel_ :: (Axis a) => Font -> Color -> LocSpec a -> View String b
makeLabel_ font color loc = inertView $ drawText font color loc

-------------------------------------------------------------------------------

newStaticTextWrapColorPaint :: (MonadDraw m) => Resources -> Int -> String
                            -> m (Int, Color -> Paint ())
newStaticTextWrapColorPaint resources width string = do
  let textLines = wrapText resources width string
  let spacing = 14
  let paint color = drawTextLines color spacing textLines
  return (spacing * length textLines, paint)

newStaticTextWrapView :: (MonadDraw m) => Resources -> Int -> String
                      -> m (Int, View a b)
newStaticTextWrapView resources width string =
  (second (inertView . (. const blackColor))) <$>
  newStaticTextWrapColorPaint resources width string

newDynamicTextWrapView :: (MonadDraw m) => Resources -> m (View String b)
newDynamicTextWrapView resources = do
  cacheRef <- newDrawRef (0, "", [])
  let paint text = do
        textLines <- do
          width <- canvasWidth
          (oldWidth, oldText, oldLines) <- readDrawRef cacheRef
          if width == oldWidth && text == oldText then return oldLines else do
            let tlines = wrapText resources width text
            writeDrawRef cacheRef (width, text, tlines)
            return tlines
        drawTextLines blackColor 14 textLines
  return $ inertView paint

newTooltipView :: (MonadDraw m) => Resources -> m (View String b)
newTooltipView resources = do
  let margin = 7
  cacheRef <- newDrawRef (0, "", [])
  let paint text = do
        textLines <- do
          width <- subtract (2 * margin) <$> canvasWidth
          (oldWidth, oldText, oldLines) <- readDrawRef cacheRef
          if width == oldWidth && text == oldText then return oldLines else do
            let tlines = wrapText resources width text
            writeDrawRef cacheRef (width, text, tlines)
            return tlines
        (width, height) <- canvasSize
        let height' = 14 * length textLines + 2 * margin
        let rect = Rect 0 (half (height - height')) width height'
        tintRect (Tint 0 0 0 220) rect
        drawRect (Tint 255 255 255 220) $ adjustRect1 1 rect
        withSubCanvas (adjustRect1 margin rect) $ do
          drawTextLines whiteColor 14 textLines
  return $ inertView paint

drawTextLines :: Color -> Int -> [TextLine] -> Paint ()
drawTextLines color spacing textLines = do
  let drawTextLine n textLine =
        paintTextLine color (LocTopleft $ Point 0 (n * spacing)) textLine
  zipWithM_ drawTextLine [0 ..] textLines

-------------------------------------------------------------------------------

data ButtonState = ButtonUp | ButtonHover | ButtonDown | ButtonDisabled
  deriving Eq

data ButtonStatus = ReadyButton | DisabledButton | DepressedButton
  deriving Eq

enabledIf :: Bool -> ButtonStatus
enabledIf b = if b then ReadyButton else DisabledButton

newButton :: (MonadDraw m) => (a -> ButtonState -> Paint ())
          -> (a -> ButtonStatus) -> [Key] -> b -> m (View a b)
newButton paintFn statusFn keys value = do
  state <- newDrawRef (False, False, False)
  let

    paint input = do
      (keyPress, mousePress, mouseHover) <- readDrawRef state
      paintFn input $
        case statusFn input of
          ReadyButton ->
            if keyPress || mousePress && mouseHover then ButtonDown else
              if mouseHover then ButtonHover else ButtonUp
          DisabledButton -> ButtonDisabled
          DepressedButton -> ButtonDown

    handler input event = do
      rect <- canvasRect
      (kp, mp, mh) <- readDrawRef state
      case event of
        EvKeyDown key [] _ ->
          if key `notElem` keys then return Ignore
          else Suppress <$ writeDrawRef state (True, mp, mh)
        EvKeyUp key ->
          if not (kp && key `elem` keys) then return Ignore else do
            writeDrawRef state (False, mp, mh)
            return $ if not (mp && mh) && statusFn input == ReadyButton
                     then Action value else Ignore
        EvMouseMotion pt _ ->
          Ignore <$ writeDrawRef state (kp, mp, rectContains rect pt)
        EvMouseDown pt ->
          if not (rectContains rect pt) then return Ignore
          else Suppress <$ writeDrawRef state (kp, True, mh)
        EvMouseUp _ -> if not mp then return Ignore else do
          writeDrawRef state (kp, False, mh)
          return $ if mh && not kp && statusFn input == ReadyButton
                   then Action value else Ignore
        EvFocus pt ->
          Ignore <$ writeDrawRef state (False, False, rectContains rect pt)
        EvBlur ->
          Ignore <$ writeDrawRef state (False, False, False)
        _ -> return Ignore

  return (View paint handler)

newStandardButton :: (MonadDraw m) => (a -> ButtonState -> Paint ())
                  -> (a -> ButtonStatus) -> [Key] -> b -> m (View a b)
newStandardButton paintFn statusFn keys value = do
  paintUp <- newBackgroundPaint "gui/blank-button.png" 0  0 3 3 57 34
  paintHv <- newBackgroundPaint "gui/blank-button.png" 0 40 3 3 57 34
  paintDn <- newBackgroundPaint "gui/blank-button.png" 0 80 3 3 57 34
  let paintFn' input bs = do
        case bs of ButtonUp -> paintUp
                   ButtonHover -> paintHv
                   ButtonDown -> paintDn
                   ButtonDisabled -> paintUp
        (w, h) <- canvasSize
        let (x, y) = if bs == ButtonDown then (2, 2) else (1, 1)
        withSubCanvas (Rect x y (w - 2) (h - 2)) $ paintFn input bs
  newButton paintFn' statusFn keys value

newTextButton :: (MonadDraw m) => Resources -> [Key] -> a
              -> m (View (String, ButtonStatus) a)
newTextButton resources keys value = do
  let font = rsrcFont resources FontChancery14
  let paintFn (text, _) bs = do
        let color = case bs of
                      ButtonUp -> Color 32 32 32
                      ButtonHover -> Color 0 0 0
                      ButtonDown -> Color 0 0 0
                      ButtonDisabled -> Color 128 128 128
        rect <- canvasRect
        drawText font color (LocCenter $ rectCenter rect) text
  newStandardButton paintFn snd keys value

newSimpleTextButton :: (MonadDraw m) => Resources -> String -> [Key] -> b
                    -> m (View a b)
newSimpleTextButton resources text keys value =
  vmap (const (text, ReadyButton)) <$> newTextButton resources keys value

newFlashingTextButton :: (MonadDraw m) => Resources -> String -> [Key] -> b
                      -> m (View Clock b)
newFlashingTextButton resources text keys value = do
  let font = rsrcFont resources FontChancery14
  let paintFn clock bs = do
        let flash = fromIntegral (20 * clockZigzag 10 1 clock)
        let color = case bs of
                      ButtonUp -> Color 32 (32 + flash) 32
                      ButtonHover -> Color 0 flash 0
                      ButtonDown -> Color flash 0 0
                      ButtonDisabled -> Color 128 128 128
        rect <- canvasRect
        drawText font color (LocCenter $ rectCenter rect) text
  newStandardButton paintFn (const ReadyButton) keys value

newIconButton :: (MonadDraw m) => [Key] -> a
              -> m (View (Sprite, ButtonStatus) a)
newIconButton keys value = do
  let paintFn (icon, _) bs = do
        let tint = case bs of
                     ButtonUp -> whiteTint
                     ButtonHover -> Tint 192 192 192 255
                     ButtonDown -> Tint 128 128 128 255
                     ButtonDisabled -> Tint 255 255 255 64
        rect <- canvasRect
        blitLocTinted tint icon $ LocCenter $ rectCenter rect
  newStandardButton paintFn snd keys value

-------------------------------------------------------------------------------

newTextBox :: (MonadDraw m) => Resources -> (String -> Draw Bool)
           -> m (View String String)
newTextBox resources testFn = do
  let font = rsrcFont resources FontGeorgiaBold16
  activeRef <- newDrawRef True
  cursorRef <- newDrawRef 0
  let

    margin = 4

    paint text = do
      tintCanvas (Tint 255 255 255 192)
      canvasRect >>= drawRect blackTint
      height <- canvasHeight
      drawText font blackColor (LocMidleft $ Point margin (half height)) text
      (readDrawRef activeRef >>=) $ flip when $ do
        cursor <- readDrawRef cursorRef
        let x = margin + textRenderWidth font (take cursor text)
        drawLine (Tint 0 128 128 128) (Point x 2) (Point x (height - 2))

    handler _ (EvKeyDown KeyLeftArrow _ _) = do
      readDrawRef cursorRef >>= writeDrawRef cursorRef . max 0 . subtract 1
      return Suppress
    handler t (EvKeyDown KeyRightArrow _ _) = do
      readDrawRef cursorRef >>= writeDrawRef cursorRef . min (length t) . (+1)
      return Suppress
    handler text (EvKeyDown _ _ char) = do
      active <- readDrawRef activeRef
      if not active || char < ' ' || char > '\DEL' then return Ignore else do
        cursor <- readDrawRef cursorRef
        let (cursor', text') =
              if char == '\DEL'
              then (cursor - 1, take (cursor - 1) text ++ drop cursor text)
              else (cursor + 1, take cursor text ++ [char] ++ drop cursor text)
        acceptable <- runDraw (testFn text')
        if not acceptable then return Suppress else
          Action text' <$ writeDrawRef cursorRef cursor'
    handler text (EvMouseDown pt) = do
      rect <- canvasRect
      let hit = rectContains rect pt
      writeDrawRef activeRef hit
      if not hit then return Ignore else do
        writeDrawRef cursorRef $ snd $ minimum $ flip zip [0 ..] $
          map (abs . subtract (pointX pt - rectX rect - margin) .
               textRenderWidth font) $ inits text
        return Suppress
    handler _ EvBlur = Ignore <$ writeDrawRef activeRef False
    handler _ _ = return Ignore

  return $ View paint handler

-------------------------------------------------------------------------------

-- | Create a new scroll bar view.  The view inputs are (minimum value, maximum
-- value, per-page, current page top), while the output is the new page top.
newScrollBar :: (MonadDraw m) => m (View (Int, Int, Int, Int) Int)
newScrollBar = do
  sheet <- loadSheet "gui/scroll-bar.png" (3, 4)
  stateRef <- newDrawRef (Nothing, False)
  let

    (width, pieceHeight) = spriteSize $ sheet ! (0, 0)

    paint input =
      if isDisabled input then paintBackground (Tint 255 255 255 128) else do
        paintBackground whiteTint
        (grab, hover) <- readDrawRef stateRef
        let col = if isJust grab then 2 else if hover then 1 else 0
        rect <- knobRect input <$> canvasRect
        blitLoc (sheet ! (0, col)) $ LocTopleft $ Point 0 (rectY rect)
        when (rectH rect > 2 * pieceHeight) $ do
          blitRepeat (sheet ! (1, col)) pZero $
            Rect 0 (rectY rect + pieceHeight)
                 width (rectH rect - 2 * pieceHeight)
        blitLoc (sheet ! (2, col)) $ LocBottomleft $
          Point 0 (rectY rect + rectH rect)

    paintBackground tint = do
      height <- canvasHeight
      blitLocTinted tint (sheet ! (0, 3)) $ LocTopleft (pZero :: IPoint)
      blitRepeatTinted tint (sheet ! (1, 3)) pZero $
        Rect 0 pieceHeight width (height - 2 * pieceHeight)
      blitLocTinted tint (sheet ! (2, 3)) $ LocBottomleft $ Point 0 height

    handler input@(minVal, maxVal, perPage, _) (EvMouseMotion pt _) = do
      rect <- canvasRect
      grab <- fst <$> readDrawRef stateRef
      writeDrawRef stateRef (grab, rectContains (knobRect input rect) pt)
      case grab of
        Nothing -> return Ignore
        Just (startValue, startY) -> do
          let dy = pointY pt - startY
          let dv = (dy * (maxVal - perPage - minVal)) !/!
                   (rectH rect - rectH (knobRect input rect))
          return $ Action $ max minVal $
            min (maxVal - perPage) (startValue + dv)
    handler input@(minVal, maxVal, perPage, curVal) (EvMouseDown pt) = do
      rect <- canvasRect
      let knob = knobRect input rect
      if rectContains knob pt then do
        Ignore <$ writeDrawRef stateRef (Just (curVal, pointY pt), True)
       else do
        return $ if not (rectContains rect pt) then Ignore
                 else Action $ if pointY pt < rectY knob
                               then max minVal $ curVal - perPage
                               else min (maxVal - perPage) (curVal + perPage)
    handler input (EvMouseUp pt) = do
      rect <- canvasRect
      writeDrawRef stateRef (Nothing, rectContains (knobRect input rect) pt)
      return Ignore
    handler (_, maxValue, perPage, curValue) (EvScrollDownwards pt) = do
      rect <- canvasRect
      return $ if rectContains rect pt && curValue < maxValue - perPage
               then Action (curValue + 1) else Ignore
    handler (minValue, _, _, curValue) (EvScrollUpwards pt) = do
      rect <- canvasRect
      return $ if rectContains rect pt && curValue > minValue
               then Action (curValue - 1) else Ignore
    handler input (EvFocus pt) = do
      rect <- canvasRect
      when (rectContains (knobRect input rect) pt) $ do
        writeDrawRef stateRef (Nothing, True)
      return Ignore
    handler _ EvBlur = Ignore <$ writeDrawRef stateRef (Nothing, False)
    handler _ _ = return Ignore

    isDisabled (minValue, maxValue, perPage, _) =
      perPage >= maxValue - minValue

    knobRect :: (Int, Int, Int, Int) -> IRect -> IRect
    knobRect input@(minValue, maxValue, perPage, curValue) rect =
      if isDisabled input
      then Rect (rectX rect) (rectY rect) width (rectH rect)
      else Rect (rectX rect) (rectY rect + y) width h where
        h = min (rectH rect) $ (perPage * rectH rect) !/! (maxValue - minValue)
        y = ((rectH rect - h) * (curValue - minValue)) !/!
            (maxValue - perPage - minValue)

    (!/!) :: Int -> Int -> Int
    x !/! y = round (fromIntegral x / (fromIntegral y :: Double))

  return $ View paint handler

newScrollZone :: (MonadDraw m) => m (View (Int, Int, Int, Int) Int)
newScrollZone = do
  let barWidth = 10
  scrollBar <- newScrollBar
  let handler (_, maxValue, perPage, curValue) (EvScrollDownwards pt) = do
        whenWithinCanvas pt $ do
          return $ Action $ min (maxValue - perPage) $ curValue + 1
      handler (minValue, _, _, curValue) (EvScrollUpwards pt) = do
        whenWithinCanvas pt $ return $ Action $ max minValue $ curValue - 1
      handler _ _ = return Ignore
  return $ compoundView [
    (subView (\_ (w, h) -> Rect (w - barWidth) 0 barWidth h) $ scrollBar),
    (View (const $ return ()) handler)]

-------------------------------------------------------------------------------
