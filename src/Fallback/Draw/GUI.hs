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

module Fallback.Draw.GUI where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_)

import Fallback.Data.Color (Tint, tintAlpha)
import Fallback.Data.Point
import Fallback.Draw.Base
import Fallback.Event (Key(..))
import Fallback.Utility (fmod)

-------------------------------------------------------------------------------

newBackgroundPaint :: (MonadDraw m) => String -> Int -> Int -> Int -> Int
                   -> Int -> Int -> m (Paint ())
newBackgroundPaint filename ox oy mh mv sh sv = do
  let load = loadSubSprite filename . (`rectPlus` Point ox oy)
  topleft <- load $ Rect 0 0 mh mv
  topright <- load $ Rect (sh + mh) 0 mh mv
  bottomleft <- load $ Rect 0 (sv + mv) mh mv
  bottomright <- load $ Rect (sh + mh) (sv + mv) mh mv
  top <- load $ Rect mh 0 sh mv
  bottom <- load $ Rect mh (sv + mv) sh mv
  left <- load $ Rect 0 mv mh sv
  right <- load $ Rect (sh + mh) mv mh sv
  center <- load $ Rect mh mv sh sv
  return $ do
    rect <- canvasRect
    let (width, height) = rectSize rect
    let { hw = half width - mh; hh = half height - mv }
    -- Center:
    blitRepeat center (Point hw hh) $ adjustRect mh mv mh mv rect
    -- Sides:
    blitRepeat top (Point hw 0) $ Rect mh 0 (width - 2*mh) mv
    blitRepeat bottom (Point hw 0) $ Rect mh (height - mv) (width - 2*mh) mv
    blitRepeat left (Point 0 hh) $ Rect 0 mv mh (height - 2*mv)
    blitRepeat right (Point 0 hh) $ Rect (width - mh) mv mh (height - 2*mv)
    -- Corners:
    blitLoc topleft $ LocTopleft $ Point 0 (0 :: Int)
    blitLoc topright $ LocTopright $ Point width 0
    blitLoc bottomleft $ LocBottomleft $ Point 0 height
    blitLoc bottomright $ LocBottomright $ Point width height

-- | Draw the outline of a rectangle with beveled corners.
drawBevelRect :: Tint -> Int {-^bevel size-} -> IRect -> Paint ()
drawBevelRect tint b rect = withSubCanvas rect $ do
  (w, h) <- canvasSize
  drawPolygon tint [Point b 0, Point (w - b - 1) 0,
                    Point (w - 1) b, Point (w - 1) (h - b - 1),
                    Point (w - b - 1) (h - 1), Point b (h - 1),
                    Point 0 (h - b - 1), Point 0 b]

-------------------------------------------------------------------------------

paintNumber :: Strip -> Int -> LocSpec Int -> Paint ()
paintNumber digits num loc = do
  let (w, h) = spriteSize (digits ! 0)
  let string = show (abs num)
  let Point x y = locTopleft loc (1 + (w - 1) * length string, h)
  let drawDigit dx char =
        blitTopleft (digits ! (fromEnum char - fromEnum '0'))
                    (Point (x + dx) y)
  zipWithM_ drawDigit [0, w - 1 ..] string

-- TODO: deprecated
newDigitPaint :: (MonadDraw m) => m (Int -> LocSpec Int -> Paint ())
newDigitPaint = paintNumber <$> loadVStrip "small-digits.png" 10

-------------------------------------------------------------------------------

paintSwoosh :: Tint -> Double -> [DPoint] -> Paint ()
paintSwoosh tint thickness points = mapM_ paintSegment $ quads points where
  paintSegment (Nothing, _, _, Nothing) = return ()
  paintSegment (Just a, b, c, Nothing) = do
    let delta = getDelta a b c
    gradientPolygon [(tint, b), (tint', b `pAdd` delta), (tint', c)]
    gradientPolygon [(tint, b), (tint', b `pSub` delta), (tint', c)]
  paintSegment (Nothing, b, c, Just d) = do
    let delta = getDelta d c b
    gradientPolygon [(tint, c), (tint', c `pAdd` delta), (tint', b)]
    gradientPolygon [(tint, c), (tint', c `pSub` delta), (tint', b)]
  paintSegment (Just a, b, c, Just d) = do
    let delta1 = getDelta a b c
        delta2 = getDelta d c b
    gradientPolygon [(tint, b), (tint', b `pAdd` delta1),
                     (tint', c `pSub` delta2), (tint, c)]
    gradientPolygon [(tint, b), (tint', b `pSub` delta1),
                     (tint', c `pAdd` delta2), (tint, c)]

  getDelta p1 p2 p3 =
    let dir1 = pAtan2 (p1 `pSub` p2)
        dir3 = pAtan2 (p3 `pSub` p2)
    in pPolar thickness $ dir3 + 0.5 * ((dir1 - dir3) `fmod` (2 * pi))

  quads :: [a] -> [(Maybe a, a, a, Maybe a)]
  quads = subquads Nothing where
    subquads a (b : c : d : rest) =
      (a, b, c, Just d) : subquads (Just b) (c : d : rest)
    subquads a [b, c] = [(a, b, c, Nothing)]
    subquads _ _ = []

  tint' = tint { tintAlpha = 0 }

-------------------------------------------------------------------------------

drawThickLine :: Double {-^width-} -> Tint {-^color-} -> DPoint {-^start-}
              -> DPoint {-^end-} -> Paint ()
drawThickLine width tint start end = do
  let perp = pPolar (width / 2) (pAtan2 (end `pSub` start) + pi/2)
  tintPolygon tint [start `pAdd` perp, start `pSub` perp,
                    end `pSub` perp, end `pAdd` perp]

drawThickLineChain :: Double -> Tint -> [DPoint] -> Paint ()
drawThickLineChain width tint (p1 : p2 : rest) = do
  drawThickLine width tint p1 p2
  drawThickLineChain width tint (p2 : rest)
drawThickLineChain _ _ _ = return ()

-------------------------------------------------------------------------------

getArrowKeysDirection :: (MonadHandler m) => m (Maybe Direction)
getArrowKeysDirection = do
  up <- getKeyState KeyUpArrow
  dn <- getKeyState KeyDownArrow
  lf <- getKeyState KeyLeftArrow
  rt <- getKeyState KeyRightArrow
  return $ case (up, dn, lf, rt) of
    (False, False, False,  True) -> Just DirE
    (False, False,  True, False) -> Just DirW
    (False,  True, False, False) -> Just DirS
    (False,  True, False,  True) -> Just DirSE
    (False,  True,  True, False) -> Just DirSW
    ( True, False, False, False) -> Just DirN
    ( True, False, False,  True) -> Just DirNE
    ( True, False,  True, False) -> Just DirNW
    _ -> Nothing

isMouseWithinCanvas :: (MonadHandler m) => m Bool
isMouseWithinCanvas = do
  rect <- canvasRect
  maybe False (rectContains rect) <$> getRelativeMousePos

-------------------------------------------------------------------------------
