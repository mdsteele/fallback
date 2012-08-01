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

module Fallback.Data.Point
  (-- * Axis class
   Axis(..),
   -- * Point type
   Point(..), IPoint, DPoint,
   pZero, pPolar, pNeg, pAdd, pSub, pMul, pDiv, pDist, pAtan2,
   -- * Rect type
   Rect(..), IRect, DRect,
   makeRect, rectContains, rectIntersects, rectIntersection,
   rectPlus, rectMinus, adjustRect, adjustRect1,
   rectSize, rectTopleft, rectCenter,
   -- * LocSpec type
   LocSpec(..), locTopleft, locRect,
   -- * Direction type
   Direction(..), allDirections, isCardinal,
   ipointDir, dirTo, dirDelta, plusDir,
   -- * Position type
   Position, PRect, adjacent, bresenhamPositions, prectPositions,
   expandPrect, expandPosition,
   SqDist(..), sqDistRadius, pSqDist, ofRadius, rangeTouchesRect)
where

import Data.Ix (Ix, range)
import Data.List (unfoldr)
import System.Random (Random(random, randomR))
import Text.Read (readPrec)

import Fallback.Utility (hypot)

-------------------------------------------------------------------------------
-- Axis class:

class (Real a) => Axis a where
  half :: a -> a
  toIntegral :: (Integral b) => a -> b
  toFloating :: (RealFloat b) => a -> b

instance Axis Int where
  half = (`div` 2)
  toIntegral = fromIntegral
  toFloating = fromIntegral

instance Axis Double where
  half = (/ 2)
  toIntegral = round
  toFloating = realToFrac

-------------------------------------------------------------------------------
-- Point type:

data Point a = Point { pointX :: !a, pointY :: !a }
  deriving (Eq, Ix, Ord)

instance Functor Point where
  fmap f (Point x y) = Point (f x) (f y)

instance (Show a) => Show (Point a) where
  show (Point x y) = show (x, y)

instance (Read a) => Read (Point a) where
  readPrec = fmap (uncurry Point) readPrec

type IPoint = Point Int
type DPoint = Point Double

pZero :: (Num a) => Point a
pZero = Point 0 0

pPolar :: (Floating a) => a -> a -> Point a
pPolar r th = Point (r * cos th) (r * sin th)

pNeg :: (Num a) => Point a -> Point a
pNeg (Point x y) = Point (negate x) (negate y)

infixl 6 `pAdd`, `pSub`

pAdd :: (Num a) => Point a -> Point a -> Point a
pAdd (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

pSub :: (Num a) => Point a -> Point a -> Point a
pSub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

infixl 7 `pMul`, `pDiv`

pMul :: (Num a) => Point a -> a -> Point a
pMul (Point x y) a = Point (x * a) (y * a)

pDiv :: (Fractional a) => Point a -> a -> Point a
pDiv (Point x y) a = Point (x / a) (y / a)

pDist :: DPoint -> DPoint -> Double
pDist (Point x1 y1) (Point x2 y2) = hypot (x2 - x1) (y2 - y1)

pAtan2 :: (RealFloat a) => Point a -> a
pAtan2 (Point x y) = atan2 y x

-------------------------------------------------------------------------------
-- Rect type:

type IRect = Rect Int
type DRect = Rect Double

data Rect a = Rect { rectX :: !a, rectY :: !a, rectW :: !a, rectH :: !a }
  deriving (Eq, Ord)

instance Functor Rect where
  fmap f (Rect x y w h) = Rect (f x) (f y) (f w) (f h)

instance (Show a) => Show (Rect a) where
  show (Rect x y w h) = show (x, y, w, h)

instance (Read a) => Read (Rect a) where
  readPrec = do (x, y, w, h) <- readPrec
                return (Rect x y w h)

makeRect :: (Num a) => Point a -> (a, a) -> Rect a
makeRect (Point x y) (w, h) = Rect x y w h

-- | Return 'True' if the rectangle contains the given point, 'False'
--   otherwise.
rectContains :: (Real a) => Rect a -> Point a -> Bool
rectContains (Rect x y w h) (Point x' y') =
  x' >= x && y' >= y && x' < x + w && y' < y + h

-- | Return 'True' if the two rectangles overlap at all, 'False' otherwise.
rectIntersects :: (Real a) => Rect a -> Rect a -> Bool
rectIntersects rect1 rect2 = let rect3 = rectIntersection rect1 rect2
                             in rectW rect3 > 0 && rectH rect3 > 0

-- | Find the intersection of two rectangles.
rectIntersection :: (Real a) => Rect a -> Rect a -> Rect a
rectIntersection (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  let x = max x1 x2
      y = max y1 y2
      w = max 0 (min (x1 + w1) (x2 + w2) - x)
      h = max 0 (min (y1 + h1) (y2 + h2) - y)
  in Rect x y w h

rectPlus :: (Num a) => Rect a -> Point a -> Rect a
rectPlus (Rect x1 y1 w h) (Point x2 y2) = Rect (x1 + x2) (y1 + y2) w h

rectMinus :: (Num a) => Rect a -> Point a -> Rect a
rectMinus (Rect x1 y1 w h) (Point x2 y2) = Rect (x1 - x2) (y1 - y2) w h

adjustRect :: (Real a) => a -> a -> a -> a -> Rect a -> Rect a
adjustRect x1 y1 x2 y2 (Rect x y w h) =
  Rect (x + x1) (y + y1) (max 0 $ w - x2 - x1) (max 0 $ h - y2 - y1)

adjustRect1 :: (Real a) => a -> Rect a -> Rect a
adjustRect1 m = adjustRect m m m m

-- | Return the size of the rectangle as a @(width, height)@ pair.
rectSize :: Rect a -> (a, a)
rectSize (Rect _ _ w h) = (w, h)

-- | Return the point at the top-left corner of the given rectangle.
rectTopleft :: Rect a -> Point a
rectTopleft (Rect x y _ _) = Point x y

-- | Return the point at the top-left corner of the given rectangle.
rectCenter :: (Axis a) => Rect a -> Point a
rectCenter (Rect x y w h) = Point (x + half w) (y + half h)

-------------------------------------------------------------------------------
-- LocSpec type:

data LocSpec a = LocTopleft !(Point a)
               | LocMidtop !(Point a)
               | LocTopright !(Point a)
               | LocMidleft !(Point a)
               | LocCenter !(Point a)
               | LocMidright !(Point a)
               | LocBottomleft !(Point a)
               | LocMidbottom !(Point a)
               | LocBottomright !(Point a)

locTopleft :: (Axis a) => LocSpec a -> (a, a) -> Point a
locTopleft loc (w, h) =
  case loc of
    LocTopleft p -> p
    LocMidtop (Point x y) -> Point (x - half w) y
    LocTopright (Point x y) -> Point (x - w) y
    LocMidleft (Point x y) -> Point x (y - half h)
    LocCenter (Point x y) -> Point (x - half w) (y - half h)
    LocMidright (Point x y) -> Point (x - w) (y - half h)
    LocBottomleft (Point x y) -> Point x (y - h)
    LocMidbottom (Point x y) -> Point (x - half w) (y - h)
    LocBottomright (Point x y) -> Point (x - w) (y - h)

locRect :: (Axis a) => LocSpec a -> (a, a) -> Rect a
locRect loc (w, h) = Rect x y w h where
  Point x y = locTopleft loc (w, h)

-------------------------------------------------------------------------------
-- Direction type:

data Direction = DirE | DirSE | DirS | DirSW | DirW | DirNW | DirN | DirNE
  deriving (Eq, Ord, Show)

instance Enum Direction where
  fromEnum d = case d of { DirE -> 0; DirSE -> 1; DirS -> 2; DirSW -> 3;
                           DirW -> 4; DirNW -> 5; DirN -> 6; DirNE -> 7 }
  toEnum n = case n `mod` 8 of { 0 -> DirE; 1 -> DirSE; 2 -> DirS; 3 -> DirSW;
                                 4 -> DirW; 5 -> DirNW; 6 -> DirN; 7 -> DirNE;
                                 _ -> error "Direction.toEnum" }
  enumFrom d = let n = fromEnum d in map toEnum [n .. n + 7]
  enumFromThen d1 d2 =
    let n1 = fromEnum d1
        n2 = fromEnum d2
        delta = n2 - n1
    in map toEnum [n1, n2 .. n1 + delta * (8 `div` gcd 8 delta - 1)]
  enumFromTo d1 d2 =
    let n1 = fromEnum d1
        n2 = fromEnum d2
    in map toEnum [n1 .. n1 + (n2 - n1) `mod` 8]
  enumFromThenTo _ _ _ = error "FIXME Direction.enumFromThenTo"

instance Random Direction where
  random g = let (i, g') = randomR (0, 7) g in (toEnum i, g')
  randomR (lo, hi) g =
    let lo' = fromEnum lo
        (i, g') = randomR (lo', lo' + (fromEnum hi - lo') `mod` 8) g
    in (toEnum i, g')

-- | A list of all 'Direction' values, with the four cardinal directions
-- appearing before the other four directions.
allDirections :: [Direction]
allDirections = [DirE, DirS, DirW, DirN, DirSE, DirSW, DirNW, DirNE]

-- | Return 'True' if the direction is east, south, west, or north.
isCardinal :: Direction -> Bool
isCardinal DirE = True
isCardinal DirSE = False
isCardinal DirS = True
isCardinal DirSW = False
isCardinal DirW = True
isCardinal DirNW = False
isCardinal DirN = True
isCardinal DirNE = False

ipointDir :: IPoint -> Direction
ipointDir = toEnum . (`mod` 8) . (`div` 2) . (+1) . floor . (* (8 / pi)) .
            pAtan2 . fmap (fromIntegral :: Int -> Double)

dirTo :: IPoint -> IPoint -> Direction
dirTo from to = ipointDir (to `pSub` from)

dirDelta :: Direction -> IPoint
dirDelta DirE  = Point   1    0
dirDelta DirSE = Point   1    1
dirDelta DirS  = Point   0    1
dirDelta DirSW = Point (-1)   1
dirDelta DirW  = Point (-1)   0
dirDelta DirNW = Point (-1) (-1)
dirDelta DirN  = Point   0  (-1)
dirDelta DirNE = Point   1  (-1)

infixl 6 `plusDir`

plusDir :: IPoint -> Direction -> IPoint
plusDir p d = p `pAdd` dirDelta d

-------------------------------------------------------------------------------
-- Position type:

-- | A position in the game grid.
type Position = IPoint

-- | A rectangle of positions.
type PRect = IRect

-- | Return 'True' if the positions are adjacent or equal, 'False' otherwise.
adjacent :: Position -> Position -> Bool
adjacent p1 p2 = pSqDist p1 p2 <= SqDist 2

bresenhamPositions :: Position -> Position -> [Position]
bresenhamPositions (Point x1'' y1'') (Point x2'' y2'') =
  -- See http://en.wikipedia.org/wiki/Bresenham_line_algorithm
  let dx' = abs (x2'' - x1'')
      dy' = abs (y2'' - y1'')
      steep = dy' > dx'
      (dx, dy, x1', y1', x2', y2') =
        if steep
        then (dy', dx', y1'', x1'', y2'', x2'')
        else (dx', dy', x1'', y1'', x2'', y2'')
      reversed = x1' > x2'
      (x1, y1, x2, y2) =
        if reversed
        then (x2', y2', x1', y1')
        else (x1', y1', x2', y2')
      yStep = if y1 < y2 then 1 else -1
      yield (x, y, err) =
        if x > x2 then Nothing else
          Just (if steep then Point y x else Point x y,
                let err' = err - dy
                in if err' < 0 then (x + 1, y + yStep, err' + dx)
                   else (x + 1, y, err'))
      positions = unfoldr yield (x1, y1, dx `div` 2)
  in if reversed then reverse positions else positions

-- | Return a list of all positions within a position-rectangle.
prectPositions :: PRect -> [Position]
prectPositions (Rect x y w h) =
  range (Point x y, Point (x + w - 1) (y + h - 1))

-- | Expand a 'PRect' by one position in each direction.
expandPrect :: PRect -> PRect
expandPrect = adjustRect1 (negate 1)

-- | Create a 3x3 'PRect' centered on the given position.
expandPosition :: Position -> PRect
expandPosition (Point x y) = Rect (x - 1) (y - 1) 3 3

-- | A squared distance between two positions.
newtype SqDist = SqDist Int
  deriving (Eq, Ord)

sqDistRadius :: SqDist -> Double
sqDistRadius (SqDist sq) = sqrt (fromIntegral sq)

pSqDist :: Position -> Position -> SqDist
pSqDist (Point x1 y1) (Point x2 y2) =
  SqDist ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- | Given a radius (in tiles), produce a squared distance that yields a pretty
-- nice circle of that radius.  In particular, this gives a better-shaped
-- circle than simply squaring the input.
ofRadius :: Int -> SqDist
ofRadius r = SqDist $ floor ((fromIntegral r + 0.5 :: Double) ^^ (2 :: Int))

rangeTouchesRect :: Position -> SqDist -> PRect -> Bool
rangeTouchesRect (Point cx cy) sqDist (Rect x1 y1 w h) =
  let x2 = x1 + w - 1
      y2 = y1 + h - 1
      corner x y = pSqDist (Point cx cy) (Point x y) <= sqDist
      side a b = SqDist ((a - b) * (a - b)) <= sqDist
  in if cx < x1 then
       if cy < y1 then corner x1 y1
       else if cy > y2 then corner x1 y2 else side cx x1
     else if cx > x2 then
       if cy < y1 then corner x2 y1
       else if cy > y2 then corner x2 y2 else side cx x2
     else
       if cy < y1 then side cy y1
       else if cy > y2 then side cy y2 else True

-------------------------------------------------------------------------------
