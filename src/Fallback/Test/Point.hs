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

module Fallback.Test.Point (pointTests) where

import Data.Maybe (listToMaybe)
import Test.HUnit ((~:), Test(TestList))

import Fallback.Data.Point
import Fallback.Test.Base (insist, insistEq, qcTest)

-------------------------------------------------------------------------------

pointTests :: Test
pointTests = "point" ~: TestList [
  bresenhamTests,
  directionTests,
  rectTests,
  sqDistTests,
  vectorTests]

bresenhamTests :: Test
bresenhamTests = "bresenham" ~: TestList [
  bpTest $ \(p1, p2) -> not $ null $ bresenhamPositions p1 p2,
  bpTest $ \(p1, p2) -> listToMaybe (bresenhamPositions p1 p2) == Just p1,
  bpTest $ \(p1, p2) -> (listToMaybe $ reverse $
                         bresenhamPositions p1 p2) == Just p2]

directionTests :: Test
directionTests = "direction" ~: TestList [
  qcTest $ \d -> pred (succ d) == (d :: Direction),
  qcTest $ \d -> pred (succ d) == succ (pred (d :: Direction)),
  qcTest $ \d -> (pred $ pred $ pred $ pred $ pred d) ==
                 (succ $ succ $ succ (d :: Direction)),
  qcTest $ \n -> toEnum (n + 1) == succ (toEnum n :: Direction),
  qcTest $ \n -> toEnum (n - 1) == pred (toEnum n :: Direction),
  insistEq [DirN, DirNE, DirE, DirSE, DirS, DirSW, DirW, DirNW] [DirN ..],
  insistEq [DirW, DirNW, DirN] [DirW .. DirN],
  insistEq [DirN, DirS] [DirN, DirS ..],
  insistEq [DirN, DirE, DirS, DirW] [DirN, DirE ..],
  insistEq [DirN, DirSW, DirE, DirNW, DirS, DirNE, DirW, DirSE]
           [DirN, DirSW ..]]

rectTests :: Test
rectTests = "rect" ~: TestList [
  qcTest $ \r s -> rectIntersection r s == rectIntersection s (r :: IRect),
  qcTest $ \r s -> let i = rectIntersection r (s :: IRect)
                   in i == rectIntersection r i && i == rectIntersection s i,
  insist $ rectIntersects (Rect 1 2 3 4) (Rect 3 1 5 5 :: IRect),
  insist $ not $ rectIntersects (Rect 1 2 3 4) (Rect 4 1 5 5 :: IRect)]

sqDistTests :: Test
sqDistTests = "sqdist" ~: TestList [
  qcTest $ \n -> let r = n `mod` 35 in radiusOf (ofRadius r) == r,
  qcTest $ \n -> let s = SqDist (n `mod` 1007) in ofRadius (radiusOf s) >= s,
  qcTest $ \p1 p2 -> pSqDist p1 p2 == pSqDist p2 p1,
  qcTest $ \p1 p2 -> prectSqDistToPosition (makeRect p1 (1, 1)) p2 ==
                     pSqDist p1 p2,
  qcTest $ \p1 p2 -> prectSqDistToPosition (expandPosition p1) p2 ==
                     prectSqDistToPosition (expandPosition p2) p1]

vectorTests :: Test
vectorTests = "vector" ~: TestList [
  qcTest $ \p -> isNaN (pVectorAngle pZero p),
  qcTest $ \p -> isNaN (pVectorAngle p pZero),
  insistEq 0.0 $ pVectorAngle (Point 2 3) (Point 2 3),
  insistEq 0.0 $ pVectorAngle (Point 3 3) (Point 3 3),
  insistEq pi $ pVectorAngle (Point 3 3) (pNeg $ Point 3 3)]

-------------------------------------------------------------------------------

bpTest :: ((Position, Position) -> Bool) -> Test
bpTest fn = qcTest $ \(p1, p2) -> fn (nearer p1, nearer p2) where
  nearer (Point x y) = Point (smaller x) (smaller y)
  smaller n = (n + 100) `mod` 200 - 100

-------------------------------------------------------------------------------
