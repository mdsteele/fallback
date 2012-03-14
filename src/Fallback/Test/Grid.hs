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

module Fallback.Test.Grid (gridTests) where

import Data.List (sort)
import Test.HUnit ((~:), Test(TestList))

import Fallback.Data.Point (Point(Point), Rect(Rect))
import qualified Fallback.Data.Grid as Grid
import Fallback.Test.Base (insist, insistAll, insistEq)

-------------------------------------------------------------------------------

gridTests :: Test
gridTests = "grid" ~: TestList [
  insist $ Grid.null Grid.empty,
  insist $ Grid.size Grid.empty == 0,
  insist $ Grid.valid Grid.empty,
  insist $ Grid.occupied grid1 (Point 3 2),
  insistEq (Just 'A') $ fmap Grid.geValue $ Grid.search grid1 (Point 3 2),
  insistEq Nothing $ fmap Grid.geValue $ Grid.search grid1 (Point 4 1),
  insistAll (Grid.rectPositions $ Rect 0 0 7 5) $ \pos ->
    maybe True (\ge -> Grid.lookup (Grid.geKey ge) grid1 == Just ge)
          (Grid.search grid1 pos),
  insistEq "ABCF" $ sort $ map Grid.geValue $ Grid.searchRect grid1 $
                    Rect 3 1 3 3,
  insistEq "AC" $ sort $ map Grid.geValue $ Grid.searchRect grid1 $
                  Rect 3 2 2 1]

-------------------------------------------------------------------------------

--  0123456
-- 0
-- 1  AA BB
-- 2  AAC
-- 3     FD
-- 4   E F
grid1 :: Grid.Grid Char
grid1 = foldr add Grid.empty entries where
  entries = [(2, 1, 2, 2, 'A'), (5, 1, 2, 1, 'B'), (4, 2, 1, 1, 'C'),
             (6, 3, 1, 1, 'D'), (3, 4, 1, 1, 'E'), (5, 3, 1, 2, 'F')]
  add (x, y, w, h, c) grid =
    maybe (error [c]) snd $ Grid.tryInsert (Rect x y w h) c grid

-------------------------------------------------------------------------------
