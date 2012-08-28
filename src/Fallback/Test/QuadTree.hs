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

module Fallback.Test.QuadTree (quadtreeTests) where

import Test.HUnit ((~:), Test(TestList))
import Test.QuickCheck ((==>))

import qualified Fallback.Data.QuadTree as QT
import Fallback.Test.Base (insist, insistEq, qcTest)

-------------------------------------------------------------------------------

quadtreeTests :: Test
quadtreeTests = "quadtree" ~: TestList [
  insist $ QT.valid QT.empty,
  insist $ QT.empty == QT.empty,
  qcTest $ \p -> not $ QT.member p QT.empty,
  qcTest $ \p -> QT.valid (QT.insert p QT.empty),
  qcTest $ \p -> QT.insert p QT.empty /= QT.empty,
  qcTest $ \p -> QT.insert p QT.empty == QT.insert p QT.empty,
  qcTest $ \p -> QT.member p (QT.insert p QT.empty),
  qcTest $ \p q -> (p /= q) ==> not (QT.member p (QT.insert q QT.empty)),
  qcTest $ \ps -> QT.valid (foldr QT.insert QT.empty ps),
  qcTest $ \ps -> let qt = foldr QT.insert QT.empty ps
                  in all (flip QT.member qt) ps,
  qcTest $ \p ps ->
    (p `notElem` ps) ==> not (QT.member p (foldr QT.insert QT.empty ps)),
  qcTest $ \ps -> foldr QT.insert QT.empty ps ==
                  foldr QT.insert QT.empty (reverse ps),
  insistEq "0e0e0e0e" (show QT.empty),
  insistEq [(QT.empty, "")] (reads "0e0e0e0e"),
  qcTest $ \ps -> let qt = foldr QT.insert QT.empty ps
                  in reads (show qt) == [(qt, "")],
  qcTest $ \ps -> let qt = foldr QT.insert QT.empty ps
                  in reads (show [QT.empty, qt]) == [([QT.empty, qt], "")]]

-------------------------------------------------------------------------------
