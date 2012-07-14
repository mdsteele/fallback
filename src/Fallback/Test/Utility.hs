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

module Fallback.Test.Utility (utilityTests) where

import Test.HUnit ((~:), Test(TestList))
import Test.QuickCheck ((==>))

import Fallback.Test.Base (insist, qcTest)
import Fallback.Utility

-------------------------------------------------------------------------------

utilityTests :: Test
utilityTests = "utility" ~: TestList [
  insist $ 0 `ceilDiv` 5 == (0 :: Int),
  insist $ 1 `ceilDiv` 5 == (1 :: Int),
  insist $ 4 `ceilDiv` 5 == (1 :: Int),
  insist $ 5 `ceilDiv` 5 == (1 :: Int),
  insist $ 6 `ceilDiv` 5 == (2 :: Int),
  qcTest $ \a b -> (b > 0) ==> a `ceilDiv` (b :: Integer) ==
                   ceiling (fromIntegral a / fromIntegral b :: Double),
  insist $ isFinite 10000,
  insist $ not $ isFinite (1 / 0),
  insist $ not $ isFinite (0 / 0),
  qcTest $ \a b -> (a > 0) ==> squash a b <= a,
  qcTest $ \a b -> (a > 0) ==> squash a b >= negate a,
  insist $ abs (3 - squash 10000 3) <= 0.0001]

-------------------------------------------------------------------------------
