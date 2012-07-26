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

module Fallback.Test.Couple (coupleTests) where

import qualified Data.Set as Set
import Test.HUnit ((~:), Test(TestList))

import Fallback.Data.Couple
import Fallback.Test.Base (qcTest)

-------------------------------------------------------------------------------

coupleTests :: Test
coupleTests = "couple" ~: TestList [
  qcTest $ \a b -> makeCouple a b == (makeCouple b a :: CI),
  qcTest $ \a b c d e f -> Set.fromList [a, b, c, d, e, f] ==
              flattenCoupleSet (Set.fromList [makeCouple a b, makeCouple c d,
                                              (makeCouple e f :: CI)]),
  qcTest $ \a b -> fromCouple (makeCouple a b :: CI) == (min a b, max a b)]

type CI = Couple Int

-------------------------------------------------------------------------------
