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

module Fallback.Test.SparseMap (smTests) where

import Data.Function (on)
import Data.List (nubBy, sort)
import Test.HUnit ((~:), Test(TestList))

import qualified Fallback.Data.SparseMap as SM
import Fallback.Test.Base (qcTest)

-------------------------------------------------------------------------------

smTests :: Test
smTests = "sm" ~: TestList [
  qcTest $ \d -> SM.defaultValue (SM.make d :: SMIC) == d,
  qcTest $ \d -> null (SM.sparseAssocs (SM.make d :: SMIC)),
  qcTest $ \k d -> SM.get k (SM.make d :: SMIC) == d,
  qcTest $ \d as -> SM.defaultValue (SM.fromSparseAssocs d as :: SMIC) == d,
  qcTest $ \d as -> SM.sparseAssocs (SM.fromSparseAssocs d as :: SMIC) ==
                    (sort $ filter ((d /=) . snd) $ nubBy ((==) `on` fst) as),
  qcTest $ \d as -> let sm = SM.fromSparseAssocs d as :: SMIC
                    in read (show sm) == sm]

-------------------------------------------------------------------------------

type SMIC = SM.SparseMap Int Char

-------------------------------------------------------------------------------
