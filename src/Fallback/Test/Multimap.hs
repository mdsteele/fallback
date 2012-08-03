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

module Fallback.Test.Multimap (multimapTests) where

import Data.List (nub, sort)
import qualified Data.Set as Set
import Test.HUnit ((~:), Test(TestList))

import qualified Fallback.Data.Multimap as MM
import Fallback.Test.Base (insist, qcTest)

-------------------------------------------------------------------------------

multimapTests :: Test
multimapTests = "multimap" ~: TestList [
  insist $ MM.null MM.empty,
  qcTest $ \k v -> not $ MM.null $ MM.insert k v (MM.empty :: MMI),
  qcTest $ \k v -> MM.singleton k v == MM.insert k v (MM.empty :: MMI),
  qcTest $ \k v ->
    MM.lookup k (MM.insert k v (MM.empty :: MMI)) == Set.singleton v,
  qcTest $ \k v ->
    MM.reverseLookup v (MM.insert k v (MM.empty :: MMI)) == Set.singleton k,
  qcTest $ \list -> MM.toList (MM.fromList list :: MMI) == nub (sort list),
  qcTest $ \v ks ->
    MM.reverseLookup v (MM.reverseSet v (Set.fromList ks) (MM.empty :: MMI)) ==
    Set.fromList ks]

type MMI = MM.Multimap Int Int

-------------------------------------------------------------------------------
