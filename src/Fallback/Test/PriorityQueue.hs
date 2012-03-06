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

module Fallback.Test.PriorityQueue (pqTests) where

import Data.List (sort, unfoldr)
import Test.HUnit ((~:), Test(TestList))

import qualified Fallback.Data.PriorityQueue as PQ
import Fallback.Test.Base (insist, qcTest)

-------------------------------------------------------------------------------

pqTests :: Test
pqTests = "pq" ~: TestList [
  insist $ PQ.null PQ.empty,
  insist $ PQ.size PQ.empty == 0,
  qcTest $ \k v -> not (PQ.null (PQ.singleton k v :: PQI)),
  qcTest $ \k v -> PQ.size (PQ.singleton k v :: PQI) == 1,
  qcTest $ \k v -> PQ.pop (PQ.singleton k v :: PQI) == Just (v, PQ.empty),
  qcTest $ \k v -> PQ.popWithPriority (PQ.singleton k v :: PQI) ==
                   Just ((k, v), PQ.empty),
  qcTest $ \k v -> PQ.insert k v PQ.empty == (PQ.singleton k v :: PQI),
  qcTest $ \k1 v1 k2 v2 -> PQ.size (PQ.insert k2 v2 $ PQ.insert k1 v1 $
                                    PQ.empty :: PQI) == 2,
  qcTest $ \list -> PQ.size (PQ.fromList list :: PQI) == length list,
  qcTest $ \list -> PQ.elems (pqFromValues list :: PQI) == sort list,
  qcTest $ \list -> let pq = PQ.fromList list :: PQI
                    in PQ.elems pq == unfoldr PQ.pop pq,
  qcTest $ \list -> let pq = PQ.fromList list :: PQI
                    in PQ.assocs pq == unfoldr PQ.popWithPriority pq]

-------------------------------------------------------------------------------

type PQI = PQ.PriorityQueue Int Int

pqFromValues :: (Ord a) => [a] -> PQ.PriorityQueue a a
pqFromValues = foldr (\x -> PQ.insert x x) PQ.empty

-------------------------------------------------------------------------------
