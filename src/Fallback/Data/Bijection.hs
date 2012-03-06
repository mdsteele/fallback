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

module Fallback.Data.Bijection
  (Bijection, make, getA, getB, invert)
where

import Data.Array ((!), Array, Ix, assocs, listArray, range)
import Data.List (foldl')
import qualified Data.Map as Map

-------------------------------------------------------------------------------

data Bijection a b = Bijection !(Array a b) !(Array b a)

make :: (Bounded a, Bounded b, Ix a, Ix b) => (a -> b) -> Bijection a b
make fn = Bijection arr1 arr2 where
  list1 = map fn $ range (minBound, maxBound)
  arr1 = listArray (minBound, maxBound) $ foldr seq list1 list1
  collide _ _ = error "Bijection.make: collision"
  add m (a, b) = Map.insertWith collide b a m
  map2 = foldl' add Map.empty $ assocs arr1
  arr2 = listArray (minBound, maxBound) $ Map.elems map2

-- | /O(1)/.  Look up the @a@ value for the corresponding @b@ value.
getA :: (Ix b) => Bijection a b -> b -> a
getA (Bijection _ arr) key = arr ! key

-- | /O(1)/.  Look up the @b@ value for the corresponding @a@ value.
getB :: (Ix a) => Bijection a b -> a -> b
getB (Bijection arr _) key = arr ! key

-- | /O(1)/.  Produce the inverse bijection.
invert :: Bijection a b -> Bijection b a
invert (Bijection arr1 arr2) = Bijection arr2 arr1

-------------------------------------------------------------------------------
