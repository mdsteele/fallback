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

-- | A 'Couple' is an unordered pair (as opposed to a 2-tuple, which is an
-- ordered pair) of two elements of the same type.  These two elements need not
-- be distinct.  Couples are useful for keeping track of edges in undirected
-- graphs (for example, putting them into a set), among other things.

module Fallback.Data.Couple
  (Couple, makeCouple, fromCouple, flattenCoupleSet)
where

import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readPrec)

-------------------------------------------------------------------------------

-- | An unordered pair of elements of type @a@.  Two 'Couple' objects compare
-- equal if each contains the same two elements, ignoring order.
data Couple a = Couple !a !a
  deriving (Eq, Ord)

instance (Read a, Ord a) => Read (Couple a) where
  readPrec = fmap (uncurry makeCouple) readPrec

instance (Show a) => Show (Couple a) where
  show = show . fromCouple

-- | Create a 'Couple' containing the given elements.
makeCouple :: (Ord a) => a -> a -> Couple a
makeCouple x y = if y < x then Couple y x else Couple x y

-- | Extract the two elements from a 'Couple'.  They may or may not be returned
-- in the same order that they were supplied to 'makeCouple'.
fromCouple :: Couple a -> (a, a)
fromCouple (Couple x y) = (x, y)

-- | Return the set of all values that appear in any couple in the input set.
flattenCoupleSet :: (Ord a) => Set (Couple a) -> Set a
flattenCoupleSet = Set.fromList . concatMap expand . Set.toList where
  expand (Couple x y) = [x, y]

-------------------------------------------------------------------------------
