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

module Fallback.Data.PriorityQueue
  (-- * Priority queue type
   PriorityQueue,
   -- * Query
   null, size,
   -- * Construction
   empty, singleton,
   -- * Update
   insert, pop, popWithPriority,
   -- * Conversion
   elems, assocs, fromList)
where

import Prelude hiding (null)

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------
-- Priority queue type:

-- | A priority queue with values @a@ sorted by priority @p@.  This is a
-- min-queue, so items with smaller priority values will be popped out before
-- items with larger priority values.
newtype PriorityQueue p a = PriorityQueue (Map.Map p [a])
  deriving (Eq)

instance Functor (PriorityQueue p) where
  fmap fn (PriorityQueue qmap) = PriorityQueue (fmap (map fn) qmap)

-------------------------------------------------------------------------------
-- Query:

-- | /O(1)/.  Is the queue empty?
null :: PriorityQueue p a -> Bool
null (PriorityQueue qmap) = Map.null qmap

-- | /O(1)/.  The number of elements in the queue.
size :: PriorityQueue p a -> Int
size (PriorityQueue qmap) = sum $ map length $ Map.elems qmap

-------------------------------------------------------------------------------
-- Construction:

-- | /O(1)/.  The empty priority queue.
empty :: PriorityQueue p a
empty = PriorityQueue Map.empty

-- | /O(1)/.  A queue with a single element.
singleton :: p -> a -> PriorityQueue p a
singleton pri value = PriorityQueue (Map.singleton pri [value])

-------------------------------------------------------------------------------
-- Update:

-- | /O(log n)/.  Insert a new item into the queue with the given priority.  If
-- one or more items of equal priority are already in the queue, the new item
-- will come back out /before/ any of them.
insert :: (Ord p) => p -> a -> PriorityQueue p a -> PriorityQueue p a
insert pri value (PriorityQueue qmap) =
  PriorityQueue (Map.alter (Just . (value :) . fromMaybe []) pri qmap)

-- | /O(log n)/.  Remove and return the item with the smallest priority value,
-- or return 'Nothing' if the queue is empty.
pop :: (Ord p) => PriorityQueue p a -> Maybe (a, PriorityQueue p a)
pop (PriorityQueue qmap) =
  case Map.minView qmap of
    Nothing -> Nothing
    Just ([], qmap') -> pop (PriorityQueue qmap')
    Just ([value], qmap') -> Just (value, PriorityQueue qmap')
    Just (value : values, _) ->
      Just (value, PriorityQueue (Map.updateMin (const $ Just values) qmap))

-- | /O(log n)/.  Remove and return the (priority, item) pair with the smallest
-- priority value, or return 'Nothing' if the queue is empty.
popWithPriority :: (Ord p) => PriorityQueue p a
                -> Maybe ((p, a), PriorityQueue p a)
popWithPriority (PriorityQueue qmap) =
  case Map.minViewWithKey qmap of
    Nothing -> Nothing
    Just ((_, []), qmap') -> popWithPriority (PriorityQueue qmap')
    Just ((pri, [value]), qmap') -> Just ((pri, value), PriorityQueue qmap')
    Just ((pri, value : values), _) ->
      Just ((pri, value),
            PriorityQueue (Map.updateMin (const $ Just values) qmap))

-------------------------------------------------------------------------------
-- Conversion:

-- | /O(n)/.  Return all elements of the queue, in the order they would come
-- out if popped one at a time.  Equivalent to @unfoldr pop@, but more
-- efficient.
elems :: PriorityQueue p a -> [a]
elems (PriorityQueue qmap) = concat (Map.elems qmap)

-- | /O(n)/.  Return all (priority, item) pairs in the queue, in the order they
-- would come out if popped one at a time.  Equivalent to @unfoldr
-- popWithPriority@, but more efficient.
assocs :: PriorityQueue p a -> [(p, a)]
assocs (PriorityQueue qmap) = concatMap fn $ Map.assocs qmap where
  fn (pri, values) = map ((,) pri) values

-- | /O(n*log n)/.  Create a priority queue from the given list of (priority,
-- item) pairs.
fromList :: (Ord p) => [(p, a)] -> PriorityQueue p a
fromList = foldr (uncurry insert) empty

-------------------------------------------------------------------------------
