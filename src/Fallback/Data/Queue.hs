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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fallback.Data.Queue
  (-- * Queue type
   Queue,
   -- * Query
   null, size,
   -- * Construction
   empty, singleton,
   -- * Update
   insert, pop,
   -- * Conversion
   toList, fromList)
where

import Prelude hiding (null)

import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq

-------------------------------------------------------------------------------
-- Queue type:

-- | A 'Queue' stores a sequence in elements in First-In, First-Out (FIFO)
-- order, allowing efficient 'insert' and 'pop' operations.
newtype Queue a = Queue (Seq.Seq a)
  deriving (Eq, Functor)

-------------------------------------------------------------------------------
-- Query:

-- | /O(1)/.  Is the queue empty?
null :: Queue a -> Bool
null (Queue s) = Seq.null s

-- | /O(1)/.  The number of elements in the queue.
size :: Queue a -> Int
size (Queue s) = Seq.length s

-------------------------------------------------------------------------------
-- Construction:

-- | /O(1)/.  The empty queue.
empty :: Queue a
empty = Queue (Seq.empty)

-- | /O(1)/.  A queue with a single element.
singleton :: a -> Queue a
singleton = Queue . Seq.singleton

-------------------------------------------------------------------------------
-- Update:

-- | /O(1)/.  Append an element to the back of the queue.
insert :: a -> Queue a -> Queue a
insert item (Queue s) = Queue (s Seq.|> item)

-- | /O(1)/.  Remove and return an element from the front of the queue, or
-- return 'Nothing' if the queue is empty.
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue s) =
  case Seq.viewl s of
    Seq.EmptyL -> Nothing
    item Seq.:< s' -> Just (item, Queue s')

-------------------------------------------------------------------------------
-- Conversion:

-- | /O(n)/.  Return a list of all elements in the queue, in the order that
-- they would be popped out.
toList :: Queue a -> [a]
toList (Queue s) = Fold.toList s

-- | /O(n)/.  Build a queue from a list of elements; the elements will be
-- popped out of the queue in the order that they appear in the list.
fromList :: [a] -> Queue a
fromList = Queue . Seq.fromList

-------------------------------------------------------------------------------
