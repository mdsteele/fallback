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

-- | A 'Multimap' is a finite mapping that can store multiple values for each
-- key (however it will not store more than one copy of a particular key/value
-- pair).
--
-- Since some function names clash with "Prelude" names, this module is usually
-- imported @qualified@, e.g.
--
-- >  import qualified Fallback.Data.Multimap as MM

module Fallback.Data.Multimap
  (-- * 'Multimap' type
   Multimap,
   -- * Construction
   empty, singleton,
   -- * Query
   null, lookup, reverseLookup,
   -- * Update
   map, insert, reverseSet,
   -- * Conversion
   collapse, fromList, toList)
where

import Prelude hiding (lookup, map, null)

import qualified Data.List as List (map)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-------------------------------------------------------------------------------

newtype Multimap k v = Multimap (Map k (Set v))
  deriving (Eq)

-------------------------------------------------------------------------------
-- Construction:

-- | /O(1)/.  The empty multimap.
empty :: Multimap k v
empty = Multimap Map.empty

-- | /O(1)/.  A multimap with a single key/value pair.
singleton :: k -> v -> Multimap k v
singleton k v = Multimap (Map.singleton k (Set.singleton v))

-------------------------------------------------------------------------------
-- Query:

-- | /O(1)/.  Is the map empty?
null :: Multimap k v -> Bool
null (Multimap m) = Map.null m

-- | /O(log n)/.  Look up all values for the given key.
lookup :: (Ord k) => k -> Multimap k v -> Set v
lookup k (Multimap m) = Map.findWithDefault Set.empty k m

-- | /O(n)/.  Look up all keys that map to the given value.
reverseLookup :: (Ord k, Ord v) => v -> Multimap k v -> Set k
reverseLookup v (Multimap m) =
  Set.fromAscList $ List.map fst $ filter (Set.member v . snd) $
  Map.toAscList m

-------------------------------------------------------------------------------
-- Update:

-- | Map a function over the multimap.  Note that the size of the resulting
-- multimap may be smaller, if the function maps multiple inputs onto equal
-- values.
map :: (Ord a, Ord b) => (a -> b) -> Multimap k a -> Multimap k b
map fn (Multimap m) = Multimap (Map.map (Set.map fn) m)

-- | /O(log n)/.  Insert a key/value pair into the map, adding it alongside any
-- existing mappings for the same key.
insert :: (Ord k, Ord v) => k -> v -> Multimap k v -> Multimap k v
insert k v (Multimap m) = Multimap $ Map.alter fn k m where
  fn Nothing = Just (Set.singleton v)
  fn (Just vs) = Just (Set.insert v vs)

-- | Set exactly which keys map to the given value, adding or removing
-- key/value pairs from the multimap as necessary.
reverseSet :: (Ord k, Ord v) => v -> Set k -> Multimap k v -> Multimap k v
reverseSet v ks (Multimap m) =
  foldr (uncurry insert) (Multimap $ Map.map (Set.delete v) m) $
  List.map (flip (,) v) (Set.toList ks)

-------------------------------------------------------------------------------
-- Conversion:

-- | Given a function that collapses all values for a key into a single value,
-- turn a 'Multimap' into a regular 'Map'.  The sets passed to the function are
-- guaranteed to be nonempty.
collapse :: (Set a -> b) -> Multimap k a -> Map k b
collapse fn (Multimap m) = fmap fn m

-- | /O(n log n)/.  Produce a multimap containing the given key/value pairs.
fromList :: (Ord k, Ord v) => [(k, v)] -> Multimap k v
fromList = foldr (uncurry insert) empty

-- | /O(n)/.  Extract all key/value pairs from the multimap.
toList :: Multimap k v -> [(k, v)]
toList (Multimap m) = concatMap fn $ Map.toList m where
  fn (k, vs) = List.map ((,) k) $ Set.toList vs

-------------------------------------------------------------------------------
