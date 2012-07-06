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
-- key..
--
-- Since some function names clash with "Prelude" names, this module is usually
-- imported @qualified@, e.g.
--
-- >  import qualified Fallback.Data.Multimap as MM

module Fallback.Data.Multimap
  (Multimap, empty, null, insert, lookup,
   reverseLookup, reverseSet,
   fromList, toList)
where

import Prelude hiding (lookup, null)

import qualified Data.Map as Map

-------------------------------------------------------------------------------

newtype Multimap k v = Multimap (Map.Map k [v])

-- | /O(1)/.  The empty multimap.
empty :: Multimap k v
empty = Multimap Map.empty

-- | /O(1)/.  Is the map empty?
null :: Multimap k v -> Bool
null (Multimap m) = Map.null m

-- | /O(log n)/.  Insert a key/value pair into the map, adding it alongside any
-- existing mappings for the same key.
insert :: (Ord k) => k -> v -> Multimap k v -> Multimap k v
insert k v (Multimap m) = Multimap $ Map.alter fn k m where
  fn Nothing = Just [v]
  fn (Just vs) = Just (v : vs)

-- | /O(log n)/.  Look up all values for the given key.
lookup :: (Ord k) => k -> Multimap k v -> [v]
lookup k (Multimap m) = Map.findWithDefault [] k m

-- | /O(n)/.  Look up all keys that map to the given value.
reverseLookup :: (Eq v) => v -> Multimap k v -> [k]
reverseLookup v = map fst . filter ((v ==) . snd) . toList

-- | Set exactly which keys map to the given value, adding or removing
-- key/value pairs from the multimap as necessary.
reverseSet :: (Ord k, Eq v) => v -> [k] -> Multimap k v -> Multimap k v
reverseSet v ks (Multimap m) =
  foldr (uncurry insert) (Multimap $ Map.map (filter (v /=)) m) $
  map (flip (,) v) ks

-- | /O(n log n)/.  Produce a multimap containing the given key/value pairs.
fromList :: (Ord k) => [(k, v)] -> Multimap k v
fromList = foldr (uncurry insert) empty

-- | /O(n)/.  Extract all key/value pairs from the multimap.
toList :: Multimap k v -> [(k, v)]
toList (Multimap m) = concatMap fn $ Map.toList m where
  fn (k, vs) = map ((,) k) vs

-------------------------------------------------------------------------------
