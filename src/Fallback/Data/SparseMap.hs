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

-- | An implementation of total maps from keys to values that is efficient when
-- most keys in the domain type are mapped to the same, \"default\" value.
--
-- Since some function names clash with "Prelude" names, this module is usually
-- imported @qualified@, e.g.
--
-- >  import qualified Fallback.Data.SparseMap as SM

module Fallback.Data.SparseMap
  (-- * SparseMap type
   SparseMap,
   -- * Construction
   make, map, zipWith, fromSparseAssocs,
   -- * Operations
   get, set, adjust, defaultValue, sparseAssocs,
   -- * Debugging
   valid)
where

import Prelude hiding (map, zipWith)

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Text.Read as Read

-------------------------------------------------------------------------------

-- | A total mapping from keys to values; /every/ key has an associated value,
-- even if the key space is infinite.  Under the hood, each 'SparseMap' stores
-- a \"default\" value, and any keys that are associated with this value are
-- not stored (thus, maps with infinite key spaces are possible as long as all
-- but a finite number of keys point to this default value).
--
-- Note: In order to maintain the internal invariant, any function that mutates
-- a 'SparseMap' requires the value type to be an instance of 'Eq'; in
-- particular, the 'map' function has this requirement, so 'SparseMap'
-- unfortunately cannot be an instance of 'Functor'.
data SparseMap k a = SparseMap !a (Map.Map k a)
  deriving (Eq)

instance (Ord k, Eq a, Read k, Read a) => Read (SparseMap k a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "fromSparseAssocs" <- Read.lexP
    d <- Read.readPrec
    as <- Read.readPrec
    return (fromSparseAssocs d as)

instance (Show k, Show a) => Show (SparseMap k a) where
  showsPrec p sm = showParen (p > appPrec) $
                   showString "fromSparseAssocs " .
                   showsPrec (appPrec + 1) (defaultValue sm) .
                   showChar ' ' . showsPrec (appPrec + 1) (sparseAssocs sm)
    where appPrec = 10

-------------------------------------------------------------------------------
-- Construction:

-- | /O(1)/.  Make a new 'SparseMap' with all keys mapping to the given value.
-- The map can then be transformed with the 'set' function; as long as most
-- keys map to the same initial value at any given time, the map will not
-- require very much memory, even if the key space is large or infinite.
make :: a -> SparseMap k a
make d = SparseMap d Map.empty

map :: (Ord k, Eq b) => (a -> b) -> SparseMap k a -> SparseMap k b
map fn (SparseMap d m) = SparseMap d' (Map.mapMaybe fn' m) where
  fn' a = let b = fn a in if b == d' then Nothing else Just b
  d' = fn d

zipWith :: (Ord k, Eq c) => (a -> b -> c) -> SparseMap k a
        -> SparseMap k b -> SparseMap k c
zipWith fn (SparseMap d1 m1) (SparseMap d2 m2) = s' where
  s' = let (s, m2') = foldr put1 (make d', m2) (Map.assocs m1)
       in foldr put2 s (Map.assocs m2')
  put1 (k, v1) (s, m2') =
    case Map.lookup k m2' of
      Just v2 -> (set k (fn v1 v2) s, Map.delete k m2')
      Nothing -> (set k (fn v1 d2) s, m2')
  put2 (k, v2) s = set k (fn d1 v2) s
  d' = fn d1 d2

fromSparseAssocs :: (Ord k, Eq a) => a -> [(k, a)] -> SparseMap k a
fromSparseAssocs d as = foldr (uncurry set) (make d) as

-------------------------------------------------------------------------------
-- Operations:

-- | Get the value associated with the given key.  This function is /O(lg n)/
-- where /n/ is the number of keys associated with values other than the
-- internal \"default\" value.
get :: (Ord k) => k -> SparseMap k a -> a
get k (SparseMap d m) = fromMaybe d (Map.lookup k m)

-- | Change the value associated with the given key.  This function is
-- /O(lg n)/ where /n/ is the number of keys associated with values other than
-- the internal \"default\" value.
set :: (Ord k, Eq a) => k -> a -> SparseMap k a -> SparseMap k a
set k a (SparseMap d m) = SparseMap d m' where
  m' = if a == d then Map.delete k m else Map.insert k a m

-- | Update the value associated with the given key.  This function is
-- /O(lg n)/ where /n/ is the number of keys associated with values other than
-- the internal \"default\" value.
adjust :: (Ord k, Eq a) => (a -> a) -> k -> SparseMap k a -> SparseMap k a
adjust fn k (SparseMap d m) = SparseMap d m' where
  m' = Map.alter (consider . fn . fromMaybe d) k m
  consider a = if a == d then Nothing else Just a

defaultValue :: SparseMap k a -> a
defaultValue (SparseMap d _) = d

sparseAssocs :: SparseMap k a -> [(k, a)]
sparseAssocs (SparseMap _ m) = Map.assocs m

-------------------------------------------------------------------------------
-- Debugging:

-- | Test that the internal invariants hold.
valid :: (Ord k, Eq a) => SparseMap k a -> Bool
valid (SparseMap d m) = Map.valid m && all (d /=) (Map.elems m)

-------------------------------------------------------------------------------
