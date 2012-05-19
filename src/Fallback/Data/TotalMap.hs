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

-- | An implementation of total maps from keys to values, with constant-time
-- access and guarantees against divergence.
--
-- Since some function names clash with "Prelude" names, this module is usually
-- imported @qualified@, e.g.
--
-- >  import qualified Fallback.Data.TotalMap as TM

module Fallback.Data.TotalMap
  (-- * TotalMap type
   TotalMap,
   -- * Construction
   make, makeA, makeM, fromList, unfold, mapWithKey,
   -- * Operations
   get, set, adjust, assocs)
where

import Control.Applicative ((<$>), (<*>), Applicative, pure)
import Control.Exception (assert)
import Control.Monad.ST (runST)
import Data.Array ((!), (//), Array, Ix, inRange, listArray, range)
import qualified Data.Array as Array (assocs)
import qualified Data.Foldable as Fold
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Traversable as Trav
import qualified Text.Read as Read

-------------------------------------------------------------------------------

-- | A 'TotalMap' is a key-value map whose key space is a finite set --
-- specifically, a type that is an instance of the 'Ix' and 'Bounded' classes.
-- The abstraction guarantees that a value will always be present in the
-- 'TotalMap' for every element of the key space, and that lookup is
-- constant-time.
--
-- An earlier implementation of this datatype used the 'Enum' typeclass in
-- place of 'Ix'; using 'Ix' has the advantage that tuples (of other valid key
-- types) can be used as the key type for a 'TotalMap'.
--
-- Note: 'TotalMap's are intended for use with key types that are fairly small
-- enumerations (or tuples of several small enumerations).  Although the type
-- system would allow one to create a 'TotalMap' with, say, 'Char' or 'Int' as
-- the key type, doing so is probably a very bad idea (and will likely OOM your
-- program).  If you need a total mapping over a large key space, consider
-- using "Fallback.Data.SparseMap" instead.
newtype TotalMap k a = TotalMap (Array k a)
  deriving (Eq, Ord)

instance (Bounded k, Ix k, Read a) => Read (TotalMap k a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "fromList" <- Read.lexP
    xs <- Read.readPrec
    return (fromList xs)

instance (Bounded k, Ix k, Show a) => Show (TotalMap k a) where
  showsPrec p tm = showParen (p > appPrec) $
                   showString "fromList " .
                   showsPrec (appPrec + 1) (Fold.toList tm)
    where appPrec = 10

instance (Bounded k, Ix k) => Functor (TotalMap k) where
  fmap fn tm = make $ \k -> fn (get k tm)

instance (Bounded k, Ix k) => Applicative (TotalMap k) where
  pure = make . const
  tm1 <*> tm2 = make $ \k -> (get k tm1) (get k tm2)

instance (Bounded k, Ix k) => Fold.Foldable (TotalMap k) where
  foldr fn start (TotalMap arr) = Fold.foldr fn start arr
  foldl fn start (TotalMap arr) = Fold.foldl fn start arr

instance (Bounded k, Ix k) => Trav.Traversable (TotalMap k) where
  traverse fn (TotalMap arr) = fromArray <$> Trav.traverse fn arr
    where fromArray a = TotalMap $ Fold.foldr seq a a

-- | Create a new 'TotalMap' with each item initialized by applying the
-- function to the corresponding key.  This function is strict in the return
-- values of the function passed (to ensure that 'get' never evaluates to
-- bottom).
make :: (Bounded k, Ix k) => (k -> a) -> TotalMap k a
make fn = unsafeFromList $ map fn $ range (minBound, maxBound)

-- | Like 'make', but allows the passed function to have side effects
-- (within an arbitrary applicative functor).  The side effects are sequenced
-- in the 'Ix' order of the keys.
makeA :: (Bounded k, Ix k, Applicative f) => (k -> f a)
              -> f (TotalMap k a)
makeA fn =
  fmap unsafeFromList $ Trav.traverse fn $ range (minBound, maxBound)

-- | Like 'make', but allows the passed function to have side effects
-- (within an arbitrary monad).  The side effects are sequenced in the 'Ix'
-- order of the keys.
makeM :: (Bounded k, Ix k, Monad m) => (k -> m a) -> m (TotalMap k a)
makeM fn = do
  values <- mapM fn $ range (minBound, maxBound)
  return (unsafeFromList values)

-- | Build a 'TotalMap' by associating list values with respective keys in 'Ix'
-- order.  Extra list items are ignored if the list is too long; this function
-- diverges if the list is too short.
fromList :: (Bounded k, Ix k) => [a] -> TotalMap k a
fromList = unfold fn where
  fn _ (b : bs) = (b, bs)
  fn _ [] = error "TotalMap.fromList: list is too short"

-- | Build a 'TotalMap' by unfolding from an initial seed value.  The key
-- values will be visited in 'Ix' order.
unfold :: (Bounded k, Ix k) => (k -> s -> (a, s)) -> s -> TotalMap k a
unfold fn initState = runST $ do
  ref <- newSTRef initState
  makeM $ \a -> do
    s <- readSTRef ref
    let (b, s') = fn a s
    writeSTRef ref s'
    return b

-- | /O(n)/.  Map a function over all values in the 'TotalMap'.
mapWithKey :: (Bounded k, Ix k) => (k -> a -> b) -> TotalMap k a
           -> TotalMap k b
mapWithKey fn tm = make $ \k -> fn k (get k tm)

-- | /O(1)/.  Get an item from a 'TotalMap'.  This function will never diverge
-- (assuming that the key type is well-behaved and that the arguments do not
-- diverge).
get :: (Bounded k, Ix k) => k -> TotalMap k a -> a
get key (TotalMap arr) =
  assert (inRange (minBound, maxBound) key) $ arr ! key

-- | Set an item in a 'TotalMap'.  This function will never diverge (assuming
-- that the key type is well-behaved and that the arguments do not diverge).
set :: (Bounded k, Ix k) => k -> a -> TotalMap k a -> TotalMap k a
set key value (TotalMap arr) =
  assert (inRange (minBound, maxBound) key) $
  value `seq` (TotalMap $ arr // [(key, value)])

-- | Update the value associated with the given key.
adjust :: (Bounded k, Ix k) => k -> (a -> a) -> TotalMap k a -> TotalMap k a
adjust key fn tmap = set key (fn $ get key tmap) tmap

-- | /O(n)/.  Return the list of associations of a 'TotalMap' in key order.
assocs :: (Bounded k, Ix k) => TotalMap k a -> [(k, a)]
assocs (TotalMap arr) = Array.assocs arr

-------------------------------------------------------------------------------
-- Private:

-- | Build a 'TotalMap' from a list; for internal use only.  Strict in the
-- elements of the list, but /does not/ check that the list is of the correct
-- length.
unsafeFromList :: (Bounded k, Ix k) => [a] -> TotalMap k a
unsafeFromList list =
  TotalMap $ listArray (minBound, maxBound) $ foldr seq list list

-------------------------------------------------------------------------------
