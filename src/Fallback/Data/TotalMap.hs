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

module Fallback.Data.TotalMap
  (-- * TotalMap type
   TotalMap,
   -- * Construction
   makeTotalMap, makeTotalMapA, makeTotalMapM, listTotalMap, unfoldTotalMap,
   tmMapWithKey,
   -- * Operations
   tmGet, tmSet, tmAlter, tmAssocs)
where

import Control.Applicative ((<$>), (<*>), Applicative, pure)
import Control.Exception (assert)
import Control.Monad.ST (runST)
import Data.Array ((!), (//), Array, Ix, assocs, inRange, listArray, range)
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
newtype TotalMap k a = TotalMap (Array k a)
  deriving (Eq, Ord)

instance (Bounded k, Ix k, Read a) => Read (TotalMap k a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "listTotalMap" <- Read.lexP
    xs <- Read.readPrec
    return (listTotalMap xs)

instance (Bounded k, Ix k, Show a) => Show (TotalMap k a) where
  showsPrec p tm = showParen (p > appPrec) $
                   showString "listTotalMap " .
                   showsPrec (appPrec + 1) (Fold.toList tm)
    where appPrec = 10

instance (Bounded k, Ix k) => Functor (TotalMap k) where
  fmap fn tm = makeTotalMap $ \k -> fn (tmGet k tm)

instance (Bounded k, Ix k) => Applicative (TotalMap k) where
  pure = makeTotalMap . const
  tm1 <*> tm2 = makeTotalMap $ \k -> (tmGet k tm1) (tmGet k tm2)

instance (Bounded k, Ix k) => Fold.Foldable (TotalMap k) where
  foldr fn start (TotalMap arr) = Fold.foldr fn start arr
  foldl fn start (TotalMap arr) = Fold.foldl fn start arr

instance (Bounded k, Ix k) => Trav.Traversable (TotalMap k) where
  traverse fn (TotalMap arr) = fromArray <$> Trav.traverse fn arr
    where fromArray a = TotalMap $ Fold.foldr seq a a

-- | Create a new 'TotalMap' with each item initialized by applying the
-- function to the corresponding key.  This function is strict in the return
-- values of the function passed (to ensure that 'tmGet' never evaluates to
-- bottom).
makeTotalMap :: (Bounded k, Ix k) => (k -> a) -> TotalMap k a
makeTotalMap fn = unsafeFromList $ map fn $ range (minBound, maxBound)

-- | Like 'makeTotalMap', but allows the passed function to have side effects
-- (within an arbitrary applicative functor).  The side effects are sequenced
-- in the 'Ix' order of the keys.
makeTotalMapA :: (Bounded k, Ix k, Applicative f) => (k -> f a)
              -> f (TotalMap k a)
makeTotalMapA fn =
  fmap unsafeFromList $ Trav.traverse fn $ range (minBound, maxBound)

-- | Like 'makeTotalMap', but allows the passed function to have side effects
-- (within an arbitrary monad).  The side effects are sequenced in the 'Ix'
-- order of the keys.
makeTotalMapM :: (Bounded k, Ix k, Monad m) => (k -> m a) -> m (TotalMap k a)
makeTotalMapM fn = do
  values <- mapM fn $ range (minBound, maxBound)
  return (unsafeFromList values)

-- | Build a 'TotalMap' by associating list values with respective keys in 'Ix'
-- order.  Extra list items are ignored if the list is too long; this function
-- diverges if the list is too short.
listTotalMap :: (Bounded k, Ix k) => [a] -> TotalMap k a
listTotalMap = unfoldTotalMap fn where
  fn _ (b : bs) = (b, bs)
  fn _ [] = error "listTotalMap: list is too short"

-- | Build a 'TotalMap' by unfolding from an initial seed value.
unfoldTotalMap :: (Bounded k, Ix k) => (k -> s -> (a, s)) -> s -> TotalMap k a
unfoldTotalMap fn initState = runST $ do
  ref <- newSTRef initState
  makeTotalMapM $ \a -> do
    s <- readSTRef ref
    let (b, s') = fn a s
    writeSTRef ref s'
    return b

tmMapWithKey :: (Bounded k, Ix k) => (k -> a -> b) -> TotalMap k a
             -> TotalMap k b
tmMapWithKey fn tm = makeTotalMap $ \k -> fn k (tmGet k tm)

-- | Get an item from a 'TotalMap'.  This function will never diverge (assuming
-- that the key type is well-behaved and that the arguments do not diverge).
tmGet :: (Bounded k, Ix k) => k -> TotalMap k a -> a
tmGet key (TotalMap arr) =
  assert (inRange (minBound, maxBound) key) $ arr ! key

-- | Set an item in a 'TotalMap'.  This function will never diverge (assuming
-- that the key type is well-behaved and that the arguments do not diverge).
tmSet :: (Bounded k, Ix k) => k -> a -> TotalMap k a -> TotalMap k a
tmSet key value (TotalMap arr) =
  assert (inRange (minBound, maxBound) key) $
  value `seq` (TotalMap $ arr // [(key, value)])

-- | Alter an item in a 'TotalMap'.
tmAlter :: (Bounded k, Ix k) => k -> (a -> a) -> TotalMap k a -> TotalMap k a
tmAlter key fn tmap = tmSet key (fn $ tmGet key tmap) tmap

-- | Return the list of associations of a 'TotalMap' in key order.
tmAssocs :: (Bounded k, Ix k) => TotalMap k a -> [(k, a)]
tmAssocs (TotalMap arr) = assocs arr

-------------------------------------------------------------------------------
-- Private:

-- | Build a 'TotalMap' from a list; for internal use only.  Strict in the
-- elements of the list, but /does not/ check that the list is of the correct
-- length.
unsafeFromList :: (Bounded k, Ix k) => [a] -> TotalMap k a
unsafeFromList list =
  TotalMap $ listArray (minBound, maxBound) $ foldr seq list list

-------------------------------------------------------------------------------
