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

-- | A 'Grid' stores a collection of objects, each of which occupies a
-- rectangular section of a grid.  It provides an efficient way to find which
-- object occupies a given grid position, and does not allow more than one
-- object to occupy a position.
--
-- Since some function names clash with "Prelude" names, this module is usually
-- imported @qualified@, e.g.
--
-- >  import qualified Fallback.Data.Grid as Grid

module Fallback.Data.Grid
  (-- * Grid type
   Grid, empty,
   Entry(..),
   Key, nilKey, coerceKey,
   -- * Query
   null, size, keys, values, entries,
   lookup, occupied, search, searchRect,
   couldMove,
   -- * Update
   tryInsert, delete, replace, tryMove,
   update, updateSelect,
   -- * Split/combine
   excise, merge,
   -- * Debug
   valid)
where

import Prelude hiding (lookup, null)

import Control.Applicative ((<$>), (<|>))
import Control.Monad (foldM, guard)
import qualified Data.Foldable as Fold
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Traversable as Trav
import qualified Text.Read as Read

import Fallback.Data.Point
import Fallback.Utility (nubKey, square)

-------------------------------------------------------------------------------

-- | A grid containing values @a@.
data Grid a = Grid
  { gridNextKey :: !(Key a),
    gridMap1 :: IntMap.IntMap (Entry a),
    gridMap2 :: Map.Map Position (Key a) }

instance Functor Grid where
  fmap fn grid = Grid { gridNextKey = coerceKey (gridNextKey grid),
                        gridMap1 = fmap (fmap fn) (gridMap1 grid),
                        gridMap2 = fmap coerceKey (gridMap2 grid) }

instance Fold.Foldable Grid where
  foldr fn start grid = foldr fn start $ map geValue $ entries grid

instance Trav.Traversable Grid where
  traverse fn grid = fmap mkGrid $ Trav.traverse fn' $ gridMap1 grid
    where
      mkGrid map1' = Grid { gridNextKey = coerceKey (gridNextKey grid),
                            gridMap1 = map1',
                            gridMap2 = fmap coerceKey (gridMap2 grid) }
      fn' ge = let mkEntry value' = ge { geKey = coerceKey (geKey ge),
                                         geValue = value' }
               in fmap mkEntry $ fn $ geValue ge

instance (Read a) => Read (Grid a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "grid" <- Read.lexP
    ents <- Read.readPrec
    let fn grid entry =
          case tryAddEntry entry grid of
            Just grid' -> return grid'
            Nothing -> fail "Grid.readPrec: overlapping entries"
    foldM fn empty ents

instance (Show a) => Show (Grid a) where
  showsPrec p grid = showParen (p > 10) $
                     showString "grid " .
                     showsPrec 11 (entries grid)

-- | A grid containing no members.
empty :: Grid a
empty = Grid
  { gridNextKey = Key 1,
    gridMap1 = IntMap.empty,
    gridMap2 = Map.empty }

-- | An entry in a 'Grid'.
data Entry a = Entry
  { geKey :: Key a, -- TODO: rename these to remove the "ge" prefix
    geRect :: PRect,
    geValue :: a }
  deriving (Eq, Read, Show)

instance Functor Entry where
  fmap fn ge = ge { geKey = coerceKey (geKey ge), geValue = fn (geValue ge) }

-- | The key type for 'Grid' values.  The type parameter serves only to help
-- prevent accidentally using a key from one grid on another grid (where it
-- would be invalid); however, if necessary the type of a key can be explicitly
-- changed with 'coerceKey'.
newtype Key a = Key { fromKey :: Int }
  deriving (Eq, Ord, Read, Show)

-- | A key that will never be a member of any grid.
nilKey :: Key a
nilKey = Key 0

-- | Change a key of one type into a key of another type.
coerceKey :: Key a -> Key b
coerceKey = Key . fromKey

-------------------------------------------------------------------------------
-- Query:

-- | Return 'True' if the grid contains no entries, 'False' otherwise.
null :: Grid a -> Bool
null = IntMap.null . gridMap1

-- | Return the number of entries in the grid.
size :: Grid a -> Int
size = IntMap.size . gridMap1

-- | Return the list of keys in the grid, in sorted order.
keys :: Grid a -> [Key a]
keys = map Key . IntMap.keys . gridMap1

-- | Return the list of values in the grid, in key order.
values :: Grid a -> [a]
values = map geValue . IntMap.elems . gridMap1

-- | Return the list of entries in the grid, in key order.
entries :: Grid a -> [Entry a]
entries = IntMap.elems . gridMap1

-- | Look up the entry associated with the given key (if any).
lookup :: Key a -> Grid a -> Maybe (Entry a)
lookup key grid = IntMap.lookup (fromKey key) (gridMap1 grid)

-- | Return 'True' if the given position is occupied by an object, 'False'
-- otherwise.
occupied :: Position -> Grid a -> Bool
occupied pos grid = isJust $ Map.lookup pos $ gridMap2 grid

-- | Find the 'Entry' of the object occupying a given position, if any.
search :: Position -> Grid a -> Maybe (Entry a)
search pos grid =
  case Map.lookup pos (gridMap2 grid) of
    Just key -> case IntMap.lookup (fromKey key) (gridMap1 grid) of
                  Just entry -> Just entry
                  Nothing -> error "Grid.search: inconsistent grid"
    Nothing -> Nothing

-- | Find the 'Entry' of all objects that intersect the given rect.
searchRect :: PRect -> Grid a -> [Entry a]
searchRect rect grid =
  -- Use a different strategy for small/large rects.
  if size grid > square (rectW rect * rectH rect)
  then nubKey geKey $ mapMaybe (flip search grid) (prectPositions rect)
  else filter (rectIntersects rect . geRect) (entries grid)

-- | Determine whether the specified item could move to occupy the given
-- rectangle without creating a collision.
couldMove :: Key a -> PRect -> Grid a -> Bool
couldMove key rect' grid = isJust (tryMove key rect' grid)

-------------------------------------------------------------------------------
-- Update:

-- | Insert a new item into the grid, and return the resulting 'Entry', along
-- with the updated 'Grid'.  If the new item collides with any existing items
-- in the grid, return 'Nothing' instead.  Note that with the current
-- implementation, insertion is efficient only for objects occupying small
-- rectangles.
tryInsert :: PRect -> a -> Grid a -> Maybe (Entry a, Grid a)
tryInsert rect value grid = (,) ent <$> tryAddEntry ent grid where
  ent = Entry { geKey = gridNextKey grid, geRect = rect, geValue = value }

-- | Remove an entry from the grid.  If there is no entry for the key provided,
-- return the grid unchanged.
delete :: Key a -> Grid a -> Grid a
delete (Key intkey) grid =
  case IntMap.lookup intkey (gridMap1 grid) of
    Just entry -> grid { gridMap1 = IntMap.delete intkey (gridMap1 grid),
                         gridMap2 = foldr Map.delete (gridMap2 grid) $
                                    prectPositions $ geRect entry }
    Nothing -> grid

-- | Replace an existing entry in the grid, or return the grid unchanged if
-- there is no entry for the given key.
replace :: Key a -> a -> Grid a -> Grid a
replace key value grid =
  let fn entry = entry { geValue = value }
  in grid { gridMap1 = IntMap.adjust fn (fromKey key) (gridMap1 grid) }

-- | Try to move an item in the grid to occupy a different rectangle.  Return
-- 'Nothing' if the move would create a collision, or if no entry exists for
-- the given key.
tryMove :: Key a -> PRect -> Grid a -> Maybe (Grid a)
tryMove key rect' grid = do
  entry <- lookup key grid
  let positions' = prectPositions rect'
  guard $ all (maybe True (key ==) .
               flip Map.lookup (gridMap2 grid)) positions'
  Just grid { gridMap1 = IntMap.insert (fromKey key)
                           (entry { geRect = rect' }) (gridMap1 grid),
              gridMap2 = foldr (flip Map.insert key)
                               (foldr Map.delete (gridMap2 grid) $
                                prectPositions $ geRect entry) positions' }

-- | Alter all items in the grid.
update :: (Entry a -> a) -> Grid a -> Grid a
update fn grid = grid { gridMap1 = IntMap.map fn' (gridMap1 grid) } where
  fn' entry = entry { geValue = fn entry }

updateSelect :: (Entry a -> (a, Maybe b)) -> Grid a -> (Grid a, Maybe b)
updateSelect fn grid = (grid { gridMap1 = map1' }, result) where
  (result, map1') = IntMap.mapAccum fn' Nothing (gridMap1 grid)
  fn' res entry = let (value', res') = fn entry
                  in (res <|> res', entry { geValue = value' })

-------------------------------------------------------------------------------
-- Split/combine:

gridPartition :: (Entry a -> Bool) -> Grid a -> (Grid a, Grid a)
gridPartition fn grid = (grid', grid'') where
  (map1', map1'') = IntMap.partition fn (gridMap1 grid)
  grid' = grid { gridMap1 = map1', gridMap2 = regenMap2 map1' }
  grid'' = grid { gridMap1 = map1'', gridMap2 = regenMap2 map1'' }

excise :: PRect -> Grid a -> (Grid a, Grid a)
excise rect grid = gridPartition (rectIntersects rect . geRect) grid

merge :: Grid a -> [Entry a] -> (Grid a, [Entry a])
merge grid ents = foldl fn (grid, []) ents where
  fn (gr, es) entry = case tryAddEntry entry gr of
                        Nothing -> (gr, entry : es)
                        Just gr' -> (gr', es)

-------------------------------------------------------------------------------
-- Debug:

-- | Test if the internal grid structure is valid.
valid :: Grid a -> Bool
valid grid = (all assoc1ok $ IntMap.assocs map1) &&
             (all assoc2ok $ Map.assocs map2)
  where
    assoc1ok (intKey, entry) =
      intKey < fromKey (gridNextKey grid) &&
      intKey == fromKey (geKey entry) &&
      (all ((Just (geKey entry) ==) . flip Map.lookup map2) $
       prectPositions $ geRect entry)
    assoc2ok (pos, Key intKey) =
      maybe False (flip rectContains pos . geRect) $ IntMap.lookup intKey map1
    map1 = gridMap1 grid
    map2 = gridMap2 grid

-------------------------------------------------------------------------------
-- Private utility functions:

regenMap2 :: IntMap.IntMap (Entry a) -> Map.Map Position (Key a)
regenMap2 map1 = Map.fromList $ concatMap fn $ IntMap.elems map1 where
  fn entry = map (flip (,) $ geKey entry) $ prectPositions $ geRect entry

tryAddEntry :: Entry a -> Grid a -> Maybe (Grid a)
tryAddEntry entry grid =
  if IntMap.member intkey (gridMap1 grid) || any (flip occupied grid) positions
  then Nothing else Just grid' where
    positions = prectPositions (geRect entry)
    key = geKey entry
    intkey = fromKey key
    grid' = grid { gridNextKey = max (gridNextKey grid) (Key (1 + intkey)),
                   gridMap1 = IntMap.insert intkey entry (gridMap1 grid),
                   gridMap2 = Map.unionWith (flip const) (gridMap2 grid) $
                              Map.fromList $ map (flip (,) key) positions }

-------------------------------------------------------------------------------
