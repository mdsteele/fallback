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

module Fallback.Data.Grid
  (-- * Grid type
   Grid, emptyGrid,
   GridEntry(..),
   GridKey, nullGridKey,
   -- * Query
   gridNull, gridSize, gridEntries,
   gridLookup, gridOccupied, gridSearch, gridSearchRect,
   gridCouldMove,
   -- * Update
   gridTryInsert, gridDelete, gridReplace, gridTryMove,
   gridUpdate, gridUpdateSelect,
   -- * Split/combine
   gridExcise, gridMerge,
   -- * Utility
   rectPositions,
   -- * Debug
   gridValid)
where

import Control.Applicative ((<$>), (<|>))
import Control.Monad (foldM, guard)
import qualified Data.Foldable as Fold
import qualified Data.IntMap as IntMap
import Data.Ix (range)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Traversable as Trav
import qualified Text.Read as Read

import Fallback.Data.Point
import Fallback.Utility (nubKey, square)

-------------------------------------------------------------------------------

-- | A 'Grid' stores a collection of objects, each of which occupies a
-- rectangular section of a grid.  It provides an efficient way to find which
-- object occupies a given grid position, and does not allow more than one
-- object to occupy a position.
data Grid a = Grid
  { gridNextKey :: !(GridKey a),
    gridMap1 :: IntMap.IntMap (GridEntry a),
    gridMap2 :: Map.Map Position (GridKey a) }

instance Functor Grid where
  fmap fn grid = Grid { gridNextKey = rekey (gridNextKey grid),
                        gridMap1 = fmap (fmap fn) (gridMap1 grid),
                        gridMap2 = fmap rekey (gridMap2 grid) }

instance Fold.Foldable Grid where
  foldr fn start grid = foldr fn start $ map geValue $ gridEntries grid

instance Trav.Traversable Grid where
  traverse fn grid = fmap mkGrid $ Trav.traverse fn' $ gridMap1 grid
    where
      mkGrid map1' = Grid { gridNextKey = rekey (gridNextKey grid),
                            gridMap1 = map1',
                            gridMap2 = fmap rekey (gridMap2 grid) }
      fn' ge = let mkEntry value' = ge { geKey = rekey (geKey ge),
                                         geValue = value' }
               in fmap mkEntry $ fn $ geValue ge

instance (Read a) => Read (Grid a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "grid" <- Read.lexP
    entries <- Read.readPrec
    let fn grid entry =
          case gridTryAddEntry entry grid of
            Just grid' -> return grid'
            Nothing -> fail "Grid.readPrec: overlapping entries"
    foldM fn emptyGrid entries

instance (Show a) => Show (Grid a) where
  showsPrec p grid = showParen (p > 10) $
                     showString "grid " .
                     showsPrec 11 (gridEntries grid)

-- | A grid containing no members.
emptyGrid :: Grid a
emptyGrid = Grid
  { gridNextKey = GridKey 1,
    gridMap1 = IntMap.empty,
    gridMap2 = Map.empty }

-- | An entry in a 'Grid'.
data GridEntry a = GridEntry
  { geKey :: GridKey a,
    geRect :: PRect,
    geValue :: a }
  deriving (Eq, Read, Show)

instance Functor GridEntry where
  fmap fn ge = ge { geKey = rekey (geKey ge), geValue = fn (geValue ge) }

-- | The key type for 'Grid' values.
newtype GridKey a = GridKey { fromGridKey :: Int }
  deriving (Eq, Ord, Read, Show)

-- | A 'GridKey' that is never a member of any 'Grid'.
nullGridKey :: GridKey a
nullGridKey = GridKey 0

-------------------------------------------------------------------------------
-- Query:

-- | Return 'True' if the grid contains no entries, 'False' otherwise.
gridNull :: Grid a -> Bool
gridNull = IntMap.null . gridMap1

-- | Return the number of entries in the grid.
gridSize :: Grid a -> Int
gridSize = IntMap.size . gridMap1

-- | Return the list of entries in the grid.
gridEntries :: Grid a -> [GridEntry a]
gridEntries = IntMap.elems . gridMap1

-- | Look up the entry associated with the given key (if any).
gridLookup :: GridKey a -> Grid a -> Maybe (GridEntry a)
gridLookup key grid = IntMap.lookup (fromGridKey key) (gridMap1 grid)

-- | Return 'True' if the given position is occupied by an object, 'False'
-- otherwise.
gridOccupied :: Grid a -> Position -> Bool
gridOccupied grid pos = isJust $ Map.lookup pos $ gridMap2 grid

-- | Find the 'GridEntry' of the object occupying a given position, if any.
gridSearch :: Grid a -> Position -> Maybe (GridEntry a)
gridSearch grid pos =
  case Map.lookup pos (gridMap2 grid) of
    Just key -> case IntMap.lookup (fromGridKey key) (gridMap1 grid) of
                  Just entry -> Just entry
                  Nothing -> error "gridSearch: inconsistent grid"
    Nothing -> Nothing

-- | Find the 'GridEntry' of all objects that intersect the given rect.
gridSearchRect :: Grid a -> PRect -> [GridEntry a]
gridSearchRect grid rect =
  -- Use a different strategy for small/large rects.
  if gridSize grid > square (rectW rect * rectH rect)
  then nubKey geKey $ mapMaybe (gridSearch grid) (rectPositions rect)
  else filter (rectIntersects rect . geRect) (gridEntries grid)

-- | Determine whether the specified item could move to occupy the given
-- rectangle without creating a collision.
gridCouldMove :: GridKey a -> PRect -> Grid a -> Bool
gridCouldMove key rect' grid = isJust (gridTryMove key rect' grid)

-------------------------------------------------------------------------------
-- Update:

-- | Insert a new item into the grid, and return the resulting 'GridEntry',
-- along with the updated 'Grid'.  If the new item collides with any existing
-- items in the grid, return 'Nothing' instead.  Note that with the current
-- implementation, insertion is efficient only for objects occupying small
-- rectangles.
gridTryInsert :: PRect -> a -> Grid a -> Maybe (GridEntry a, Grid a)
gridTryInsert rect value grid = (,) ent <$> gridTryAddEntry ent grid where
  ent = GridEntry { geKey = gridNextKey grid, geRect = rect, geValue = value }
-- gridTryInsert rect value grid =
--   if any (gridOccupied grid) positions then Nothing else Just (entry, grid')
--   where
--     positions = rectPositions rect
--     key = gridNextKey grid
--     intkey = fromGridKey key
--     entry = GridEntry { geKey = key, geRect = rect, geValue = value }
--     grid' = grid { gridNextKey = GridKey (1 + intkey),
--                    gridMap1 = IntMap.insert intkey entry (gridMap1 grid),
--                    gridMap2 = Map.unionWith (flip const) (gridMap2 grid) $
--                               Map.fromList $ map (flip (,) key) positions }

gridTryAddEntry :: GridEntry a -> Grid a -> Maybe (Grid a)
gridTryAddEntry entry grid =
  if IntMap.member intkey (gridMap1 grid) ||
     any (gridOccupied grid) positions
  then Nothing else Just grid' where
    positions = rectPositions (geRect entry)
    key = geKey entry
    intkey = fromGridKey key
    grid' = grid { gridNextKey = max (gridNextKey grid) (GridKey (1 + intkey)),
                   gridMap1 = IntMap.insert intkey entry (gridMap1 grid),
                   gridMap2 = Map.unionWith (flip const) (gridMap2 grid) $
                              Map.fromList $ map (flip (,) key) positions }

-- | Remove an entry from the grid.
gridDelete :: GridKey a -> Grid a -> Grid a
gridDelete (GridKey intkey) grid =
  case IntMap.lookup intkey (gridMap1 grid) of
    Just entry -> grid { gridMap1 = IntMap.delete intkey (gridMap1 grid),
                         gridMap2 = foldr Map.delete (gridMap2 grid) $
                                    rectPositions $ geRect entry }
    Nothing -> grid

-- | Replace an existing entry in the grid, or return the grid unchanged if
-- there is no entry for the given key.
gridReplace :: GridKey a -> a -> Grid a -> Grid a
gridReplace key value grid =
  let fn entry = entry { geValue = value }
  in grid { gridMap1 = IntMap.adjust fn (fromGridKey key) (gridMap1 grid) }

-- | Try to move an item in the grid to occupy a different rectangle.  Return
-- 'Nothing' if the move would create a collision, or if no entry exists for
-- the given key.
gridTryMove :: GridKey a -> PRect -> Grid a -> Maybe (Grid a)
gridTryMove key rect' grid = do
  entry <- gridLookup key grid
  let positions' = rectPositions rect'
  guard $ all (maybe True (key ==) .
               flip Map.lookup (gridMap2 grid)) positions'
  Just grid { gridMap1 = IntMap.insert (fromGridKey key)
                           (entry { geRect = rect' }) (gridMap1 grid),
              gridMap2 = foldr (flip Map.insert key)
                               (foldr Map.delete (gridMap2 grid) $
                                rectPositions $ geRect entry) positions' }

-- | Alter all items in the grid.
gridUpdate :: (a -> a) -> Grid a -> Grid a
gridUpdate fn grid = grid { gridMap1 = IntMap.map fn' (gridMap1 grid) } where
  fn' entry = entry { geValue = fn (geValue entry) }

gridUpdateSelect :: (GridEntry a -> (a, Maybe b)) -> Grid a
                 -> (Grid a, Maybe b)
gridUpdateSelect fn grid = (grid { gridMap1 = map1' }, result) where
  (result, map1') = IntMap.mapAccum fn' Nothing (gridMap1 grid)
  fn' res entry = let (value', res') = fn entry
                  in (res <|> res', entry { geValue = value' })

-------------------------------------------------------------------------------
-- Split/combine:

gridPartition :: (GridEntry a -> Bool) -> Grid a -> (Grid a, Grid a)
gridPartition fn grid = (grid', grid'') where
  (map1', map1'') = IntMap.partition fn (gridMap1 grid)
  grid' = grid { gridMap1 = map1', gridMap2 = regenMap2 map1' }
  grid'' = grid { gridMap1 = map1'', gridMap2 = regenMap2 map1'' }

gridExcise :: PRect -> Grid a -> (Grid a, Grid a)
gridExcise rect grid = gridPartition (rectIntersects rect . geRect) grid

gridMerge :: Grid a -> [GridEntry a] -> (Grid a, [GridEntry a])
gridMerge grid entries = foldl fn (grid, []) entries where
  fn (gr, es) entry = case gridTryAddEntry entry gr of
                        Nothing -> (gr, entry : es)
                        Just gr' -> (gr', es)

-------------------------------------------------------------------------------
-- Utility functions:

rectPositions :: PRect -> [Position]
rectPositions (Rect x y w h) = range (Point x y, Point (x + w - 1) (y + h - 1))

regenMap2 :: IntMap.IntMap (GridEntry a) -> Map.Map Position (GridKey a)
regenMap2 map1 = Map.fromList $ concatMap fn $ IntMap.elems map1 where
  fn entry = map (flip (,) $ geKey entry) $ rectPositions $ geRect entry

rekey :: GridKey a -> GridKey b
rekey = GridKey . fromGridKey

-------------------------------------------------------------------------------
-- Debug:

gridValid :: Grid a -> Bool
gridValid grid = (all assoc1ok $ IntMap.assocs map1) &&
                 (all assoc2ok $ Map.assocs map2)
  where
    assoc1ok (intKey, entry) =
      intKey > fromGridKey nullGridKey &&
      intKey < fromGridKey (gridNextKey grid) &&
      intKey == fromGridKey (geKey entry) &&
      (all ((Just (geKey entry) ==) . flip Map.lookup map2) $
       rectPositions $ geRect entry)
    assoc2ok (pos, GridKey intKey) =
      maybe False (flip rectContains pos . geRect) $ IntMap.lookup intKey map1
    map1 = gridMap1 grid
    map2 = gridMap2 grid

-------------------------------------------------------------------------------
