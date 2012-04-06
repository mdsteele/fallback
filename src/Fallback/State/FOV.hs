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

-- | This module implements the Precise Permissive Field of View algorithm.  It
-- is a Haskell port of a piece of Python code by Aaron MacDonald.  The
-- original code was found at
-- <http://roguebasin.roguelikedevelopment.org/index.php?title=Permissive_Field_of_View_in_Python>.
--
-- The original file contained the below notice:
--
-- > Author:         Aaron MacDonald
-- > Date:           June 14, 2007
-- >
-- > Description:    An implementation of the precise permissive field
-- >                 of view algorithm for use in tile-based games.
-- >                 Based on the algorithm presented at
-- >                 http://roguebasin.roguelikedevelopment.org/
-- >                   index.php?title=
-- >                   Precise_Permissive_Field_of_View.
-- >
-- > You are free to use or modify this code as long as this notice is
-- > included.
-- > This code is released without warranty.
--
-- Much thanks to Aaron for releasing his code.

module Fallback.State.FOV (fieldOfView, lineOfSight) where

import Control.Applicative ((<$>))
import Control.Monad (ap, foldM, liftM2, when)
import Control.Monad.Fix (mfix)
import Control.Monad.ST (ST, runST)
import qualified Data.Set as Set
import Data.STRef

import Fallback.Data.Point (IPoint, Point(..), Position, SqDist(..), pAdd)
import Fallback.Utility (flip3, whenM)

-------------------------------------------------------------------------------

-- | Compute the field of view of a viewer, adding the coordinates of all
-- visible tiles to the set of already visible tiles.  The required arguments
-- are the width and height of the map, a function that determines whether a
-- tile at a given position blocks vision, the square of the maximum vision
-- radius, the position of the viewer on the map, and the set to add
-- coordinates to.
fieldOfView :: (Int, Int) {-^map size-}
            -> (Position -> Bool) {-^isBlocked function-}
            -> SqDist {-^radius squared-} -> Position {-^viewer position-}
            -> Set.Set Position {-^already visible-} -> Set.Set Position
fieldOfView (width, height) isBlocked (SqDist radiusSquared) start visible =
  let radius = floor $ sqrt (fromIntegral radiusSquared :: Double)
      minX = min radius $ pointX start
      maxX = min radius $ width - pointX start - 1
      minY = min radius $ pointY start
      maxY = min radius $ height - pointY start - 1
      check = checkQuadrent isBlocked radiusSquared start
  in runST $ check (-1) 1 minX maxY =<< check (-1) (-1) minX minY =<<
     check 1 (-1) maxX minY =<< check 1 1 maxX maxY (Set.insert start visible)

-- | Determine whether two positions can see each other (assuming an unlimited
-- vision range).  This is more efficient than calling 'fieldOfView' from one
-- position and checking if the other position is in the set; however, calling
-- 'fieldOfView' is more efficient than calling this function many times for
-- many positions.
lineOfSight :: (Position -> Bool) {-^isBlocked function-} -> Position
            -> Position -> Bool
lineOfSight isBlocked p1@(Point x1 y1) p2@(Point x2 y2) =
  if x1 == x2 then
    if y1 == y2 then True
    else not $ any isBlocked $ map (Point x1) $ subrange y1 y2
  else
    if y1 == y2 then not $ any isBlocked $ map (flip Point y1) $ subrange x1 x2
    else runST $ do
      let { dx = x2 - x1; dy = y2 - y1 }
      visible <- checkQuadrent isBlocked (dx * dx + dy * dy) p1 (signum dx)
                               (signum dy) (abs dx) (abs dy) Set.empty
      return (Set.member p2 visible)
  where subrange a b = [(min a b + 1) .. (max a b - 1)]

checkQuadrent :: (Position -> Bool) -> Int -> Position -> Int -> Int
              -> Int -> Int -> Set.Set Position -> ST s (Set.Set Position)
checkQuadrent isBlocked radSq start dX dY maxX maxY vis = do
  viewList <- newViewList maxX maxY
  -- Visit the tiles diagonally and going outwards, like so:
  -- .
  -- .
  -- .           .
  -- 9        .
  -- 5  8  .
  -- 2  4  7
  -- @  1  3  6  .  .  .
  flip3 foldM vis [1 .. maxX + maxY] $ \vis' i -> do
    flip3 foldM vis' [max 0 (i - maxX) .. min i maxY] $ \visible y -> do
      let x = i - y
      if (x * x + y * y > radSq) then return visible else do
        -- Look for a view that contains this tile.
        mbView <- firstView viewList >>= findView (Point x y)
        -- If no view contains this tile, we can't see it, and we move on.
        flip (maybe (return visible)) mbView $ \view -> do
          -- We can see the tile; determine its absolute coordinates.
          let tile = start `pAdd` Point (x * dX) (y * dY)
          -- If this tile is opaque, we need to alter the view.
          when (isBlocked tile) $ do
            shallowLine <- readSTRef (viewShallowLine view)
            steepLine <- readSTRef (viewSteepLine view)
            above <- shallowLine `abovePoint` Point (x + 1) y
            below <- steepLine `belowPoint` Point x (y + 1)
            if above then
              if below then do
                -- The current coordinate is intersected by both lines in the
                -- current view.  The view is completely blocked.
                removeView view
              else do
                -- The current coordinate is intersected by the shallow line of
                -- the current view.  The shallow line needs to be raised.
                addShallowBump (Point x (y + 1)) view
             else
              if below then do
                -- The current coordinate is intersected by the steep line of
                -- the current view.  The steep line needs to be lowered.
                addSteepBump (Point (x + 1) y) view
              else do
                -- The current coordinate is completely between the two lines
                -- of the current view.  Split the current view into two views
                -- above and below the current coordinate.
                view' <- duplicateView view
                addSteepBump (Point (x + 1) y) view
                addShallowBump (Point x (y + 1)) view'
          -- Whether or not this tile was blocked, mark that we can see it.
          return (Set.insert tile visible)

-- | Find a view that contains the given position.
findView :: IPoint -> Maybe (View s) -> ST s (Maybe (View s))
findView (Point x y) mbView = do
  mbView' <- findAbove (Point (x + 1) y) mbView
  flip (maybe (return Nothing)) mbView' $ \view -> do
    -- The position is below this view's steep line, so check if it's also
    -- above the view's shallow line.
    shallow <- readSTRef (viewShallowLine view)
    aoc <- shallow `abovePointOrColinear` (Point x (y + 1))
    return $ if aoc then Nothing else Just view

-- | Find a view whose steep line is above the given position.
findAbove :: IPoint -> Maybe (View s) -> ST s (Maybe (View s))
findAbove _ Nothing = return Nothing
findAbove pos (Just view) = do
  steep <- readSTRef (viewSteepLine view)
  boc <- steep `belowPointOrColinear` pos
  if not boc then return (Just view) else
    -- The current coordinate is above the current view and is ignored.  The
    -- steeper fields may need it though.
    nextView view >>= findAbove pos

-------------------------------------------------------------------------------

-- | A doubly-linked list of 'View' objects.
data ViewList s = ViewList
  { vlistFirst :: ViewLink s,
    vlistLast :: ViewLink s }

-- | Get the first view in the given list (or 'Nothing' if the list is empty).
firstView :: ViewList s -> ST s (Maybe (View s))
firstView vlist = either (const Nothing) Just <$> readSTRef (vlistFirst vlist)

-- | Create a new 'ViewList' containing a single 'View' with the given maximum
-- x and y values.
newViewList :: Int -> Int -> ST s (ViewList s)
newViewList maxX maxY = mfix $ \vlist -> do
  shallow <- newLine (Point 0 1) (Point (maxX + 1) 0)
  steep <- newLine (Point 1 0) (Point 0 (maxY + 1))
  view <- return View `ap` (newSTRef shallow) `ap` (newSTRef steep)
                      `ap` (newSTRef []) `ap` (newSTRef [])
                      `ap` (newSTRef $ Left vlist) `ap` (newSTRef $ Left vlist)
  liftM2 ViewList (newSTRef $ Right view) (newSTRef $ Right view)

-------------------------------------------------------------------------------

-- | A link in a 'ViewList'.
type ViewLink s = STRef s (Either (ViewList s) (View s))

pointPrevAt :: ViewLink s -> Either (ViewList s) (View s) -> ST s ()
pointPrevAt vlink target =
  (either vlistLast viewPrev <$> readSTRef vlink) >>= flip writeSTRef target

pointNextAt :: ViewLink s -> Either (ViewList s) (View s) -> ST s ()
pointNextAt vlink target =
  (either vlistFirst viewNext <$> readSTRef vlink) >>= flip writeSTRef target

-------------------------------------------------------------------------------

data View s = View
  { viewShallowLine :: STRef s (Line s),
    viewSteepLine :: STRef s (Line s),
    viewShallowBumps :: STRef s [IPoint],
    viewSteepBumps :: STRef s [IPoint],
    viewPrev :: ViewLink s,
    viewNext :: ViewLink s }

-- | Get the next view in the list after the given view, if any.
nextView :: View s -> ST s (Maybe (View s))
nextView view = either (const Nothing) Just <$> readSTRef (viewNext view)

-- | Create a deep copy of the given view, insert the copy into the list just
-- after the view, and return the copy.
duplicateView :: View s -> ST s (View s)
duplicateView view = do
  shallow' <- readSTRef (viewShallowLine view) >>= cloneLine
  steep' <- readSTRef (viewSteepLine view) >>= cloneLine
  -- Create a new view, with prev pointing to the given view and next pointing
  -- to the next view after the given view.
  view' <- return View `ap` (newSTRef shallow') `ap` (newSTRef steep')
                       `ap` (cloneSTRef $ viewShallowBumps view)
                       `ap` (cloneSTRef $ viewSteepBumps view)
                       `ap` (newSTRef $ Right view)
                       `ap` (cloneSTRef $ viewNext view)
  -- Set prev of the next view to point to the clone.
  (viewNext view') `pointPrevAt` (Right view')
  -- Set next of the given view to point to the clone.
  writeSTRef (viewNext view) (Right view')
  return view'

-- | Remove the view from the list.
removeView :: View s -> ST s ()
removeView view = do
  -- Set next of the previous view to point at the next view.
  (viewPrev view `pointNextAt`) =<< readSTRef (viewNext view)
  -- Set prev of the next view to point at the previous view.
  (viewNext view `pointPrevAt`) =<< readSTRef (viewPrev view)

addShallowBump :: IPoint -> View s -> ST s ()
addShallowBump pos view = do
  shallowLine <- readSTRef (viewShallowLine view)
  writeSTRef (lineEnd shallowLine) pos
  modifySTRef (viewShallowBumps view) (pos :)
  loopM (readSTRef $ viewSteepBumps view) $ \bump -> do
    whenM (shallowLine `abovePoint` bump) $ do
      writeSTRef (lineBegin shallowLine) bump
  checkView view

addSteepBump :: IPoint -> View s -> ST s ()
addSteepBump pos view = do
  steepLine <- readSTRef (viewSteepLine view)
  writeSTRef (lineEnd steepLine) pos
  modifySTRef (viewSteepBumps view) (pos :)
  loopM (readSTRef $ viewShallowBumps view) $ \bump -> do
    whenM (steepLine `belowPoint` bump) $ do
      writeSTRef (lineBegin steepLine) bump
  checkView view

-- | Remove the view from the list if the two lines are colinear and they pass
-- through either of the starting corners.
checkView :: View s -> ST s ()
checkView view = do
  shallow <- readSTRef (viewShallowLine view)
  steep <- readSTRef (viewSteepLine view)
  whenM (linesColinear shallow steep `andM`
         ((shallow `colinearWithPoint` Point 0 1) `orM`
          (shallow `colinearWithPoint` Point 1 0))) $ do
    removeView view

-------------------------------------------------------------------------------

data Line s = Line
  { lineBegin :: STRef s IPoint,
    lineEnd :: STRef s IPoint }

newLine :: IPoint -> IPoint -> ST s (Line s)
newLine begin end = liftM2 Line (newSTRef begin) (newSTRef end)

cloneLine :: Line s -> ST s (Line s)
cloneLine line =
  liftM2 Line (cloneSTRef $ lineBegin line) (cloneSTRef $ lineEnd line)

compareToPoint :: (Int -> Int -> Bool) -> Line s -> IPoint -> ST s Bool
compareToPoint fn line (Point x y) = do
  Point xb yb <- readSTRef (lineBegin line)
  Point xe ye <- readSTRef (lineEnd line)
  return $ fn ((xe - xb) * (ye - y)) ((ye - yb) * (xe - x))

belowPoint :: Line s -> IPoint -> ST s Bool
belowPoint = compareToPoint (<)

belowPointOrColinear :: Line s -> IPoint -> ST s Bool
belowPointOrColinear = compareToPoint (<=)

colinearWithPoint :: Line s -> IPoint -> ST s Bool
colinearWithPoint = compareToPoint (==)

abovePointOrColinear :: Line s -> IPoint -> ST s Bool
abovePointOrColinear = compareToPoint (>=)

abovePoint :: Line s -> IPoint -> ST s Bool
abovePoint = compareToPoint (>)

linesColinear :: Line s -> Line s -> ST s Bool
linesColinear line1 line2 = do
  pb <- readSTRef (lineBegin line2)
  pe <- readSTRef (lineEnd line2)
  (line1 `colinearWithPoint` pb) `andM` (line1 `colinearWithPoint` pe)

-------------------------------------------------------------------------------

andM, orM :: (Monad m) => m Bool -> m Bool -> m Bool
andM m1 m2 = do { b1 <- m1; if b1 then m2 else return False }
orM m1 m2 = do { b1 <- m1; if b1 then return True else m2 }

loopM :: (Monad m) => m [a] -> (a -> m ()) -> m ()
loopM list fn = list >>= mapM_ fn

cloneSTRef :: STRef s a -> ST s (STRef s a)
cloneSTRef ref = readSTRef ref >>= newSTRef

-------------------------------------------------------------------------------
