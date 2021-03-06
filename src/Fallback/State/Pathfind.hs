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

module Fallback.State.Pathfind
  (allPathsFrom, pathfind, pathfindToRect, pathfindRectToRange,
   pathfindRectToRanges)
where

import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Fallback.Data.Point
import qualified Fallback.Data.PriorityQueue as PQ
import qualified Fallback.Data.Queue as Queue

-------------------------------------------------------------------------------

-- | Lazily compute the shortest path to each and every position reachable from
-- the start position, sorted from fewest steps to most.
allPathsFrom :: [Direction] {-^ directions to use -}
             -> (Position -> Bool) {-^ Is this position blocked? -}
             -> Position {-^ start position -} -> [[Position]]
allPathsFrom dirs isBlocked start = expand Set.empty Queue.empty start [] where
  step visited queue = flip (maybe []) (Queue.pop queue) $
                       \((pos, path), queue') ->
    reverse path : expand visited queue' pos path
  expand visited queue pos path =
    let children = filter (\p -> Set.notMember p visited &&
                           not (isBlocked p)) $ map (pos `plusDir`) dirs
        addToQueue child queue' = Queue.insert (pos, child : path) queue'
    in step (foldr Set.insert visited children)
            (foldr addToQueue queue children)

-- | Find a path from the start position to a goal position.  The return value
-- will be a sequence of positions from the start to the goal, including the
-- goal (as the last element) but /not/ including the start position.  If the
-- goal cannot be reached, return 'Nothing'; if the start position is already a
-- goal position, return 'Just'@ []@.
pathfind :: (Position -> Bool) {-^ Is this position blocked? -}
         -> (Position -> Bool) {-^ Is this position a goal position? -}
         -> (Position -> Double) {-^ heuristic distance to goal from here -}
         -> Int {-^ max number of steps -}
         -> Position {-^ start position -}
         -> Maybe [Position] -- path, not including start
pathfind isBlocked isGoal heuristic limit start =
  step (Set.singleton start) (PQ.singleton (heuristic start) (start, 0, 0, []))
  where
    step visited queue = PQ.pop queue >>= \((pos, dist, len, path), queue') ->
      let len' = len + 1
          addChild (child, dist') =
            if remain > fromIntegral (limit - len') * sqrt 2 then id
            else PQ.insert (dist' + remain) (child, dist', len', child : path)
            where remain = heuristic child -- See [Note: Heuristic] below
          children = flip mapMaybe allDirections $ \dir ->
            let pos' = pos `plusDir` dir
            in if Set.notMember pos' visited && not (isBlocked pos')
               then Just (pos', dist + dirDist dir) else Nothing
      in if isGoal pos then Just $ reverse path
         else if len >= limit then step visited queue'
              else step (foldr Set.insert visited $ map fst children)
                        (foldr addChild queue' children)

    dirDist :: Direction -> Double
    dirDist DirE = 1
    dirDist DirN = 1
    dirDist DirW = 1
    dirDist DirS = 1
    dirDist _ = sqrt 2

-- [Note: Heuristic] The heuristic gives a lower bound on the distance
-- remaining to the goal.  If the maximum distance we could possibly cover with
-- the remaining steps we have is less than that, then we'll never get to the
-- goal from this child, so don't bother adding it to the queue.  This
-- optimization is important for when limit is high; otherwise, we will
-- completely explore all spaces within limit even when the goal is much too
-- far away.

pathfindToRect :: (Position -> Bool) -> PRect -> Int -> Position
               -> Maybe [Position]
pathfindToRect isBlocked rect =
  pathfind isBlocked (rectContains rect) heuristic where
    heuristic pos =
      let pos' = fromIntegral <$> pos
          rect' = fromIntegral <$> rect
          (w, h) = rectSize rect'
      in (pDist pos' $ rectCenter rect') - 0.5 * (sqrt $ w*w + h*h)

pathfindToRange :: (Position -> Bool) -> Position -> SqDist -> Int
                -> Position -> Maybe [Position]
pathfindToRange isBlocked goal sqDist =
  pathfind isBlocked ((sqDist >=) . pSqDist goal)
           (max 0 . subtract (sqDistRadius sqDist) .
            pDist (fromIntegral <$> goal) . fmap fromIntegral)

pathfindToRanges :: (Position -> Bool) -> [Position] -> SqDist -> Int
                 -> Position -> Maybe [Position]
pathfindToRanges _isBlocked [] _sqDist _limit _start = Nothing
pathfindToRanges isBlocked [goal] sqDist limit start =
  pathfindToRange isBlocked goal sqDist limit start
pathfindToRanges isBlocked goals sqDist limit start =
  pathfind isBlocked isGoal heuristic limit start
  where
    isGoal pos = any (\goal -> sqDist >= pSqDist pos goal) goals
    heuristic pos = max 0 $ subtract (sqDistRadius sqDist) $
                    minimum $ map (distTo pos) goals
    pos `distTo` goal = sqDistRadius $ pSqDist pos goal

pathfindSizeToRange :: (Position -> Bool) -> (Int, Int) -> Position -> SqDist
                    -> Int -> Position -> Maybe [Position]
pathfindSizeToRange isBlocked size goal sqDist =
  pathfindSizeToRanges isBlocked size [goal] sqDist
--   pathfind isBlocked isGoal (max 0 . subtract radius . pDist goal' .
--                              pAdd offset . fmap fromIntegral)
--   where
--     isGoal = rangeTouchesRect goal sqDist . flip makeRect (w, h)
--     goal' = fromIntegral <$> goal
--     radius = (sqrt $ fromIntegral sqDist) +
--              (sqrt $ fromIntegral $ w * w + h * h)
--     offset = Point (fromIntegral (w - 1) / 2) (fromIntegral (h - 1) / 2)

pathfindSizeToRanges :: (Position -> Bool) -> (Int, Int) -> [Position]
                     -> SqDist -> Int -> Position -> Maybe [Position]
pathfindSizeToRanges isBlocked (1, 1) goals sqDist limit start =
  pathfindToRanges isBlocked goals sqDist limit start
pathfindSizeToRanges _isBlocked _size [] _sqDist _limit _start = Nothing
-- pathfindSizeToRanges isBlocked size [goal] sqDist =
--   pathfindSizeToRange isBlocked size goal sqDist
pathfindSizeToRanges isBlocked (w, h) goals sqDist limit start =
  pathfind isBlocked isGoal heuristic limit start
  where
    isGoal pos =
      any ((sqDist >=) . prectSqDistToPosition (makeRect pos (w, h))) goals
    heuristic pos = max 0 $ subtract radius $ minimum $ map (distTo pos) goals
    pos `distTo` goal = pDist (fromIntegral <$> goal) $
                        pAdd offset (fromIntegral <$> pos)
    radius = sqDistRadius sqDist + (sqrt $ fromIntegral $ w * w + h * h)
    offset = Point (fromIntegral (w - 1) / 2) (fromIntegral (h - 1) / 2)

pathfindRectToRange :: (Position -> Bool) -> PRect -> Position -> SqDist
                    -> Int -> Maybe [Position]
pathfindRectToRange isBlocked rect goal sqDist limit =
  pathfindSizeToRange isBlocked (rectSize rect) goal sqDist limit
                      (rectTopleft rect)

pathfindRectToRanges :: (Position -> Bool) -> PRect -> [Position] -> SqDist
                     -> Int -> Maybe [Position]
pathfindRectToRanges isBlocked rect goals sqDist limit =
  pathfindSizeToRanges isBlocked (rectSize rect) goals sqDist limit
                       (rectTopleft rect)

-------------------------------------------------------------------------------
