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

module Fallback.Test.Pathfind (pathfindTests) where

import Control.Applicative ((<$>))
import Data.Array
import Data.List (transpose, unfoldr)
import Data.Maybe (listToMaybe)
import Test.HUnit ((~:), Test(TestList))

import Fallback.Data.Point
import Fallback.State.Pathfind
import Fallback.Test.Base (insistEq)

-------------------------------------------------------------------------------

pathfindTests :: Test
pathfindTests = "pathfind" ~: TestList [

  pathfindTest 10
    "OOOOOOO\n\
    \O  e gO\n\
    \O  .  O\n\
    \OOO.  O\n\
    \O .   O\n\
    \Os    O\n\
    \OOOOOOO",

  pathfindFail 3
    "OOOOOOO\n\
    \O  g gO\n\
    \O     O\n\
    \OOO   O\n\
    \O     O\n\
    \Os    O\n\
    \OOOOOOO",

  pathfindTest 20
    "OOOOOOOO\n\
    \O    e O\n\
    \O ...  O\n\
    \O.OOOOOO\n\
    \O ..   O\n\
    \OOOO.  O\n\
    \O ..   O\n\
    \Os     O\n\
    \OOOOOOOO",

  pathfindTest 20
    "OOOOOOOO\n\
    \O  ..e O\n\
    \O .O   O\n\
    \O.OOOOOO\n\
    \O ..   O\n\
    \OOOO.  O\n\
    \O ..   O\n\
    \Os     O\n\
    \OOOOOOOO"]

-------------------------------------------------------------------------------

pathfindTest :: Int -> String -> Test
pathfindTest limit terrain =
  let strings = lines terrain
      width = length (head strings)
      height = length strings
      arr = listArray ((0, 0), (width - 1, height - 1)) $ concat $
            transpose strings
      get (Point x y) = arr ! (x, y)
      allPoints = range (Point 0 0, Point (width - 1) (height - 1))
      isBlocked = ('O' ==) . get
      isGoal pt = let c = get pt in c == 'e' || c == 'g'
      goals = filter isGoal allPoints
      heuristic pos = minimum $ flip map goals $ \goal ->
                      pDist (fromIntegral <$> pos) (fromIntegral <$> goal)
      start = head $ filter (('s' ==) . get) $ allPoints
      path = pathfind isBlocked isGoal heuristic limit start
      expected =
        let fn (mbCurrent, prev) =
              case mbCurrent of
                Nothing -> Nothing
                Just current ->
                  let mbNext = listToMaybe $ filter (ok prev) $
                               map (current `plusDir`) allDirections
                  in Just (current, (mbNext, current))
            ok prev next =
              next /= prev && (let c = get next in c == '.' || c == 'e')
        in unfoldr fn (Just start, start)
  in insistEq expected path

pathfindFail :: Int -> String -> Test
pathfindFail limit terrain =
  let strings = lines terrain
      width = length (head strings)
      height = length strings
      arr = listArray ((0, 0), (width - 1, height - 1)) $ concat $
            transpose strings
      get (Point x y) = arr ! (x, y)
      allPoints = range (Point 0 0, Point (width - 1) (height - 1))
      isBlocked = ('O' ==) . get
      isGoal pt = let c = get pt in c == 'e' || c == 'g'
      goals = filter isGoal allPoints
      heuristic pos = minimum $ flip map goals $ \goal ->
                      pDist (fromIntegral <$> pos) (fromIntegral <$> goal)
      start = head $ filter (('s' ==) . get) $ allPoints
      path = pathfind isBlocked isGoal heuristic limit start
  in insistEq [] path

-------------------------------------------------------------------------------
