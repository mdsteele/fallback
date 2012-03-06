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

module Fallback.Test.FOV (fovTests) where

import Data.Array
import Data.List (transpose)
import qualified Data.Set as Set
import Test.HUnit ((~:), Test(TestCase, TestList), assertFailure)

import Fallback.Data.Point (Point(Point))
import Fallback.State.FOV (fieldOfView)

-------------------------------------------------------------------------------

fovTests :: Test
fovTests = "fov" ~: TestList [

  fovTest 007 ".......\n\
              \..   ..\n\
              \.     .\n\
              \.  *  .\n\
              \.     .\n\
              \..   ..\n\
              \.......",

  fovTest 999 "*   \n\
              \ o  \n\
              \  . \n\
              \   .",

  fovTest 999 "*oO.\n\
              \o oO\n\
              \Oo O\n\
              \.OOo",

  fovTest 999 "* o.\n\
              \o   \n\
              \.   \n\
              \.   ",

  fovTest 999 "OOo*oOO\n\
              \OOo oOO\n\
              \O.   .O\n\
              \O     O\n\
              \OoooooO",

  fovTest 999 "OOo*oOO\n\
              \OOo oOO\n\
              \O.   .O\n\
              \O     O\n\
              \o     o\n\
              \ooooooo",

  fovTest 999 "o*oOOO\n\
              \o oOOO\n\
              \o oOOO\n\
              \o  ...\n\
              \oooOOO",

  fovTest 999 ".OOOOOOO.\n\
              \OoooooooO\n\
              \Oo     oO\n\
              \Oo *   oO\n\
              \Oo     oO\n\
              \OoooooooO\n\
              \.O.O.O.O.",

  fovTest 999 "OOOOOOOO.OOOOO\n\
              \OOoooooo oo  o\n\
              \oo          oO\n\
              \o* oOOOOOOOOOO\n\
              \ooooOOOOOOOOOO",

  fovTest 110 "OOOOOOOOo\n\
              \OOOOOooo \n\
              \OOOOoo   \n\
              \OOooo    \n\
              \OOo      \n\
              \OOo   *  \n\
              \Oo       ",

  fovTest 110 "OOOOOOOOo\n\
              \OOOOOooo \n\
              \OOOOoo   \n\
              \OOooo    \n\
              \OOo      \n\
              \OOo    * \n\
              \Oo       ",

  fovTest 110 "OOOOOOOoo\n\
              \OOOOOooo \n\
              \OOOOoo   \n\
              \OOooo    \n\
              \OOo      \n\
              \OOo     *\n\
              \Oo       "]

-------------------------------------------------------------------------------


fovTest :: Int -> String -> Test
fovTest radSq expectedString =
  let strings = lines expectedString
      width = length (head strings)
      height = length strings
      arr = listArray ((0, 0), (width - 1, height - 1)) $ concat $
            transpose strings
      isBlocked (Point x y) = let c = arr ! (x, y) in c == 'o' || c == 'O'
      visible = foldr (fieldOfView (width, height) isBlocked radSq) Set.empty $
                map (uncurry Point) $ filter (('*' ==) . (arr !)) $
                range $ bounds arr
      shouldBeVisible p = let c = arr ! p in c /= '.' && c /= 'O'
      expected = Set.fromList $ map (uncurry Point) $ filter shouldBeVisible $
                 range $ bounds arr
  in TestCase $
     if visible == expected then return () else
       let trans ((x, y), c) =
             if c == 'o' || c == 'O'
             then if Set.member p visible then 'o' else 'O'
             else if c == ' ' || c == '.'
                  then if Set.member p visible then ' ' else '.'
                  else if Set.member p visible then c else '#'
             where p = Point x y
           arr' = listArray (bounds arr) $ map trans $ assocs arr
           rowString y = map (\x -> arr' ! (x, y)) [0 .. width - 1]
           actualString = unlines $ map rowString [0 .. height - 1]
       in assertFailure ("Actual:\n" ++ actualString ++
                         "Expected:\n" ++ expectedString)

-------------------------------------------------------------------------------
