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
import Data.List (find, transpose)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Test.HUnit ((~:), Test(TestCase, TestList), assertFailure)

import Fallback.Data.Point (Point(Point), Position, SqDist(SqDist))
import Fallback.State.FOV (fieldOfView, lineOfSight)

-------------------------------------------------------------------------------

fovTests :: Test
fovTests = "fov" ~: TestList [

  fovTest (SqDist 7)
    ".......\n\
    \..   ..\n\
    \.     .\n\
    \.  *  .\n\
    \.     .\n\
    \..   ..\n\
    \.......",

  fovTest (SqDist 999)
    "*   \n\
    \ o  \n\
    \  . \n\
    \   .",

  fovTest (SqDist 999)
    "*oO.\n\
    \o oO\n\
    \Oo O\n\
    \.OOo",

  losTest
    "*oO.\n\
    \o oO\n\
    \Oo O\n\
    \.OOo",

  fovTest (SqDist 999)
    "* o.\n\
    \o   \n\
    \.   \n\
    \.   ",

  fovTest (SqDist 999)
    "OOo*oOO\n\
    \OOo oOO\n\
    \O.   .O\n\
    \O     O\n\
    \OoooooO",

  fovTest (SqDist 999)
    "OOo*oOO\n\
    \OOo oOO\n\
    \O.   .O\n\
    \O     O\n\
    \o     o\n\
    \ooooooo",

  fovTest (SqDist 999)
    "o*oOOO\n\
    \o oOOO\n\
    \o oOOO\n\
    \o  ...\n\
    \oooOOO",

  fovTest (SqDist 999)
    ".OOOOOOO.\n\
    \OoooooooO\n\
    \Oo     oO\n\
    \Oo *   oO\n\
    \Oo     oO\n\
    \OoooooooO\n\
    \.O.O.O.O.",

  fovTest (SqDist 999)
    "OOOOOOOO.OOOOO\n\
    \OOoooooo oo  o\n\
    \oo          oO\n\
    \o* oOOOOOOOOOO\n\
    \ooooOOOOOOOOOO",

  losTest
    "OOOOOOOO.OOOOO\n\
    \OOoooooo oo  o\n\
    \oo          oO\n\
    \o* oOOOOOOOOOO\n\
    \ooooOOOOOOOOOO",

  fovTest (SqDist 110)
    "OOOOOOOOo\n\
    \OOOOOooo \n\
    \OOOOoo   \n\
    \OOooo    \n\
    \OOo      \n\
    \OOo   *  \n\
    \Oo       ",

  fovTest (SqDist 110)
    "OOOOOOOOo\n\
    \OOOOOooo \n\
    \OOOOoo   \n\
    \OOooo    \n\
    \OOo      \n\
    \OOo    * \n\
    \Oo       ",

  fovTest (SqDist 110)
    "OOOOOOOoo\n\
    \OOOOOooo \n\
    \OOOOoo   \n\
    \OOooo    \n\
    \OOo      \n\
    \OOo     *\n\
    \Oo       "]

-------------------------------------------------------------------------------


fovTest :: SqDist -> String -> Test
fovTest radSq expectedString =
  let (width, height, arr, isBlocked, expected) = buildArray expectedString
      visible = foldr (fieldOfView (width, height) isBlocked radSq) Set.empty $
                map (uncurry Point) $ filter (('*' ==) . (arr !)) $
                range $ bounds arr
  in TestCase $
     if visible == expected then return () else
       let actualString = computeActualString (width, height) arr visible
       in assertFailure ("Actual:\n" ++ actualString ++
                         "Expected:\n" ++ expectedString)

losTest :: String -> Test
losTest expectedString =
  let (width, height, arr, isBlocked, expected) = buildArray expectedString
      eye = uncurry Point $ fromJust $ find (('*' ==) . (arr !)) $ range $
            bounds arr
      visible = Set.fromList $ filter (lineOfSight isBlocked eye) $
                map (uncurry Point) $ range $ bounds arr
  in TestCase $
     if visible == expected then return () else
       let actualString = computeActualString (width, height) arr visible
       in assertFailure ("Actual:\n" ++ actualString ++
                         "Expected:\n" ++ expectedString)

buildArray :: String -> (Int, Int, Array (Int, Int) Char, Position -> Bool,
                         Set.Set Position)
buildArray string =
  let strings = lines string
      width = length (head strings)
      height = length strings
      arr = listArray ((0, 0), (width - 1, height - 1)) $ concat $
            transpose strings
      isBlocked (Point x y) = let c = arr ! (x, y) in c == 'o' || c == 'O'
      shouldBeVisible p = let c = arr ! p in c /= '.' && c /= 'O'
      expected = Set.fromList $ map (uncurry Point) $ filter shouldBeVisible $
                 range $ bounds arr
  in (width, height, arr, isBlocked, expected)

computeActualString :: (Int, Int) -> Array (Int, Int) Char -> Set.Set Position
                    -> String
computeActualString (width, height) arr visible =
  let trans ((x, y), c) =
        if c == 'o' || c == 'O' then if canSee then 'o' else 'O'
        else if c == ' ' || c == '.' then if canSee then ' ' else '.'
             else if canSee then c else '#'
        where canSee = Set.member (Point x y) visible
      arr' = listArray (bounds arr) $ map trans $ assocs arr
      rowString y = map (\x -> arr' ! (x, y)) [0 .. width - 1]
  in unlines $ map rowString [0 .. height - 1]

-------------------------------------------------------------------------------
