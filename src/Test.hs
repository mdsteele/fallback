{- ============================================================================
| Copyright 2010 Matthew D. Steele <mdsteele@alum.mit.edu>                    |
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

module Test (main) where

import Test.HUnit (Test(TestList), runTestTT)

import Fallback.Test.Error (errorTests)
import Fallback.Test.FOV (fovTests)
import Fallback.Test.Grid (gridTests)
import Fallback.Test.Pathfind (pathfindTests)
import Fallback.Test.Point (pointTests)
import Fallback.Test.PriorityQueue (pqTests)
import Fallback.Test.Script (scriptTests)
import Fallback.Test.Utility (utilityTests)

-------------------------------------------------------------------------------

main :: IO ()
main = runTestTT allTests >> return ()

allTests :: Test
allTests = TestList [errorTests, fovTests, gridTests, pathfindTests,
                     pointTests, pqTests, scriptTests, utilityTests]

-------------------------------------------------------------------------------
