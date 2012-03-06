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

module Fallback.Test.Script (scriptTests) where

import Control.Applicative ((<$>), (<*>), (*>))
import Test.HUnit ((~:), Test(TestList))

import Fallback.Control.Script
import Fallback.Test.Base (insist)

-------------------------------------------------------------------------------

scriptTests :: Test
scriptTests = "script" ~: TestList [
  testI [] $ return (),
  testI [3] $ emit_ 3,
  testI [1,2] $ do { x <- emit 1 2; emit_ x },
  testI [5,6,5,6] $ let x = emit_ 5 >> emit_ 6 in x *> x,
  testI [9,3] $ (length <$> emit 9 "foo") >>= emit_,
  testI [2,8,1,5] $ (emit 2 (-) <*> emit 8 9 <*> emit 1 4) >>= emit_,
  testS ["2","7"] $ mapEffect change $ emit_ 2 >> emit_ 7]

-------------------------------------------------------------------------------

data TestEffect e a = TestEffect e a

emit :: e -> a -> Script (TestEffect e) a
emit msg value = emitEffect (TestEffect msg value)

emit_ :: e -> Script (TestEffect e) ()
emit_ = flip emit ()

run :: Script (TestEffect e) () -> [e]
run = helper id where
  helper fn script =
    case execScript script of
      ResultFinal () -> fn []
      ResultEffect (TestEffect msg value) sfn ->
        helper (fn . (msg :)) (sfn value)

testI :: [Int] -> Script (TestEffect Int) () -> Test
testI expected script = insist (expected == run script)

testS :: [String] -> Script (TestEffect String) () -> Test
testS expected script = insist (expected == run script)

-------------------------------------------------------------------------------

change :: TestEffect Int a -> TestEffect String a
change (TestEffect n a) = TestEffect (show n) a

-------------------------------------------------------------------------------
