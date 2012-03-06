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

module Fallback.Test.Error (errorTests) where

import Control.Applicative
import Control.Monad.Fix (mfix)
import Data.Traversable (traverse)
import Test.HUnit ((~:), Test(TestList))

import Fallback.Control.Error
import Fallback.Test.Base (insist)

-------------------------------------------------------------------------------

errorTests :: Test
errorTests = "error" ~: TestList [
  insist $ Right 42 == runEO ok42,
  insist $ Left ["foo"] == runEO failure,
  insist $ Right 42 == runEO (fmap id ok42),
  insist $ Left ["foo"] == runEO (fmap id failure),
  insist $ Right 84 == runEO (fmap (* 2) ok42),
  insist $ Left ["bar", "foo"] ==
           runEO ((fail "bar" :: EO (Int -> Int)) <*> failure),
  insist $ runEO ok42 == runEO (return 42),
  insist $ Right 42 == runEO (ok42 >>= return),
  insist $ Left ["foo"] == runEO (failure >>= return),
  insist $ Left ["foo"] == runEO (failure >> ok42),
  insist $ Right 42 == runEO (return 'x' >> ok42),
  insist $ Right ["a", "b", "c"] == runEO (traverse pure ["a", "b", "c"]),
  insist $ Left ["a", "b", "c"] ==
           runEO (traverse fail ["a", "b", "c"] :: EO [Int]),
  insist $ Right ["a", "b", "c"] == runEO (mapM return ["a", "b", "c"]),
  insist $ Left ["a"] == runEO (mapM fail ["a", "b", "c"] :: EO [Int]),
  insist $ Left ["foo"] == runEO (mfix $ \x -> fmap (: x) failure),
  insist $ Right [42, 42, 42, 42, 42] ==
           runEO (fmap (take 5) $ mfix $ \x -> fmap (: x) ok42)]

-------------------------------------------------------------------------------

ok42 :: EO Int
ok42 = pure 42

failure :: EO Int
failure = fail "foo"

-------------------------------------------------------------------------------
