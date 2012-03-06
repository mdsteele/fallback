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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fallback.Test.Base
  (insist, insistEq, insistAll, qcTest)
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import qualified Data.Set as Set
import qualified Test.HUnit as HU (Test(TestCase), assert, assertFailure)
import qualified Test.QuickCheck as QC

import Fallback.Data.Point (Direction, Point(Point), Rect(Rect), allDirections)

-------------------------------------------------------------------------------

insist :: Bool -> HU.Test
insist = HU.TestCase . HU.assert

insistEq :: (Eq a, Show a) => a -> a -> HU.Test
insistEq expected actual = HU.TestCase $ do
  unless (expected == actual) $ do
    HU.assertFailure ("Expected: " ++ show expected ++
                      "\n  Actual: " ++ show actual)

insistAll :: (Show a) => [a] -> (a -> Bool) -> HU.Test
insistAll items testFn = HU.TestCase $ mapM_ action items where
  action item = unless (testFn item) $
                HU.assertFailure ("Failed for: " ++ show item)

qcTest :: (QC.Testable a) => a -> HU.Test
qcTest test =
  HU.TestCase $ do
    result <- QC.quickCheckWithResult (QC.stdArgs { QC.chatty = False }) test
    case result of
      QC.Success {} -> return ()
      _ -> HU.assertFailure (QC.output result)

-------------------------------------------------------------------------------

instance (QC.Arbitrary a, Ord a) => QC.Arbitrary (Set.Set a) where
  arbitrary = fmap Set.fromList QC.arbitrary

instance QC.Arbitrary Direction where
  arbitrary = QC.elements allDirections

instance (QC.Arbitrary a) => QC.Arbitrary (Point a) where
  arbitrary = Point <$> QC.arbitrary <*> QC.arbitrary

instance (QC.Arbitrary a, Real a) => QC.Arbitrary (Rect a) where
  arbitrary = Rect <$> QC.arbitrary <*> QC.arbitrary
                   <*> (abs <$> QC.arbitrary) <*> (abs <$> QC.arbitrary)

-------------------------------------------------------------------------------
