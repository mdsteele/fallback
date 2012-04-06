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

module Fallback.Test.Queue (queueTests) where

import Data.List (unfoldr)
import Test.HUnit ((~:), Test(TestList))

import qualified Fallback.Data.Queue as Queue
import Fallback.Test.Base (insist, qcTest)

-------------------------------------------------------------------------------

queueTests :: Test
queueTests = "queue" ~: TestList [
  insist $ Queue.null Queue.empty,
  insist $ Queue.size Queue.empty == 0,
  qcTest $ \v -> not (Queue.null (Queue.singleton v :: QI)),
  qcTest $ \v -> Queue.size (Queue.singleton v :: QI) == 1,
  qcTest $ \v -> Queue.pop (Queue.singleton v :: QI) == Just (v, Queue.empty),
  qcTest $ \v -> Queue.insert v Queue.empty == (Queue.singleton v :: QI),
  qcTest $ \v1 v2 -> Queue.size (Queue.insert v2 $ Queue.insert v1 $
                                 Queue.empty :: QI) == 2,
  qcTest $ \list -> Queue.size (Queue.fromList list :: QI) == length list,
  qcTest $ \list -> Queue.toList (Queue.fromList list :: QI) == list,
  qcTest $ \list -> (Queue.fromList list :: QI) ==
                    foldl (flip Queue.insert) Queue.empty list,
  qcTest $ \list -> let queue = Queue.fromList list :: QI
                    in Queue.toList queue == unfoldr Queue.pop queue]

type QI = Queue.Queue Int

-------------------------------------------------------------------------------
