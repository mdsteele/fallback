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

module Fallback.Data.Clock
  (Clock, initClock, clockInc, clockMod, clockZigzag)
where

-------------------------------------------------------------------------------

-- In theory, the memory usage of a Clock object grows unbounded as you
-- increment it.  In practice, you could probably run the game until the heat
-- death of the universe and never even notice the memory usage.  So it's fine.

-- | A 'Clock' represents a counter that is incremented with each animation
-- frame.  The `clockMod` function can be used to find the value of this
-- counter modulo some number, which can be used to drive cyclic animations.
newtype Clock = Clock Integer

-- | A clock in its initial state.
initClock :: Clock
initClock = Clock 0

-- | Increment the clock by one frame.
clockInc :: Clock -> Clock
clockInc (Clock n) = Clock (n + 1)

-- | Return a number that cycles from zero up to @modulus - 1@, with the number
-- advancing by one every @slowdown@ frames.
clockMod :: Int {-^modulus-} -> Int {-^slowdown-} -> Clock -> Int
clockMod modulus slowdown (Clock n) =
  fromInteger (n `mod` toInteger (modulus * slowdown)) `div` slowdown

-- | Return a number that cycles from zero up to @modulus - 1@ and back down,
-- with the number advancing by one every @slowdown@ frames.
clockZigzag :: Int -> Int -> Clock -> Int
clockZigzag modulus slowdown clock =
  (clockMod ((modulus - 1) * 2) slowdown clock) `zigzag` modulus

-------------------------------------------------------------------------------
-- Private:

zigzag :: Int -> Int -> Int
zigzag d m = let (q, r) = d `divMod` (m - 1)
             in if q `mod` 2 == 0 then r else (m - 1) - r

-------------------------------------------------------------------------------
