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

module Fallback.View.Quiver
  (Quiver, newQuiver, resetQuiver, quiverKeyDown, quiverKeyUp, quiverDirection)
where

import Control.Applicative ((<$>), (<*>))

import Fallback.Data.Point
import Fallback.Draw (Draw, DrawRef, newDrawRef, readDrawRef, writeDrawRef)
import Fallback.Event

-------------------------------------------------------------------------------

-- | A 'Quiver' is used to keep track of your arrows.
data Quiver = Quiver
  { quiverUpArrow :: DrawRef Bool,
    quiverDownArrow :: DrawRef Bool,
    quiverLeftArrow :: DrawRef Bool,
    quiverRightArrow :: DrawRef Bool }

-- | Create a new 'Quiver' with no arrow keys currently active.
newQuiver :: Draw z Quiver
newQuiver = Quiver <$> newDrawRef False <*> newDrawRef False <*>
            newDrawRef False <*> newDrawRef False

-- | Reset a 'Quiver' to its initial state (no arrow keys active).
resetQuiver :: Quiver -> Draw z ()
resetQuiver q = do
  writeDrawRef (quiverUpArrow q) False
  writeDrawRef (quiverDownArrow q) False
  writeDrawRef (quiverLeftArrow q) False
  writeDrawRef (quiverRightArrow q) False

quiverKeyDown :: Quiver -> Key -> Draw z ()
quiverKeyDown q KeyUpArrow = writeDrawRef (quiverUpArrow q) True
quiverKeyDown q KeyDownArrow = writeDrawRef (quiverDownArrow q) True
quiverKeyDown q KeyLeftArrow = writeDrawRef (quiverLeftArrow q) True
quiverKeyDown q KeyRightArrow = writeDrawRef (quiverRightArrow q) True
quiverKeyDown _ _ = return ()

quiverKeyUp :: Quiver -> Key -> Draw z ()
quiverKeyUp q KeyUpArrow = writeDrawRef (quiverUpArrow q) False
quiverKeyUp q KeyDownArrow = writeDrawRef (quiverDownArrow q) False
quiverKeyUp q KeyLeftArrow = writeDrawRef (quiverLeftArrow q) False
quiverKeyUp q KeyRightArrow = writeDrawRef (quiverRightArrow q) False
quiverKeyUp _ _ = return ()

quiverDirection :: Quiver -> Draw z (Maybe Direction)
quiverDirection q = do
  up <- readDrawRef (quiverUpArrow q)
  dn <- readDrawRef (quiverDownArrow q)
  lf <- readDrawRef (quiverLeftArrow q)
  rt <- readDrawRef (quiverRightArrow q)
  return $ case (up, dn, lf, rt) of
    (False, False, False, False) -> Nothing
    (False, False, False,  True) -> Just DirE
    (False, False,  True, False) -> Just DirW
    (False, False,  True,  True) -> Nothing
    (False,  True, False, False) -> Just DirS
    (False,  True, False,  True) -> Just DirSE
    (False,  True,  True, False) -> Just DirSW
    (False,  True,  True,  True) -> Just DirS
    ( True, False, False, False) -> Just DirN
    ( True, False, False,  True) -> Just DirNE
    ( True, False,  True, False) -> Just DirNW
    ( True, False,  True,  True) -> Just DirN
    ( True,  True, False, False) -> Nothing
    ( True,  True, False,  True) -> Just DirE
    ( True,  True,  True, False) -> Just DirW
    ( True,  True,  True,  True) -> Nothing

-------------------------------------------------------------------------------
