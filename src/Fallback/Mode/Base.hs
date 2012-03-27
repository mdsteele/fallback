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

module Fallback.Mode.Base
  (Mode, NextMode(..), Modes(..), focusBlurMode)
where

import Data.IORef

import Fallback.Draw (handleScreen)
import Fallback.Event (Event(EvBlur))
import Fallback.State.Combat (CombatState)
import Fallback.State.Region (RegionState)
import Fallback.State.Town (TownState)
import Fallback.View (View, viewHandler)

-------------------------------------------------------------------------------

type Mode = Event -> IO NextMode

data NextMode = DoQuit | SameMode | ChangeMode Mode

-------------------------------------------------------------------------------

data Modes = Modes
  { newCombatMode' :: CombatState -> IO Mode,
    newGameOverMode' :: IO Mode,
    newMainMenuMode' :: IO Mode,
    newRegionMode' :: RegionState -> IO Mode,
    newTownMode' :: TownState -> IO Mode }

-------------------------------------------------------------------------------

focusBlurMode :: IO a -> View a b -> Mode -> IO Mode
focusBlurMode getInput view mode = do
  activeRef <- newIORef False
  return $ \event -> do
    writeIORef activeRef True
    result <- mode event
    case result of
      ChangeMode _ -> do
        input <- getInput
        _ <- handleScreen $ viewHandler view input EvBlur
        writeIORef activeRef False
      _ -> return ()
    return result

-------------------------------------------------------------------------------
