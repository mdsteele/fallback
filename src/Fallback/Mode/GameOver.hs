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

module Fallback.Mode.GameOver (newGameOverMode) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.IORef

import Fallback.Constants (screenRect)
import Fallback.Data.Clock
import Fallback.Draw (paintScreen, runDraw)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.LoadGame (newLoadGameMode)
import Fallback.State.Resources (Resources)
import Fallback.View (fromAction, viewHandler, viewPaint)
import Fallback.View.GameOver (GameOverAction(..), newGameOverView)

-------------------------------------------------------------------------------

newGameOverMode :: Resources -> Modes -> IO Mode
newGameOverMode resources modes = do
  clockRef <- newIORef initClock
  view <- runDraw (newGameOverView resources)
  let mode EvQuit = return DoQuit
      mode event = do
        when (event == EvTick) $ modifyIORef clockRef clockInc
        clock <- readIORef clockRef
        action <- runDraw $ viewHandler view clock screenRect event
        when (event == EvTick) $ paintScreen (viewPaint view clock)
        case fromAction action of
          Nothing -> return SameMode
          Just LoadASavedGame ->
            ChangeMode <$> newLoadGameMode resources modes mode view clock
          Just ReturnToMainMenu -> ChangeMode <$> newMainMenuMode' modes
          Just QuitGame -> return DoQuit
  focusBlurMode (readIORef clockRef) view mode

-------------------------------------------------------------------------------
