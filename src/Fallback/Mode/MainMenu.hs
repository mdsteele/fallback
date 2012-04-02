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

module Fallback.Mode.MainMenu (newMainMenuMode) where

import Control.Applicative ((<$>))
import Control.Monad (when)

import Fallback.Draw (handleScreen, paintScreen)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Editor (newEditorMode)
import Fallback.Mode.LoadGame (newLoadGameMode)
import Fallback.Mode.NewGame (newNewGameMode)
import Fallback.State.Resources (Resources)
import Fallback.View (fromAction, viewHandler, viewPaint)
import Fallback.View.MainMenu (MainMenuAction(..), newMainMenuView)

-------------------------------------------------------------------------------

newMainMenuMode :: Resources -> Modes -> IO Mode
newMainMenuMode resources modes = do
  view <- newMainMenuView resources
  let mode EvQuit = return DoQuit
      mode event = do
        action <- handleScreen $ viewHandler view () event
        when (event == EvTick) $ paintScreen (viewPaint view ())
        case fromAction action of
          Nothing -> return SameMode
          Just NewGame ->
            ChangeMode <$> newNewGameMode resources modes mode view ()
          Just LoadGame ->
            ChangeMode <$> newLoadGameMode resources modes mode view ()
          Just QuitGame -> return DoQuit
          Just EditTerrain -> ChangeMode <$> newEditorMode resources
  return mode

-------------------------------------------------------------------------------
