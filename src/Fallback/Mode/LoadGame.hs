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

module Fallback.Mode.LoadGame (newLoadGameMode) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.List (intercalate)

import Fallback.Constants (screenRect)
import Fallback.Control.Error (runEO, runIOEO)
import Fallback.Draw (paintScreen, runDraw)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog
import Fallback.Mode.Narrate (newNarrateMode)
import Fallback.Scenario.Save
import Fallback.State.Resources (Resources)
import Fallback.View (View, fromAction, viewHandler, viewPaint)
import Fallback.View.LoadGame

-------------------------------------------------------------------------------

newLoadGameMode :: Resources -> Modes -> Mode -> View a b -> a -> IO Mode
newLoadGameMode resources modes prevMode bgView bgInput = do
  view <- runDraw . newLoadGameView resources bgView bgInput =<<
          loadSavedGameSummaries
  let
    mode EvQuit =
      ChangeMode <$> newQuitWithoutSavingMode resources mode view ()
    mode event = do
      action <- runDraw $ viewHandler view () screenRect event
      when (event == EvTick) $ paintScreen (viewPaint view ())
      case fromAction action of
        Nothing -> return SameMode
        Just CancelLoadGame -> return (ChangeMode prevMode)
        Just (DoLoadGame sgs) -> do
          eoSavedGame <- runIOEO $ loadSavedGame resources sgs
          case runEO eoSavedGame of
            Right (SavedRegionState rs) ->
              ChangeMode <$> newRegionMode' modes rs
            Right (SavedTownState ts) ->
              ChangeMode <$> newTownMode' modes ts
            Left errors -> do
              let msg = intercalate "\n" errors
              ChangeMode <$> newNarrateMode resources view () msg (return mode)
  focusBlurMode (return ()) view mode

-------------------------------------------------------------------------------
