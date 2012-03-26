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

module Fallback.Mode.SaveGame (newSaveGameMode) where

import Control.Applicative ((<$>))
import Control.Monad (when)

import Fallback.Draw (Sprite, handleScreen, paintScreen)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog
import Fallback.Mode.Error (popupIfErrors)
import Fallback.Scenario.Save
import Fallback.State.Resources (Resources)
import Fallback.View (View, fromAction, viewHandler, viewPaint)
import Fallback.View.SaveGame

-------------------------------------------------------------------------------

newSaveGameMode :: Resources -> (SavedGameSummary -> IO NextMode) -> Sprite
                -> SavedGame -> Mode -> View a b -> a -> IO Mode
newSaveGameMode resources onSave screenshot savedGame
                prevMode bgView bgInput = do
  view <- do
    summaries <- loadSavedGameSummaries
    newSaveGameView resources bgView bgInput screenshot
                    (savedGameLocation savedGame) summaries
  let mode EvQuit =
        ChangeMode <$> newQuitWithoutSavingMode resources mode view ()
      mode event = do
        action <- handleScreen $ viewHandler view () event
        when (event == EvTick) $ paintScreen (viewPaint view ())
        case fromAction action of
          Nothing -> return SameMode
          Just CancelSaveGame -> return (ChangeMode prevMode)
          Just (DoSaveGame name) -> do
            popupIfErrors resources view () (return mode)
                          (saveGame name screenshot savedGame) $ \summary -> do
              onSave summary
  focusBlurMode (return ()) view mode

-------------------------------------------------------------------------------
