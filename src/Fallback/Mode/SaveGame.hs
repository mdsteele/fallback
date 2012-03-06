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
import Data.List (intercalate)

import Fallback.Constants (screenRect)
import Fallback.Control.Error (runEO, runIOEO)
import Fallback.Draw (Sprite, paintScreen, runDraw)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog
import Fallback.Mode.Narrate (newNarrateMode)
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
    runDraw $ newSaveGameView resources bgView bgInput screenshot
                              (savedGameLocation savedGame) summaries
  let mode EvQuit =
        ChangeMode <$> newQuitWithoutSavingMode resources mode view ()
      mode event = do
        action <- runDraw $ viewHandler view () screenRect event
        when (event == EvTick) $ paintScreen (viewPaint view ())
        case fromAction action of
          Nothing -> return SameMode
          Just CancelSaveGame -> return (ChangeMode prevMode)
          Just (DoSaveGame name) -> do
            eoSummary <- runIOEO $ saveGame name screenshot savedGame
            case runEO eoSummary of
              Left errors -> do
                -- TODO extract this into a newErrorMode or something
                let msg = "Failed to save game:\n" ++ intercalate "\n" errors
                ChangeMode <$>
                  newNarrateMode resources view () msg (return mode)
              Right summary -> onSave summary
  focusBlurMode (return ()) view mode

-------------------------------------------------------------------------------
