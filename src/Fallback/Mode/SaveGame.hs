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

module Fallback.Mode.SaveGame
  (newSaveGameMode, newSaveBeforeQuittingMode, newSaveBeforeLeavingMode)
where

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
  return mode

-------------------------------------------------------------------------------

data SaveBeforeQuittingResponse = CancelQuit | QuitWithoutSave | SaveAndQuit

newSaveBeforeQuittingMode :: Resources -> Sprite -> SavedGame -> Mode
                          -> View a b -> a -> IO Mode
newSaveBeforeQuittingMode resources screenshot savedGame
                          prevMode bgView bgInput =
  newHorizontalDialogMode resources text buttons nextMode bgView bgInput where
    text = "Would you like to save your game before quitting?"
    buttons = [("Save", [KeyReturn], SaveAndQuit),
               ("Don't Save", [KeyD], QuitWithoutSave),
               ("Cancel", [KeyEscape], CancelQuit)]
    nextMode CancelQuit = return (ChangeMode prevMode)
    nextMode QuitWithoutSave = return DoQuit
    nextMode SaveAndQuit =
      ChangeMode <$> newSaveGameMode resources onSave screenshot savedGame
                                     prevMode bgView bgInput
    onSave _ = return DoQuit

newSaveBeforeLeavingMode :: Resources -> Modes -> Sprite -> SavedGame -> Mode
                         -> View a b -> a -> IO Mode
newSaveBeforeLeavingMode resources modes screenshot savedGame
                         prevMode bgView bgInput =
  newHorizontalDialogMode resources text buttons nextMode bgView bgInput where
    text = "Would you like to save your game before returning to the title\
           \ screen?"
    buttons = [("Save", [KeyReturn], SaveAndQuit),
               ("Don't Save", [KeyD], QuitWithoutSave),
               ("Cancel", [KeyEscape], CancelQuit)]
    nextMode CancelQuit = return (ChangeMode prevMode)
    nextMode QuitWithoutSave = leave
    nextMode SaveAndQuit =
      ChangeMode <$> newSaveGameMode resources (const leave) screenshot
                                     savedGame prevMode bgView bgInput
    leave = ChangeMode <$> newMainMenuMode' modes

-------------------------------------------------------------------------------
