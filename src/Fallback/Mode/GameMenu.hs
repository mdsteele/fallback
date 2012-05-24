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

module Fallback.Mode.GameMenu
  (GameMenuState(..), newGameMenuMode)
where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.IORef (modifyIORef, newIORef, readIORef)

import Fallback.Draw (Sprite, handleScreen, paintScreen)
import Fallback.Event
import Fallback.Preferences (getPreferences, setPreferences)
import Fallback.Mode.Base
import Fallback.Mode.Dialog (newHorizontalDialogMode, newQuitWithoutSavingMode)
import Fallback.Mode.LoadGame (newLoadGameMode)
import Fallback.Mode.SaveGame
  (newSaveBeforeLeavingMode, newSaveBeforeQuittingMode, newSaveGameMode)
import Fallback.State.Party (partyDifficulty)
import Fallback.State.Resources (Resources)
import Fallback.State.Simple (Difficulty)
import Fallback.View (View, fromAction, viewHandler, viewPaint)
import Fallback.View.GameMenu

-------------------------------------------------------------------------------


newGameMenuMode :: Resources -> Modes -> Sprite -> GameMenuState a
                -> (a -> IO Mode) -> View a b -> IO Mode
newGameMenuMode resources modes screenshot initState onDone bgView = do
  view <- newGameMenuView resources bgView
  stateRef <- newIORef initState
  let

    mode EvQuit = askBeforeQuitting =<< readIORef stateRef
    mode event = do
      gms <- readIORef stateRef
      action <- handleScreen $ viewHandler view gms event
      when (event == EvTick) $ paintScreen (viewPaint view gms)
      case fromAction action of
        Just GameMenuChangeDifficulty -> do
          let onDiff diff = do
                modifyIORef stateRef $ gmsModifyParty $ \party ->
                  party { partyDifficulty = diff }
                return (ChangeMode mode)
          ChangeMode <$> newChangeDifficultyMode resources screenshot onDiff
                                                 view gms
        Just GameMenuEditParty -> return SameMode -- FIXME implement this
        Just GameMenuLoad -> do
          ChangeMode <$> newLoadGameMode resources modes mode view gms
        Just GameMenuPreferences -> do
          ChangeMode <$> newPreferencesMode resources screenshot mode view gms
        Just GameMenuQuit -> askBeforeQuitting gms
        Just GameMenuReturnToGame -> ChangeMode <$> onDone (gmsSubState gms)
        Just GameMenuReturnToMainMenu -> askBeforeLeaving gms
        Just GameMenuSave -> do
          case gmsSavedGame gms of
            Nothing -> return SameMode
            Just savedGame -> do
              let onSave _ = return (ChangeMode mode) -- TODO display msg?
              ChangeMode <$> newSaveGameMode resources onSave screenshot
                                             savedGame mode view gms
        Nothing -> return SameMode

    askBeforeLeaving gms = do
      case gmsSavedGame gms of
        Nothing -> ChangeMode <$>
          newLeaveWithoutSavingMode resources modes mode view gms
        Just savedGame -> ChangeMode <$>
          newSaveBeforeLeavingMode resources modes screenshot savedGame mode
                                   view gms
    askBeforeQuitting gms = do
      if not (gmsUnsaved gms) then return DoQuit else do
      case gmsSavedGame gms of
        Nothing -> ChangeMode <$>
          newQuitWithoutSavingMode resources mode view gms
        Just savedGame -> ChangeMode <$>
          newSaveBeforeQuittingMode resources screenshot savedGame mode
                                    view gms

  return mode

-------------------------------------------------------------------------------

newPreferencesMode :: Resources -> Sprite -> Mode
                   -> View (GameMenuState a) b -> (GameMenuState a)
                   -> IO Mode
newPreferencesMode resources screenshot prevMode bgView gms = do
  initPrefs <- getPreferences
  view <- newPreferencesView resources initPrefs bgView gms
  let
    mode EvQuit = ChangeMode <$>
      case gmsSavedGame gms of
        Nothing -> newQuitWithoutSavingMode resources mode view ()
        Just savedGame -> newSaveBeforeQuittingMode resources screenshot
                                                    savedGame mode view ()
    mode event = do
      action <- handleScreen $ viewHandler view () event
      when (event == EvTick) $ paintScreen (viewPaint view ())
      case fromAction action of
        Just (Just prefs') -> do
          setPreferences prefs'
          return (ChangeMode prevMode)
        Just Nothing -> return (ChangeMode prevMode)
        Nothing -> return SameMode
  return mode

newChangeDifficultyMode :: Resources -> Sprite -> (Difficulty -> IO NextMode)
                        -> View (GameMenuState a) b -> (GameMenuState a)
                        -> IO Mode
newChangeDifficultyMode resources screenshot onDone bgView gms = do
  view <- newChangeDifficultyView resources bgView gms
  let
    mode EvQuit = ChangeMode <$>
      case gmsSavedGame gms of
        Nothing -> newQuitWithoutSavingMode resources mode view ()
        Just savedGame -> newSaveBeforeQuittingMode resources screenshot
                                                    savedGame mode view ()
    mode event = do
      action <- handleScreen $ viewHandler view () event
      when (event == EvTick) $ paintScreen (viewPaint view ())
      maybe (return SameMode) onDone (fromAction action)
  return mode

newLeaveWithoutSavingMode :: Resources -> Modes -> Mode -> View a b -> a
                          -> IO Mode
newLeaveWithoutSavingMode resources modes prevMode bgView bgInput =
  newHorizontalDialogMode resources text buttons nextMode bgView bgInput where
    text = "Are you sure you want to leave this game and return to the title\
           \ screen right now?  You won't be able to save your game if you do."
    buttons = [("Leave", [KeyReturn], True), ("Cancel", [KeyEscape], False)]
    nextMode leave = if leave then ChangeMode <$> newMainMenuMode' modes
                     else return (ChangeMode prevMode)

-------------------------------------------------------------------------------
