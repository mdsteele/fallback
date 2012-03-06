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

module Fallback.Mode.Dialog where

import Control.Applicative ((<$>))
import Control.Monad (when)

import Fallback.Constants (screenRect)
import Fallback.Draw (paintScreen, runDraw)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.State.Resources (Resources)
import Fallback.View
import Fallback.View.Dialog (newHorizontalDialogView, newTextEntryDialogView)

-------------------------------------------------------------------------------

newHorizontalDialogMode :: Resources -> String -> [(String, [Key], c)]
                        -> (c -> IO NextMode) -> View a b -> a -> IO Mode
newHorizontalDialogMode resources text buttonSpecs nextMode bgView bgInput = do
  view <- runDraw $
          newHorizontalDialogView resources text buttonSpecs bgView bgInput
  let mode EvQuit = return SameMode
      mode event = do
        action <- runDraw $ viewHandler view () screenRect event
        when (event == EvTick) $ paintScreen (viewPaint view ())
        maybe (return SameMode) nextMode (fromAction action)
  focusBlurMode (return ()) view mode

newTextEntryDialogMode :: Resources -> String -> String -> (String -> Bool)
                       -> IO Mode -> (String -> IO Mode) -> View a b -> a
                       -> IO Mode
newTextEntryDialogMode resources text initValue testFn
                       onCancel onOk bgView bgInput = do
  view <- runDraw $
          newTextEntryDialogView resources text initValue testFn bgView bgInput
  let mode EvQuit = return SameMode
      mode event = do
        action <- runDraw $ viewHandler view () screenRect event
        when (event == EvTick) $ paintScreen (viewPaint view ())
        case fromAction action of
          Nothing -> return SameMode
          Just Nothing -> ChangeMode <$> onCancel
          Just (Just string) -> ChangeMode <$> onOk string
  focusBlurMode (return ()) view mode

newQuitWithoutSavingMode :: Resources -> Mode -> View a b -> a -> IO Mode
newQuitWithoutSavingMode resources prevMode bgView bgInput =
  newHorizontalDialogMode resources text buttons nextMode bgView bgInput where
    text = "Are you sure you want to quit right now?  You won't be able to\
           \ save your game if you do."
    buttons = [("Quit", [KeyReturn], True), ("Cancel", [KeyEscape], False)]
    nextMode q = if q then return DoQuit else return (ChangeMode prevMode)

-------------------------------------------------------------------------------
