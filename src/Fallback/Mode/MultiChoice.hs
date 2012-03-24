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

module Fallback.Mode.MultiChoice (newMultiChoiceMode) where

import Control.Applicative ((<$>))
import Control.Monad (when)

import Fallback.Constants (screenRect)
import Fallback.Draw (paintScreen, runDraw)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog (newQuitWithoutSavingMode)
import Fallback.State.Resources (Resources)
import Fallback.View (View, fromAction, viewHandler, viewPaint)
import Fallback.View.MultiChoice (newMultiChoiceView)

-------------------------------------------------------------------------------

newMultiChoiceMode :: Resources -> View a b -> a -> String -> [(String, c)]
                   -> Maybe c -> (c -> IO Mode) -> IO Mode
newMultiChoiceMode resources bgView bgInput text choices cancel nextMode = do
  view <- runDraw $ do
    newMultiChoiceView resources bgView bgInput text choices cancel
  let mode EvQuit = do
        ChangeMode <$> newQuitWithoutSavingMode resources mode view ()
      mode event = do
        action <- runDraw $ viewHandler view () screenRect event
        when (event == EvTick) $ paintScreen (viewPaint view ())
        case fromAction action of
          Nothing -> return SameMode
          Just value -> ChangeMode <$> nextMode value
  focusBlurMode (return ()) view mode

-------------------------------------------------------------------------------
