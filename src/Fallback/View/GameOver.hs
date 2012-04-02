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

module Fallback.View.GameOver
  (GameOverAction(..), newGameOverView)
where

import Control.Applicative ((<$>))

import Fallback.Data.Clock (Clock, clockZigzag)
import Fallback.Data.Color (Color(Color), blackTint)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event (Key(..))
import Fallback.State.Resources (FontTag(FontChancery72), Resources, rsrcFont)
import Fallback.View.Base
import Fallback.View.Widget (newSimpleTextButton)

-------------------------------------------------------------------------------

data GameOverAction = LoadASavedGame | ReturnToMainMenu | QuitGame

-------------------------------------------------------------------------------

newGameOverView :: (MonadDraw m) => Resources -> m (View Clock GameOverAction)
newGameOverView resources = do
  compoundViewM [
    newBackgroundView resources,
    (subView_ (Rect 500 300 100 24) <$>
     newSimpleTextButton resources "Continue" [KeyC, KeyO] LoadASavedGame),
    (subView_ (Rect 500 350 100 24) <$>
     newSimpleTextButton resources "Return to Main Menu" [KeyM]
                         ReturnToMainMenu),
    (subView_ (Rect 500 400 100 24) <$>
     newSimpleTextButton resources "Quit" [KeyQ] QuitGame)]

-------------------------------------------------------------------------------

newBackgroundView :: (MonadDraw m) => Resources -> m (View Clock b)
newBackgroundView resources = do
  let font = rsrcFont resources FontChancery72
  let paint clock = do
        tintCanvas blackTint
        width <- canvasWidth
        let g = fromIntegral (4 * clockZigzag 61 1 clock)
        drawText font (Color g g g) (LocCenter $ Point (half width) 180)
                 "Game Over"
  return (inertView paint)

-------------------------------------------------------------------------------
