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

module Fallback.View.MainMenu
  (MainMenuAction(..), newMainMenuView)
where

import Control.Applicative ((<$>))

import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.State.Resources (Resources)
import Fallback.View.Base
import Fallback.View.Widget (newSimpleTextButton)

-------------------------------------------------------------------------------

data MainMenuAction = NewGame | LoadGame | QuitGame | EditTerrain

-------------------------------------------------------------------------------

newMainMenuView :: (MonadDraw m) => Resources -> m (View () MainMenuAction)
newMainMenuView resources = do
  compoundViewM [
    newBackgroundView,
    (subView_ (Rect 40 400 100 24) <$>
     newSimpleTextButton resources "Editor" [KeyE] EditTerrain),
    (subView_ (Rect 500 300 100 24) <$>
     newSimpleTextButton resources "New Game" [KeyN] NewGame),
    (subView_ (Rect 500 350 100 24) <$>
     newSimpleTextButton resources "Continue" [KeyC, KeyO] LoadGame),
    (subView_ (Rect 500 400 100 24) <$>
     newSimpleTextButton resources "Quit" [KeyQ] QuitGame)]

-------------------------------------------------------------------------------

newBackgroundView :: (MonadDraw m) => m (View a b)
newBackgroundView = do
--   bgSprite <- loadSprite "tileset_indoors.png"
--   return $ inertView $ const $ blitStretch bgSprite (Rect 0 0 (16 * 28) (16 * 36) :: IRect)
  bgSprite <- loadSprite "gui/main-menu-background.png"
  return $ inertView $ const $ canvasRect >>= blitStretch bgSprite

-------------------------------------------------------------------------------
