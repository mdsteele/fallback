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

module Fallback.View.Narrate (newNarrateView) where

import Fallback.Constants (screenHeight, screenWidth)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event (Key(KeyReturn))
import Fallback.State.Resources (Resources)
import Fallback.View.Base (View, compoundView, subView_)
import Fallback.View.Dialog (newDialogView)
import Fallback.View.Widget (newSimpleTextButton, newStaticTextWrapView)

-------------------------------------------------------------------------------

newNarrateView :: Resources -> View a b -> a -> String -> Draw z (View () ())
newNarrateView resources bgView bgInput text = do
  let margin = 20
      textW = 512
      width = textW + 2 * margin
  (textH, textView) <- newStaticTextWrapView resources textW text
  button <- newSimpleTextButton resources "OK" [KeyReturn] ()
  let buttonTop = margin + textH + 12
      buttonWidth = 100
      buttonHeight = 20
  let height = buttonTop + buttonHeight + margin
  let rect = Rect (half (screenWidth - width)) (half (screenHeight - height))
                  width height
  let view = compoundView [
               subView_ (Rect margin margin textW textH) textView,
               subView_ (Rect (width - buttonWidth - margin) buttonTop
                              buttonWidth buttonHeight) button]
  newDialogView bgView bgInput view rect

-------------------------------------------------------------------------------
