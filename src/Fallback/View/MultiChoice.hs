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

module Fallback.View.MultiChoice (newMultiChoiceView) where

import Data.Traversable (for)

import Fallback.Constants (screenHeight, screenWidth)
import Fallback.Data.Color (Color(Color), blackColor)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event (Key(KeyEscape), letterKeys)
import Fallback.State.Resources (FontTag(FontChancery14), Resources, rsrcFont)
import Fallback.View.Base (View, compoundView, nullView, subView_)
import Fallback.View.Dialog (newDialogView)
import Fallback.View.Widget

-------------------------------------------------------------------------------

newMultiChoiceView :: Resources -> View a b -> a -> String -> [(String, c)]
                   -> Maybe c -> Draw z (View () c)
newMultiChoiceView resources bgView bgInput text choices cancelValue = do
  let margin = 20
      textW = 512
      width = textW + 2 * margin
      choiceIndent = 20
      choiceW = textW - choiceIndent
      choiceSpacing = 6
  (textH, textView) <- newStaticTextWrapView resources textW text
  topRef <- newDrawRef (margin + textH + 24)
  choiceButtons <- for (zip3 choices ['A'..'Z'] letterKeys) $
                   \((label, value), letter, key) -> do
    (choiceH, choicePaint) <-
      newStaticTextWrapColorPaint resources choiceW label
    let buttonColor ButtonUp = blackColor
        buttonColor ButtonHover = Color 0 64 0
        buttonColor ButtonDown = Color 128 0 0
        buttonColor ButtonDisabled = blackColor
    let buttonPaint _ state = do
          let color = buttonColor state
          drawText (rsrcFont resources FontChancery14) color
                   (LocTopleft $ Point 0 (0 :: Int)) (letter : "")
          withSubCanvas (Rect choiceIndent 0 choiceW choiceH) $ do
            choicePaint color
    choiceButton <- newButton buttonPaint (const ReadyButton) [key] value
    top <- readDrawRef topRef
    writeDrawRef topRef (top + choiceH + choiceSpacing)
    return $ subView_ (Rect margin top textW choiceH) choiceButton
  cancelButton <- maybe (return nullView)
    (newSimpleTextButton resources "Done" [KeyEscape]) cancelValue
  buttonTop <- readDrawRef topRef
  let (buttonWidth, buttonHeight) = (100, 20)
  let height = buttonTop + buttonHeight + margin
  let rect = Rect (half (screenWidth - width)) (half (screenHeight - height))
                  width height
  let view = compoundView [
               subView_ (Rect margin margin textW textH) textView,
               compoundView choiceButtons,
               subView_ (Rect (width - buttonWidth - margin) buttonTop
                              buttonWidth buttonHeight) cancelButton]
  newDialogView bgView bgInput view rect

-------------------------------------------------------------------------------
