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

module Fallback.View.Dialog
  (newDialogView, newDialogView', newHorizontalDialogView,
   newTextEntryDialogView, newDialogBackgroundView)
where

import Control.Applicative ((<$), (<$>))
import Control.Monad (zipWithM)

import Fallback.Constants (screenHeight, screenWidth)
import Fallback.Data.Color (Tint(Tint))
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.State.Resources (Resources)
import Fallback.View.Base
import Fallback.View.Widget
  (newSimpleTextButton, newStaticTextWrapView, newTextBox)

-------------------------------------------------------------------------------

newDialogView :: (MonadDraw m) => View c d -> c -> View a b -> IRect
              -> m (View a b)
newDialogView bgView bgInput = newDialogView' bgView (const bgInput)

newDialogView' :: (MonadDraw m) => View c d -> (a -> c) -> View a b -> IRect
               -> m (View a b)
newDialogView' bgView bgInput fgView subRect = do
  let bgPaint input = do
        withInputsSuppressed $ viewPaint bgView (bgInput input)
        tintCanvas (Tint 0 0 0 64)
      bgHandler input EvTick = Ignore <$
        withInputsSuppressed (viewHandler bgView (bgInput input) EvTick)
      bgHandler _ _ = return Ignore
  midView <- newDialogBackgroundView
  return $ compoundView $ [
    (View bgPaint bgHandler),
    (subView_ subRect $ compoundView [midView, fgView])]

-------------------------------------------------------------------------------

newHorizontalDialogView :: (MonadDraw m) => Resources -> String
                        -> [(String, [Key], b)] -> View c d -> c
                        -> m (View a b)
newHorizontalDialogView resources text buttonSpecs bgView bgInput = do
  let width = 320
      margin = 16
  let textW = width - 2 * margin
  (textH, textView) <- newStaticTextWrapView resources textW text
  let buttonTop = margin + textH + 20
      buttonWidth = 80
      buttonHeight = 20
      buttonSpacing = 8
  let makeButton (label, keys, value) i =
        subView_ (Rect (width - margin - buttonWidth -
                        i * (buttonWidth + buttonSpacing))
                  buttonTop buttonWidth buttonHeight) <$>
        newSimpleTextButton resources label keys value
  buttonViews <- zipWithM makeButton buttonSpecs [0 ..]
  let height = buttonTop + buttonHeight + margin
  let dialog = compoundView $
               (subView_ (Rect margin margin textW textH) textView) :
               buttonViews
  newDialogView bgView bgInput dialog (Rect (half (screenWidth - width))
                                            (half (screenHeight - height))
                                            width height)

-------------------------------------------------------------------------------

newTextEntryDialogView :: (MonadDraw m) => Resources -> String -> String
                       -> (String -> Bool) -> View c d -> c
                       -> m (View a (Maybe String))
newTextEntryDialogView resources text initValue testFn bgView bgInput = do
  let width = 320
      margin = 16
      textW = width - 2 * margin
  (textH, textView) <- newStaticTextWrapView resources textW text
  let textBoxTop = margin + textH + 10
      textBoxH = 20
      buttonTop = textBoxTop + textBoxH + 10
      buttonW = 80
      buttonH = 20
      buttonSpacing = 8
      height = buttonTop + buttonH + margin
  stringRef <- newDrawRef initValue
  dialog <- compoundViewM [
    (return $ subView_ (Rect margin margin textW textH) textView),
    (subView_ (Rect margin textBoxTop textW textBoxH) .
     viewMapM (const $ readDrawRef stringRef)
              ((Suppress <$) . writeDrawRef stringRef) <$>
     newTextBox resources (return . testFn)),
    (subView_ (Rect (width - margin - 2 * buttonW - buttonSpacing)
                    buttonTop buttonW buttonH) <$>
     newSimpleTextButton resources "Cancel" [KeyEscape] Nothing),
    (subView_ (Rect (width - margin - buttonW) buttonTop buttonW buttonH) .
     viewMapM return (const $ fmap (Action . Just) $ readDrawRef stringRef) <$>
     newSimpleTextButton resources "OK" [KeyReturn] ())]
  newDialogView bgView bgInput dialog (Rect (half (screenWidth - width))
                                            (half (screenHeight - height))
                                            width height)

-------------------------------------------------------------------------------

newDialogBackgroundView :: (MonadDraw m) => m (View a b)
newDialogBackgroundView = wallView . const <$>
  newBackgroundPaint "gui/dialog-background.png" 0 0 8 8 64 64

-------------------------------------------------------------------------------
