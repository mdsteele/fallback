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

module Fallback.View.LoadGame
  (LoadGameAction(..), newLoadGameView)
where

import Control.Applicative ((<$>))
import Data.Array (Array, bounds, listArray)

import Fallback.Data.Color (Tint(Tint), blackColor)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.Scenario.Save (SavedGameSummary(..))
import Fallback.State.Resources (FontTag(..), Resources, rsrcFont)
import Fallback.View.Base
import Fallback.View.Dialog (newDialogView)
import Fallback.View.Widget
  (ButtonStatus(DisabledButton), newScrollZone, newSimpleTextButton,
   newTextButton)

-------------------------------------------------------------------------------

data LoadGameAction = CancelLoadGame | DoLoadGame SavedGameSummary

-------------------------------------------------------------------------------

newLoadGameView :: (MonadDraw m) => Resources -> View c d -> c
                -> [SavedGameSummary] -> m (View () LoadGameAction)
newLoadGameView resources bgView bgInput summaries = do
  dialog <- newLoadGameDialog resources summaries
  newDialogView bgView bgInput dialog $ Rect 120 25 400 440

newLoadGameDialog :: (MonadDraw m) => Resources -> [SavedGameSummary]
                  -> m (View () LoadGameAction)
newLoadGameDialog resources []  = do
  let font = rsrcFont resources FontGeorgia12
  let paintMessage () = do
        center <- rectCenter <$> canvasRect
        drawText font blackColor (LocCenter center)
                 "There are no saved games to load."
  compoundViewM [
    (subView (\_ (w, h) -> Rect 0 0 w (h - 40)) <$>
     return (inertView paintMessage)),
    (subView (\_ (_, h) -> Rect 16 (h - 40) 100 24) <$>
     newSimpleTextButton resources "Cancel" [KeyEscape, KeyReturn]
                         CancelLoadGame),
    (subView (\_ (w, h) -> Rect (w - 116) (h - 40) 100 24) .
     vmap (const ("Load Game", DisabledButton)) <$>
     newTextButton resources [] CancelLoadGame)]
newLoadGameDialog resources summaries = do
  let upperBound = length summaries - 1
  stateRef <- newDrawRef $ InternalState 0 0 $
              listArray (0, upperBound) summaries
  let handleAction action = do
        state <- readDrawRef stateRef
        writeDrawRef stateRef $ case action of
          ScrollSummaries top ->
            state { nsScrollTop = max 0 $
                                  min ((snd $ bounds $ nsSummaries state) -
                                       numEntriesVisible) top }
          SelectSummary index -> state { nsSelectedIndex = index }
        return Suppress
  compoundViewM [
    (viewMapM (const $ readDrawRef stateRef) handleAction <$> compoundViewM [
       (subView (\_ (w, _) -> Rect 20 20 (w - 40) 110) .
        vmap nsSelectedSummary <$> newSelectedSummaryView resources),
       (subView (\_ (w, h) -> Rect 30 140 (w - 65) (h - 190)) <$>
        newSummariesListView resources upperBound),
       (subView (\_ (w, h) -> Rect 30 140 (w - 50) (h - 190)) .
        viewMap (\ns -> (0, upperBound, numEntriesVisible, nsScrollTop ns))
                ScrollSummaries <$> newScrollZone)]),
    (subView (\_ (_, h) -> Rect 16 (h - 40) 100 24) <$>
     newSimpleTextButton resources "Cancel" [KeyEscape] CancelLoadGame),
    (viewMapM return (const (Action . DoLoadGame . nsSelectedSummary <$>
                             readDrawRef stateRef)) .
     subView (\_ (w, h) -> Rect (w - 116) (h - 40) 100 24) <$>
     newSimpleTextButton resources "Load Game" [KeyReturn] ())]

numEntriesVisible :: Int
numEntriesVisible = 6

-------------------------------------------------------------------------------

data InternalState = InternalState
  { nsScrollTop :: Int,
    nsSelectedIndex :: Int,
    nsSummaries :: Array Int SavedGameSummary }

nsSelectedSummary :: InternalState -> SavedGameSummary
nsSelectedSummary ns = nsSummaries ns ! nsSelectedIndex ns

data InternalAction = ScrollSummaries Int
                    | SelectSummary Int

newSelectedSummaryView :: (MonadDraw m) => Resources
                       -> m (View SavedGameSummary a)
newSelectedSummaryView resources = do
  let nameFont = rsrcFont resources FontChancery14
  let locFont = rsrcFont resources FontGeorgiaBold12
  let timeFont = rsrcFont resources FontGeorgia12
  let
    paint sgs = do
      blitStretch (sgsScreenshot sgs) (Rect 10 7 128 96 :: IRect)
      drawText nameFont blackColor (LocTopleft (Point 160 20 :: IPoint))
               (sgsName sgs)
      drawText locFont blackColor (LocTopleft (Point 160 50 :: IPoint))
               (sgsLocation sgs)
      drawText timeFont blackColor (LocTopleft (Point 160 80 :: IPoint))
               (sgsTimeSaved sgs)
      drawBevelRect (Tint 0 0 0 128) 5 =<< canvasRect
  return (inertView paint)

newSummariesListView :: (MonadDraw m) => Resources -> Int
                     -> m (View InternalState InternalAction)
newSummariesListView resources upperBound = do
  let spacing = 3
  let itemRect offset _ (w, h) =
        let h' = (h - spacing * (numEntriesVisible - 1)) `div`
                 numEntriesVisible
        in Rect 0 ((h' + spacing) * offset) w h'
  let newItem offset =
        subView (itemRect offset) <$> newSummaryItemView resources offset
  compoundView <$> mapM newItem [0 .. min upperBound (numEntriesVisible - 1)]

newSummaryItemView :: (MonadDraw m) => Resources -> Int
                   -> m (View InternalState InternalAction)
newSummaryItemView resources offset = do
  let nameFont = rsrcFont resources FontGeorgiaBold12
  let infoFont = rsrcFont resources FontGeorgia12
  let

    paint (ns, mbPt) = do
      rect <- canvasRect
      let sgs = nsSummaries ns ! getIndex ns
      drawText nameFont blackColor (LocTopleft (Point 6 5 :: IPoint))
               (sgsName sgs)
      drawText infoFont blackColor (LocTopleft (Point 6 22 :: IPoint))
               (sgsLocation sgs)
      drawText infoFont blackColor (LocTopright (Point (rectW rect - 6) 22))
               (sgsTimeSaved sgs)
      let tint = if getIndex ns == nsSelectedIndex ns then Tint 0 192 0 255
                 else if maybe False (rectContains rect) mbPt
                      then Tint 64 128 64 255 else Tint 64 64 64 128
      drawBevelRect tint 3 rect

    handler (ns, _) (EvMouseDown pt) = do
      whenWithinCanvas pt $ return $ Action $ SelectSummary $ getIndex ns
    handler _ _ = return Ignore

    getIndex ns = nsScrollTop ns + offset

  newMouseView $ View paint handler

-------------------------------------------------------------------------------
