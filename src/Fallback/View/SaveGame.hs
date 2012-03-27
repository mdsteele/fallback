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

module Fallback.View.SaveGame
  (SaveGameAction(..), newSaveGameView)
where

import Control.Applicative ((<$>))
import Data.Array (Array, bounds, elems, listArray)
import Data.List (findIndex)

import Fallback.Data.Color (Tint(Tint), blackColor)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.Scenario.Save (SavedGameSummary(..))
import Fallback.State.Resources (FontTag(..), Resources, rsrcFont)
import Fallback.View.Base
import Fallback.View.Dialog (newDialogView)
import Fallback.View.Widget
  (enabledIf, newScrollZone, newSimpleTextButton, newTextBox, newTextButton)

-------------------------------------------------------------------------------

data SaveGameAction = CancelSaveGame | DoSaveGame String

-------------------------------------------------------------------------------

newSaveGameView :: (MonadDraw m) => Resources -> View c d -> c -> Sprite
                -> String -> [SavedGameSummary] -> m (View () SaveGameAction)
newSaveGameView resources bgView bgInput screenshot location summaries = do
  dialog <- newSaveGameDialog resources screenshot location summaries
  newDialogView bgView bgInput dialog $ Rect 120 25 400 440

newSaveGameDialog :: (MonadDraw m) => Resources -> Sprite -> String
                  -> [SavedGameSummary] -> m (View () SaveGameAction)
newSaveGameDialog resources screenshot location summaries = do
  let upperBound = length summaries - 1
  stateRef <- newDrawRef $ InternalState
                { nsLocation = location,
                  nsSaveName = "",
                  nsScreenshot = screenshot,
                  nsScrollTop = 0,
                  nsSummaries = listArray (0, upperBound) summaries }
  let handleAction action = do
        ns <- readDrawRef stateRef
        writeDrawRef stateRef $ case action of
          ScrollSummaries top ->
            ns { nsScrollTop = max 0 $ min ((snd $ bounds $ nsSummaries ns) -
                                            numEntriesVisible) top }
          SetSaveName name ->
            let mbIndex = findIndex ((name ==) . sgsName) $
                          elems $ nsSummaries ns
                top = nsScrollTop ns
                fn ix = if ix < top then ix
                        else if ix >= top + numEntriesVisible
                             then ix - numEntriesVisible + 1 else top
            in ns { nsSaveName = name, nsScrollTop = maybe top fn mbIndex }
        return Suppress
  compoundViewM [
    (viewMapM (const $ readDrawRef stateRef) handleAction <$> compoundViewM [
       (subView (\_ (w, _) -> Rect 20 20 (w - 40) 110) <$>
        newSaveSummaryView resources),
       (subView (\_ (w, h) -> Rect 30 140 (w - 65) (h - 190)) <$>
        newSummariesListView resources upperBound),
       (subView (\_ (w, h) -> Rect 30 140 (w - 50) (h - 190)) .
        viewMap (\ns -> (0, upperBound, numEntriesVisible, nsScrollTop ns))
                ScrollSummaries <$> newScrollZone)]),
    (subView (\_ (_, h) -> Rect 16 (h - 40) 100 24) <$>
     newSimpleTextButton resources "Cancel" [KeyEscape] CancelSaveGame),
    (viewMapM (const ((,) "Save Game" . enabledIf . isLegalSaveName .
                      nsSaveName <$> readDrawRef stateRef))
              (const (Action . DoSaveGame . trimSaveName . nsSaveName <$>
                      readDrawRef stateRef)) .
     subView (\_ (w, h) -> Rect (w - 116) (h - 40) 100 24) <$>
     newTextButton resources [KeyReturn] ())]

isLegalSaveName :: String -> Bool
isLegalSaveName = any $ \c -> ' ' < c && c < '\DEL'

trimSaveName :: String -> String
trimSaveName = let fn = reverse . dropWhile (' ' ==) in fn . fn

numEntriesVisible :: Int
numEntriesVisible = 6

-------------------------------------------------------------------------------

data InternalState = InternalState
  { nsLocation :: String,
    nsSaveName :: String,
    nsScreenshot :: Sprite,
    nsScrollTop :: Int,
    nsSummaries :: Array Int SavedGameSummary }

data InternalAction = ScrollSummaries Int
                    | SetSaveName String

newSaveSummaryView :: (MonadDraw m) => Resources
                   -> m (View InternalState InternalAction)
newSaveSummaryView resources = do
  let locFont = rsrcFont resources FontGeorgiaBold12
  let
    paintBackground ns = do
      blitStretch (nsScreenshot ns) (Rect 10 7 128 96 :: IRect)
      drawText locFont blackColor (LocTopleft (Point 160 50 :: IPoint))
               (nsLocation ns)
      drawBevelRect (Tint 0 0 0 128) 5 =<< canvasRect
  compoundViewM [
    (return $ inertView paintBackground),
    (subView (\_ (w, _) -> Rect 160 20 (w - 190) 20) .
     viewMap nsSaveName SetSaveName <$>
     newTextBox resources (const $ return True))]

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

    paint ns = do
      rect <- canvasRect
      let sgs = getSummary ns
      drawText nameFont blackColor (LocTopleft (Point 6 5 :: IPoint))
               (sgsName sgs)
      drawText infoFont blackColor (LocTopleft (Point 6 22 :: IPoint))
               (sgsLocation sgs)
      drawText infoFont blackColor (LocTopright (Point (rectW rect - 6) 22))
               (sgsTimeSaved sgs)
      tint <- if sgsName sgs == trimSaveName (nsSaveName ns)
              then return (Tint 0 192 0 255) else do
        mbMousePt <- getRelativeMousePos
        return $ if maybe False (rectContains rect) mbMousePt
                 then Tint 64 128 64 255 else Tint 64 64 64 128
      drawBevelRect tint 3 rect

    handler ns (EvMouseDown pt) = do
      whenWithinCanvas pt $ do
        return $ Action $ SetSaveName $ sgsName $ getSummary ns
    handler _ _ = return Ignore

    getSummary ns = nsSummaries ns ! (nsScrollTop ns + offset)

  return $ View paint handler

-------------------------------------------------------------------------------
