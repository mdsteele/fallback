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

module Fallback.View.Hover
  (HoverRef, newHoverRef, reopenHoverRef, readHoverRef,
   HoverSink, hoverSink, writeHoverSink,
   hoverView, hoverView', hoverJunction,
   Cursor(..), newCursorView)
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)

import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event (Event(..))
import Fallback.State.Item (itemIconCoords)
import Fallback.State.Resources (Resources, rsrcCursorsStrip, rsrcItemIcon)
import Fallback.State.Tags (ItemTag)
import Fallback.Utility (maybeM)
import Fallback.View.Base (View(View))

-------------------------------------------------------------------------------

data HoverRef a = HoverRef
  { hovCurrent :: DrawRef a,
    hovOpen :: DrawRef Bool }

newHoverRef :: (MonadDraw m) => a -> m (HoverRef a)
newHoverRef value = HoverRef <$> newDrawRef value <*> newDrawRef True

reopenHoverRef :: (MonadDraw m) => HoverRef a -> m ()
reopenHoverRef hov = writeDrawRef (hovOpen hov) True

readHoverRef :: (MonadDraw m) => HoverRef a -> m a
readHoverRef = readDrawRef . hovCurrent

newtype HoverSink a = HoverSink (HoverRef a)

hoverSink :: HoverRef a -> HoverSink a
hoverSink = HoverSink

writeHoverSink :: (MonadDraw m) => HoverSink a -> a -> m ()
writeHoverSink (HoverSink hov) value = do
  open <- readDrawRef (hovOpen hov)
  when open $ do
    writeDrawRef (hovOpen hov) False
    writeDrawRef (hovCurrent hov) value

-------------------------------------------------------------------------------

hoverView :: HoverSink c -> c -> View a b -> View a b
hoverView sink value view = hoverView' sink (const $ Just value) view

hoverView' :: HoverSink c -> (a -> Maybe c) -> View a b -> View a b
hoverView' sink valueFn (View paint handler) =
  let handler' input event = do
        result <- handler input event
        rect <- canvasRect
        let check pt = when (rectContains rect pt) $
                       maybeM (valueFn input) (writeHoverSink sink)
        case event of
          EvTick -> do
            mbPt <- getRelativeMousePos
            maybeM mbPt check
          EvMouseMotion pt _ -> check pt
          EvMouseDown pt -> check pt
          EvMouseUp pt -> check pt
          _ -> return ()
        return result
  in View paint handler'

hoverJunction :: HoverRef c -> View a b -> View a b
hoverJunction ref (View paint handler) = View paint handler' where
  handler' input event = reopenHoverRef ref >> handler input event

-------------------------------------------------------------------------------

data Cursor = DefaultCursor
            | HandCursor
            | InfoCursor
            | ItemCursor ItemTag
            | SwapCursor Direction
            | TargetCursor
            | TargetNCursor Int
            | TalkCursor
            | WalkCursor Direction

paintCursor :: Resources -> Cursor -> IPoint -> Paint ()
paintCursor resources cursor mousePt =
  case cursor of
    DefaultCursor -> paint 0 (Point 1 1)
    HandCursor -> paint 16 (Point 8 8)
    InfoCursor -> paint 1 (Point 7 13)
    ItemCursor tag -> blitLoc (rsrcItemIcon resources $ itemIconCoords tag)
                              (LocCenter mousePt)
    SwapCursor DirE -> paint 11 (Point 20 8)
    SwapCursor DirNE -> paint 12 (Point 14 1)
    SwapCursor DirN -> paint 13 (Point 9 2)
    SwapCursor DirNW -> paint 14 (Point 1 1)
    SwapCursor DirW -> paint 11 (Point 2 6)
    SwapCursor DirSW -> paint 12 (Point 1 14)
    SwapCursor DirS -> paint 13 (Point 7 20)
    SwapCursor DirSE -> paint 14 (Point 14 14)
    TalkCursor -> paint 15 (Point 4 14)
    TargetCursor -> paint 0 (Point 1 1) -- FIXME
    TargetNCursor _ -> paint 0 (Point 1 1) -- FIXME
    WalkCursor DirE -> paint 3 (Point 14 7)
    WalkCursor DirNE -> paint 4 (Point 14 1)
    WalkCursor DirN -> paint 5 (Point 7 1)
    WalkCursor DirNW -> paint 6 (Point 1 1)
    WalkCursor DirW -> paint 7 (Point 1 7)
    WalkCursor DirSW -> paint 8 (Point 1 14)
    WalkCursor DirS -> paint 9 (Point 7 14)
    WalkCursor DirSE -> paint 10 (Point 14 14)
  where paint index offset =
          blitTopleft (rsrcCursorsStrip resources ! index)
                      (mousePt `pSub` offset)

newCursorView :: (MonadDraw m) => Resources
              -> (HoverSink Cursor -> m (View a b)) -> m (View a b)
newCursorView resources fn = do
  hov <- newHoverRef DefaultCursor
  View paint handler <- fn (hoverSink hov)
  let
    paint' input = do
      cursor <- readHoverRef hov
      paint input
      mbMousePt <- getRelativeMousePos
      maybeM mbMousePt (paintCursor resources cursor)
    handler' input event = do
      reopenHoverRef hov
      handler input event
  return (View paint' handler')

-------------------------------------------------------------------------------
