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

module Fallback.View.Editor
  (EditorState(..), tickEditorState,
   EditorAction(..), newEditorView)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.Array (Array, bounds, range)
import Data.List (find)

import Fallback.Constants
  (cameraWidth, cameraHeight, sidebarWidth, tileHeight, tileWidth)
import Fallback.Data.Clock (Clock, clockInc, clockMod)
import Fallback.Data.Color (Tint(Tint), blackColor)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcFont, rsrcTerrainOverlaySprite,
   rsrcTerrainSprite)
import Fallback.State.Terrain
import Fallback.State.Tileset
import Fallback.Utility (ceilDiv)
import Fallback.View.Base
import Fallback.View.Camera (paintTerrainFullyExplored)
import Fallback.View.Hover
import Fallback.View.Quiver
import Fallback.View.Sidebar (newMinimapView)
import Fallback.View.Widget (makeLabel, newScrollZone)

-------------------------------------------------------------------------------

data EditorAction = ScrollMap IPoint
                  | JumpMapTo Position
                  | ScrollPalette Int
                  | PickTile TerrainTile
                  | AutoPaintAt Position
                  | PaintAt Position
                  | FloodFill Position
                  | DoLoad
                  | DoSave
                  | DoUndo
                  | DoRedo

data EditorState = EditorState
  { esBrush :: TerrainTile,
    esCameraTopleft :: IPoint,
    esClock :: Clock,
    esFilename :: String,
    esMinimap :: Minimap,
    esNullTile :: TerrainTile,
    esPaletteTop :: Int,
    esRedoStack :: [TerrainMap],
    esTerrain :: TerrainMap,
    esTileArray :: Array Int TerrainTile,
    esTileset :: Tileset,
    esUndoStack :: [TerrainMap],
    esUnsaved :: Bool }

esCameraRect :: EditorState -> IRect
esCameraRect es = let Point x y = esCameraTopleft es
                  in Rect x y cameraWidth cameraHeight

tickEditorState :: EditorState -> EditorState
tickEditorState es = es { esClock = clockInc (esClock es) }

-------------------------------------------------------------------------------

newEditorView :: Resources -> Draw z (View EditorState EditorAction)
newEditorView resources = do
  ref <- newHoverRef Nothing
  let mapRect _ (w, h) = Rect sidebarWidth 0 (w - sidebarWidth) h
  let sidebarRect _ (_, h) = Rect 0 0 sidebarWidth h
  hoverJunction ref <$> compoundViewM [
    (subView mapRect <$> newEditorMapView resources (hoverSink ref)),
    (subView sidebarRect <$> newEditorSidebarView resources ref)]

-------------------------------------------------------------------------------

newEditorMapView :: Resources -> HoverSink (Maybe Position)
                 -> Draw z (View EditorState EditorAction)
newEditorMapView resources sink = do
  quiver <- newQuiver
  dragRef <- newDrawRef False
  keyARef <- newDrawRef False
  keyERef <- newDrawRef False
  keyFRef <- newDrawRef False
  let

    paint state = do
      paintTerrainFullyExplored resources (esCameraTopleft state)
                                (esTerrain state) (esClock state)

    handler _ _ EvTick =
      maybe Ignore (Action . ScrollMap . (`pMul` 8) . dirDelta) <$>
      quiverDirection quiver
    handler state rect (EvMouseMotion pt _) = do
      writeHoverSink sink $ positionAt state rect pt
      drag <- readDrawRef dragRef
      if drag then paintAt state rect pt else return Ignore
    handler state rect (EvMouseDown pt) = do
      eyedropper <- readDrawRef keyERef
      fill <- readDrawRef keyFRef
      (if eyedropper
       then return $
            maybe Ignore (Action . PickTile . tmapGet (esTerrain state)) $
            positionAt state rect pt
       else if fill
       then return $ maybe Ignore (Action . FloodFill) $
            positionAt state rect pt
       else writeDrawRef dragRef True >> paintAt state rect pt)
    handler _ _ (EvMouseUp _) = Ignore <$ writeDrawRef dragRef False
    handler _ _ (EvKeyDown KeyA _ _) = Suppress <$ writeDrawRef keyARef True
    handler _ _ (EvKeyDown KeyE _ _) = Suppress <$ writeDrawRef keyERef True
    handler _ _ (EvKeyDown KeyF _ _) = Suppress <$ writeDrawRef keyFRef True
    handler _ _ (EvKeyDown KeyO [KeyModCmd] _) = return (Action DoLoad)
    handler _ _ (EvKeyDown KeyS [KeyModCmd] _) = return (Action DoSave)
    handler _ _ (EvKeyDown KeyZ [KeyModCmd] _) = return (Action DoUndo)
    handler _ _ (EvKeyDown KeyZ [KeyModCmd, KeyModShift] _) =
      return (Action DoRedo)
    handler _ _ (EvKeyDown key _ _) = Ignore <$ quiverKeyDown quiver key
    handler _ _ (EvKeyUp KeyA) = Ignore <$ writeDrawRef keyARef False
    handler _ _ (EvKeyUp KeyE) = Ignore <$ writeDrawRef keyERef False
    handler _ _ (EvKeyUp KeyF) = Ignore <$ writeDrawRef keyFRef False
    handler _ _ (EvKeyUp key) = Ignore <$ quiverKeyUp quiver key
    handler state rect (EvFocus pt) = do
      writeHoverSink sink $ positionAt state rect pt
      return Ignore
    handler _ _ EvBlur = do
      resetQuiver quiver
      writeDrawRef dragRef False
      writeDrawRef keyARef False
      writeDrawRef keyERef False
      writeDrawRef keyFRef False
      return Ignore
    handler _ _ _ = return Ignore

    paintAt state rect pt = do
      auto <- readDrawRef keyARef
      let action = if auto then AutoPaintAt else PaintAt
      return $ maybe Ignore (Action . action) $ positionAt state rect pt
    positionAt state rect pt =
      if not (rectContains rect pt) then Nothing
      else Just $ pointPosition $
           pt `pSub` rectTopleft rect `pAdd` esCameraTopleft state

  return $ View paint handler

-------------------------------------------------------------------------------

newEditorSidebarView :: Resources -> HoverRef (Maybe Position)
                     -> Draw z (View EditorState EditorAction)
newEditorSidebarView resources ref = do
  bgSprite <- loadSprite "gui/sidebar-background.png"
  let font = rsrcFont resources FontGeorgia10
  hoverView (hoverSink ref) Nothing <$> compoundViewM [
    (return $ inertView $ const $ canvasRect >>= blitStretch bgSprite),
    -- Minimap:
    (subView_ (Rect 2 2 (sidebarWidth - 10) 92) .
     viewMap (esCameraRect &&& esMinimap) JumpMapTo <$> newMinimapView),
    -- Palette and scroll bar:
    (subView_ (Rect 12 114 88 340) <$> newEditorPaletteView resources),
    (subView_ (Rect 12 114 101 340) .
     vmap (\es -> (0, (snd $ bounds $ esTileArray es) `ceilDiv` 3, 9,
                   esPaletteTop es)) .
     fmap ScrollPalette <$> newScrollZone),
    -- Position label:
    (viewMapM (const (readHoverRef ref)) (return . Action) <$>
     (newMaybeView (fmap show) $ makeLabel font blackColor $ \(_, h) ->
        LocBottomleft $ Point 2 (h - 2)))]

newEditorPaletteView :: Resources -> Draw z (View EditorState EditorAction)
newEditorPaletteView resources = do
  let

    paint state = do
      let paintTile (rect, tile) = do
            case ttAppearance tile of
              Still row col -> do
                blitStretch (rsrcTerrainSprite resources (row, col)) rect
              Anim row c0 slowdown overlay -> do
                let col = c0 + clockMod 4 slowdown (esClock state)
                blitStretch (rsrcTerrainSprite resources (row, col)) rect
                case overlay of
                  NoOverlay -> return ()
                  Overlay r c -> do
                    blitStretch (rsrcTerrainOverlaySprite resources r c) rect
            when (ttId tile == ttId (esBrush state)) $ do
              drawRect (Tint 255 0 255 255) rect
      canvasSize >>= (mapM_ paintTile . rectsAndTiles state)

    handler state rect (EvMouseDown pt) =
      let hit rAndT = rectContains (fst rAndT) (pt `pSub` rectTopleft rect)
      in return $ maybe Ignore (Action . PickTile . snd) $ find hit $
         rectsAndTiles state $ rectSize rect
    handler _ _ _ = return Ignore

    rectsAndTiles state (width, height) =
      let gap = 2
          numCols = (width + gap) `div` (tileWidth + gap)
          numRows = (height + gap) `div` (tileHeight + gap)
          (lo, hi) = bounds (esTileArray state)
          start = min hi (esPaletteTop state * numCols + lo + 1)
          rAndT index = let (row, col) = (index - start) `divMod` numCols
                        in (Rect (col * (tileWidth + gap))
                                 (row * (tileHeight + gap))
                                 tileWidth tileHeight,
                            esTileArray state ! index)
      in map rAndT $ range (start, min hi (start + numCols * numRows - 1))

  return $ View paint handler

-------------------------------------------------------------------------------
