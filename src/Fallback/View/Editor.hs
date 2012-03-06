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
  (EditorState(..), newEditorState, tickEditorState,
   EditorAction(..), newEditorView)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.Array (Array, bounds, range)
import Data.List (find)

import Fallback.Constants
  (cameraWidth, cameraHeight, sidebarWidth, tileHeight, tileWidth)
import Fallback.Data.Clock
import Fallback.Data.Color (Tint(Tint), blackColor)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.State.Resources (FontTag(..), Resources, rsrcFont, rsrcTileset)
import Fallback.State.Terrain
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
    esTileset :: Array Int TerrainTile,
    esUndoStack :: [TerrainMap],
    esUnsaved :: Bool }

esCameraRect :: EditorState -> IRect
esCameraRect es = let Point x y = esCameraTopleft es
                  in Rect x y cameraWidth cameraHeight

newEditorState :: Resources -> IO EditorState
newEditorState resources = do
  let tileset = rsrcTileset resources
  let offTile = tileset ! 0
      nullTile = tileset ! 1
  let terrain = makeEmptyTerrain (55, 44) offTile nullTile
  minimap <- newMinimap $ tmapSize terrain
  updateMinimap minimap terrain $ tmapAllPositions terrain
  return EditorState
    { esBrush = nullTile,
      esCameraTopleft = pZero,
      esClock = initClock,
      esFilename = "",
      esMinimap = minimap,
      esNullTile = nullTile,
      esPaletteTop = 0,
      esRedoStack = [],
      esTerrain = terrain,
      esTileset = tileset,
      esUndoStack = [],
      esUnsaved = False }

tickEditorState :: EditorState -> EditorState
tickEditorState es = es { esClock = clockInc (esClock es) }

-------------------------------------------------------------------------------

newEditorView :: Resources -> Draw z (View EditorState EditorAction)
newEditorView resources = do
  ref <- newHoverRef Nothing
  let mapRect _ (w, h) = Rect sidebarWidth 0 (w - sidebarWidth) h
  let sidebarRect _ (_, h) = Rect 0 0 sidebarWidth h
  hoverJunction ref <$> compoundViewM [
    (subView mapRect <$> newEditorMapView (hoverSink ref)),
    (subView sidebarRect <$> newEditorSidebarView resources ref)]

-------------------------------------------------------------------------------

newEditorMapView :: HoverSink (Maybe Position)
                 -> Draw z (View EditorState EditorAction)
newEditorMapView sink = do
  quiver <- newQuiver
  dragRef <- newDrawRef False
  keyFRef <- newDrawRef False
  keyERef <- newDrawRef False
  let

    paint state = do
      paintTerrainFullyExplored (esCameraTopleft state) (esTerrain state)
                                (esClock state)

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
    handler _ _ (EvKeyDown KeyE _ _) = Suppress <$ writeDrawRef keyERef True
    handler _ _ (EvKeyDown KeyF _ _) = Suppress <$ writeDrawRef keyFRef True
    handler _ _ (EvKeyDown KeyO [KeyModCmd] _) = return (Action DoLoad)
    handler _ _ (EvKeyDown KeyS [KeyModCmd] _) = return (Action DoSave)
    handler _ _ (EvKeyDown KeyZ [KeyModCmd] _) = return (Action DoUndo)
    handler _ _ (EvKeyDown KeyZ [KeyModCmd, KeyModShift] _) =
      return (Action DoRedo)
    handler _ _ (EvKeyDown key _ _) = Ignore <$ quiverKeyDown quiver key
    handler _ _ (EvKeyUp KeyE) = Ignore <$ writeDrawRef keyERef False
    handler _ _ (EvKeyUp KeyF) = Ignore <$ writeDrawRef keyFRef False
    handler _ _ (EvKeyUp key) = Ignore <$ quiverKeyUp quiver key
    handler state rect (EvFocus pt) = do
      writeHoverSink sink $ positionAt state rect pt
      return Ignore
    handler _ _ EvBlur = do
      resetQuiver quiver
      writeDrawRef dragRef False
      writeDrawRef keyERef False
      writeDrawRef keyFRef False
      return Ignore
    handler _ _ _ = return Ignore

    paintAt state rect pt = return $ maybe Ignore (Action . PaintAt) $
                            positionAt state rect pt
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
    (subView_ (Rect 12 114 88 340) <$> newEditorPaletteView),
    (subView_ (Rect 12 114 101 340) .
     vmap (\es -> (0, (snd $ bounds $ esTileset es) `ceilDiv` 3, 9,
                   esPaletteTop es)) .
     fmap ScrollPalette <$> newScrollZone),
    -- Position label:
    (viewMapM (const (readHoverRef ref)) (return . Action) <$>
     (newMaybeView (fmap show) $ makeLabel font blackColor $ \(_, h) ->
        LocBottomleft $ Point 2 (h - 2)))]

newEditorPaletteView :: Draw z (View EditorState EditorAction)
newEditorPaletteView = do
  let

    paint state = do
      let paintTile (rect, tile) = do
            blitStretch (ttSprite tile $ esClock state) rect
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
          (lo, hi) = bounds (esTileset state)
          start = min hi (esPaletteTop state * numCols + lo + 1)
          rAndT index = let (row, col) = (index - start) `divMod` numCols
                        in (Rect (col * (tileWidth + gap))
                                 (row * (tileHeight + gap))
                                 tileWidth tileHeight,
                            esTileset state ! index)
      in map rAndT $ range (start, min hi (start + numCols * numRows - 1))

  return $ View paint handler

-------------------------------------------------------------------------------
