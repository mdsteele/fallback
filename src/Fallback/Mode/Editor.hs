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

module Fallback.Mode.Editor
  (newEditorMode)
where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.IORef
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as Read

import Fallback.Constants (screenRect)
import Fallback.Control.Error (runEO, runIOEO)
import Fallback.Data.Clock (initClock)
import Fallback.Data.Point
import Fallback.Draw (paintScreen, runDraw)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog (newTextEntryDialogMode)
import Fallback.Mode.Narrate (newNarrateMode)
import Fallback.State.Minimap
  (newMinimapFromTerrainMap, updateMinimapFromTerrainMap)
import Fallback.State.Resources (Resources, rsrcTileset)
import Fallback.State.Terrain
import Fallback.State.Tileset
import Fallback.Utility (maybeM)
import Fallback.View (fromAction, viewHandler, viewPaint)
import Fallback.View.Editor

-------------------------------------------------------------------------------

newEditorMode :: Resources -> IO Mode
newEditorMode resources = do
  view <- runDraw (newEditorView resources)
  stateRef <- newEditorState resources >>= newIORef
  let

    mode EvQuit = do
      unsaved <- fmap esUnsaved $ readIORef stateRef
      if not unsaved then return DoQuit else
        return DoQuit -- TODO offer to save terrain
    mode (EvKeyDown KeyR [KeyModCmd] _) = do
      es <- readIORef stateRef
      ChangeMode <$> newTextEntryDialogMode
        resources "Enter new map dimensions:"
        (let (w, h) = tmapSize (esTerrain es) in show w ++ "x" ++ show h)
        (const True) (return mode) doResize view es
    mode (EvKeyDown KeyH [KeyModCmd] _) = do
      es <- readIORef stateRef
      ChangeMode <$> newTextEntryDialogMode resources
        "Enter shift delta:" "0,0" (const True) (return mode) doShift view es
    mode event = do
      when (event == EvTick) $ modifyIORef stateRef tickEditorState
      es <- readIORef stateRef
      action <- runDraw $ viewHandler view es screenRect event
      when (event == EvTick) $ paintScreen (viewPaint view es)
      case fromAction action of
        Nothing -> return SameMode
        Just (JumpMapTo _) -> return SameMode -- TODO implement this
        Just (ScrollMap delta) -> alterState $
          es { esCameraTopleft = esCameraTopleft es `pAdd` delta }
        Just (ScrollPalette top) -> alterState $
          es { esPaletteTop = max 0 top }
        Just (PickTile tile) -> alterState es { esBrush = tile }
        Just (AutoPaintAt pos) -> do
          let brush = autoPaintTile (esTileset es) (esTerrain es) pos
          when (ttId (tmapGet (esTerrain es) pos) /= ttId brush) $ do
            let terrain' = tmapSet [pos] brush (esTerrain es)
            updateMinimapFromTerrainMap (esMinimap es) terrain' [pos]
            setTerrain es terrain'
          return SameMode
        Just (PaintAt pos) -> do
          when (ttId (tmapGet (esTerrain es) pos) /= ttId (esBrush es)) $ do
            let terrain' = tmapSet [pos] (esBrush es) (esTerrain es)
            updateMinimapFromTerrainMap (esMinimap es) terrain' [pos]
            setTerrain es terrain'
          return SameMode
        Just (FloodFill pos) -> do
          when (ttId (tmapGet (esTerrain es) pos) /= ttId (esBrush es)) $ do
            let (terrain', filled) = floodFill pos (esBrush es) (esTerrain es)
            updateMinimapFromTerrainMap (esMinimap es) terrain' filled
            setTerrain es terrain'
          return SameMode
        Just DoSave -> do
          let doSave name = do
                saveTerrainMap name (esTerrain es)
                writeIORef stateRef es { esFilename = name, esUnsaved = False }
                return mode
          ChangeMode <$> newTextEntryDialogMode resources
            "Save terrain file as:" (esFilename es) (const True)
            (return mode) doSave view es
        Just DoLoad -> do
          let doLoad name = do
                eoTerrain <- runIOEO (loadTerrainMap resources name)
                case runEO eoTerrain of
                  Left errors -> newNarrateMode resources view es
                                   (intercalate "\n\n" errors) (return mode)
                  Right terrain -> do
                    writeIORef stateRef es { esCameraTopleft = pZero,
                                             esFilename = name,
                                             esRedoStack = [],
                                             esTerrain = terrain,
                                             esUndoStack = [],
                                             esUnsaved = False }
                    recreateMinimap
                    return mode
          ChangeMode <$> newTextEntryDialogMode resources
            "Load terrain file named:" (esFilename es) (const True)
            (return mode) doLoad view es
        Just DoUndo -> do
          case esUndoStack es of
            (undo : undos) -> do
              writeIORef stateRef es
                { esRedoStack = esTerrain es : esRedoStack es,
                  esTerrain = undo,
                  esUndoStack = undos,
                  esUnsaved = True }
              recreateMinimap
            [] -> return ()
          return SameMode
        Just DoRedo -> do
          case esRedoStack es of
            (redo : redos) -> do
              writeIORef stateRef es
                { esRedoStack = redos,
                  esTerrain = redo,
                  esUndoStack = esTerrain es : esUndoStack es,
                  esUnsaved = True }
              recreateMinimap
            [] -> return ()
          return SameMode

    alterState es' = writeIORef stateRef es' >> return SameMode

    setTerrain es terrain' = writeIORef stateRef $
      es { esRedoStack = [],
           esTerrain = terrain',
           esUndoStack = esTerrain es : take 10 (esUndoStack es),
           esUnsaved = True }

    doResize string = do
      let mbPair = fmap fst $ listToMaybe $ flip Read.readP_to_S string $ do
            w <- read <$> Read.munch isDigit
            _ <- Read.char 'x'
            h <- read <$> Read.munch isDigit
            Read.eof
            return (w, h)
      maybeM mbPair $ \size -> do
        es <- readIORef stateRef
        setTerrain es $ tmapResize (esNullTile es) size (esTerrain es)
        recreateMinimap
      return mode

    doShift string = do
      let mbPair = fmap fst $ listToMaybe $ flip Read.readP_to_S string $ do
            let parseInt = do
                  sign <- Read.option "" (Read.string "-")
                  digits <- Read.munch1 isDigit
                  return $ read $ sign ++ digits
            dx <- parseInt
            _ <- Read.char ','
            dy <- parseInt
            Read.eof
            return (Point dx dy)
      maybeM mbPair $ \delta -> do
        es <- readIORef stateRef
        setTerrain es $ tmapShift (esNullTile es) delta (esTerrain es)
        recreateMinimap
      return mode

    recreateMinimap = do
      es <- readIORef stateRef
      let tmap = esTerrain es
      minimap <- newMinimapFromTerrainMap tmap
      writeIORef stateRef es { esMinimap = minimap }

  return mode

-------------------------------------------------------------------------------

autoPaintTile :: Tileset -> TerrainMap -> Position -> TerrainTile
autoPaintTile tileset tmap pos =
  if ttId tile `elem` caveWallIds
  then case surround caveWallIds of
         (True, True, True, True, True, True, True, True) -> get 1422
         (True, _, False, _, True, _, True, _) -> get 8648
         (False, _, False, _, True, _, True, _) -> get 7655
         (False, _, True, _, True, _, True, _) -> get 7069
         (False, _, True, _, True, _, False, _) -> get 9022
         (True, _, True, _, True, _, False, _) -> get 9090
         (True, _, True, _, False, _, False, _) -> get 2636
         (True, _, True, _, False, _, True, _) -> get 8111
         (True, _, False, _, False, _, True, _) -> get 5652
         (True, True, True, False, True, True, True, True) -> get 2680
         (True, True, True, True, True, False, True, True) -> get 9166
         (True, True, True, True, True, True, True, False) -> get 5750
         (True, False, True, True, True, True, True, True) -> get 1212
         _ -> tile
  else tile
  where
    tile = tmapGet tmap pos
    caveWallIds = [1422, 8648, 7655, 7069, 9022, 9090, 2636, 8111, 5652,
                   2680, 9166, 5750, 1212]
    surround ids =
      let check dir = ttId (tmapGet tmap (pos `plusDir` dir)) `elem`
                      (ttId (tmapOffTile tmap) : ids)
      in (check DirE, check DirSE, check DirS, check DirSW,
          check DirW, check DirNW, check DirN, check DirNE)
    get tid = fromMaybe (error $ "no such tile: " ++ show tid) $
              tilesetLookup tid tileset

floodFill :: Position -> TerrainTile -> TerrainMap -> (TerrainMap, [Position])
floodFill start tile tmap =
  let fill [] visited = visited
      fill (pos : rest) visited =
        if Set.member pos visited || ttId (tmapGet tmap pos) /= startId
        then fill rest visited else
          fill (map (pos `plusDir`) cardinalDirections ++ rest)
               (Set.insert pos visited)
      startId = ttId (tmapGet tmap start)
      filled = Set.toList $ fill [start] Set.empty
  in (tmapSet filled tile tmap, filled)

newEditorState :: Resources -> IO EditorState
newEditorState resources = do
  let tileset = rsrcTileset resources
  let offTile = tilesetGet OffTile tileset
  let nullTile = tilesetGet NullTile tileset
  let tmap = makeEmptyTerrainMap (55, 44) offTile nullTile
  minimap <- newMinimapFromTerrainMap tmap
  return EditorState
    { esBrush = nullTile,
      esCameraTopleft = pZero,
      esClock = initClock,
      esFilename = "",
      esMinimap = minimap,
      esNullTile = nullTile,
      esPaletteTop = 0,
      esRedoStack = [],
      esTerrain = tmap,
      esTileArray = tilesetArray tileset, -- TODO remove this field
      esTileset = tileset,
      esUndoStack = [],
      esUnsaved = False }

-------------------------------------------------------------------------------
