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
import Data.Char (isAlphaNum, isDigit)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.IORef
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as Read
import Text.ParserCombinators.ReadPrec (readPrec_to_P)
import Text.Read (readPrec)

import Fallback.Constants (cameraCenterOffset)
import Fallback.Control.Error (runEO, runIOEO)
import Fallback.Data.Clock (initClock)
import Fallback.Data.Point
import Fallback.Draw (handleScreen, paintScreen)
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
  view <- newEditorView resources
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
      action <- handleScreen $ viewHandler view es event
      when (event == EvTick) $ paintScreen (viewPaint view es)
      case fromAction action of
        Nothing -> return SameMode
        Just (JumpMapTo pos) -> alterState $
          es { esCameraTopleft =
                 round <$> (positionCenter pos `pSub` cameraCenterOffset) }
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
        Just (ChangeMarks pos) -> do
          ChangeMode <$> newTextEntryDialogMode
            resources ("Set marks for " ++ show pos ++ ":")
            (intercalate "," $ Set.toList $ tmapGetMarks pos $ esTerrain es)
            (const True) (return mode) (doSetMarks pos) view es
        Just (ChangeRects pos) -> do
          ChangeMode <$> newTextEntryDialogMode
            resources ("Set rect:")
            (maybe ("R:" ++ show (makeRect pos (1, 1)))
                   (\(s, r) -> s ++ ":" ++ show r) $
             find (flip rectContains pos . snd) $ tmapAllRects $ esTerrain es)
            (const True) (return mode) doSetRect view es
        Just DoSave -> do
          let doSave name = do
                eo <- runIOEO $ saveTerrainMap name $ esTerrain es
                case runEO eo of
                  Left errors -> newNarrateMode resources view es
                                   (intercalate "\n\n" errors) (return mode)
                  Right () -> do
                    writeIORef stateRef es { esFilename = name,
                                             esUnsaved = False }
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

    doSetMarks pos string = do
      let mbKeys = fmap fst $ listToMaybe $ flip Read.readP_to_S string $ do
            ks <- Read.sepBy (Read.munch1 isAlphaNum) (Read.char ',')
            Read.eof
            return ks
      maybeM mbKeys $ \keys -> do
        es <- readIORef stateRef
        setTerrain es $ tmapSetMarks pos (Set.fromList keys) (esTerrain es)
      return mode

    doSetRect string = do
      let mbSetting = fmap fst $ listToMaybe $ flip Read.readP_to_S string $ do
            key <- Read.munch1 isAlphaNum
            mbRect <- Read.option Nothing $ do
              Read.char ':' >> fmap Just (readPrec_to_P readPrec 11)
            Read.eof
            return (key, mbRect)
      maybeM mbSetting $ \(key, mbRect) -> do
        es <- readIORef stateRef
        setTerrain es $ tmapSetRect key mbRect (esTerrain es)
      return mode

    doResize string = do
      let mbPair = fmap fst $ listToMaybe $ flip Read.readP_to_S string $ do
            w <- read <$> Read.munch1 isDigit
            _ <- Read.char 'x'
            h <- read <$> Read.munch1 isDigit
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
autoPaintTile tileset tmap pos = get $
  case ttId tile of
    c | caveWall c ->
        case nearbyTileIds of
          (e, s, w, n, se, sw, nw, ne)
            | all wall [e, s, w, n, se, sw, nw, ne] -> 1422
            | all wall [c, e, w, n] && none wall [s] -> 8648
            | all wall [c, w, n] && none wall [e, s] -> 7655
            | all wall [c, s, w, n] && none wall [e] -> 7069
            | all wall [c, s, w] && none wall [e, n] -> 9022
            | all wall [c, e, s, w] && none wall [n] -> 9090
            | all wall [c, e, s] && none wall [n, w] -> 2636
            | all wall [c, e, s, n] && none wall [w] -> 8111
            | all wall [c, e, n] && none wall [s, w] -> 5652
            | all wall [c, e, s, w, n, se, nw, ne] && not (wall sw) -> 2680
            | all wall [c, e, s, w, n, se, sw, ne] && not (wall nw) -> 9166
            | all wall [c, e, s, w, n, se, sw, nw] && not (wall ne) -> 5750
            | all wall [c, e, s, w, n, sw, nw, ne] && not (wall se) -> 1212
            | otherwise -> ignore
      | grassWall c ->
        case nearbyTileIds of
          (e, s, w, n, se, sw, nw, ne)
            | all wall [e, s, w, n, se, sw, nw, ne] -> 2851
            | all wall [c, e, w, n] && none wall [s] -> 4461
            | all wall [c, w, n] && none wall [e, s] -> 8920
            | all wall [c, s, w, n] && none wall [e] -> 5255
            | all wall [c, s, w] && none wall [e, n] -> 3054
            | all wall [c, e, s, w] && none wall [n] -> 5504
            | all wall [c, e, s] && none wall [n, w] -> 1491
            | all wall [c, e, s, n] && none wall [w] -> 1825
            | all wall [c, e, n] && none wall [s, w] -> 6055
            | all wall [c, e, s, w, n, se, nw, ne] && not (wall sw) -> 3302
            | all wall [c, e, s, w, n, se, sw, ne] && not (wall nw) -> 3597
            | all wall [c, e, s, w, n, se, sw, nw] && not (wall ne) -> 6745
            | all wall [c, e, s, w, n, sw, nw, ne] && not (wall se) -> 3394
            | otherwise -> ignore
      | snowWall c ->
        case nearbyTileIds of
          (e, s, w, n, se, sw, nw, ne)
            | all wall [e, s, w, n, se, sw, nw, ne] -> 5203
            | all wall [c, e, w, n] && none wall [s] -> 1455
            | all wall [c, w, n] && none wall [e, s] -> 6668
            | all wall [c, s, w, n] && none wall [e] -> 6722
            | all wall [c, s, w] && none wall [e, n] -> 0245
            | all wall [c, e, s, w] && none wall [n] -> 2645
            | all wall [c, e, s] && none wall [n, w] -> 0059
            | all wall [c, e, s, n] && none wall [w] -> 8854
            | all wall [c, e, n] && none wall [s, w] -> 0941
            | all wall [c, e, s, w, n, se, nw, ne] && not (wall sw) -> 4238
            | all wall [c, e, s, w, n, se, sw, ne] && not (wall nw) -> 1331
            | all wall [c, e, s, w, n, se, sw, nw] && not (wall ne) -> 7167
            | all wall [c, e, s, w, n, sw, nw, ne] && not (wall se) -> 7043
            | otherwise -> ignore
      | darkGrass c ->
        case nearbyTileIds of
          (e, s, w, n, se, sw, nw, ne)
            | all darkg [e, s, w, n, se, sw, nw, ne] -> 3404
            | all darkg [c, e, w, n] && none darkg [s] -> 1783
            | all darkg [c, w, n] && none darkg [e, s] -> 8052
            | all darkg [c, s, w, n] && none darkg [e] -> 6875
            | all darkg [c, s, w] && none darkg [e, n] -> 2628
            | all darkg [c, e, s, w] && none darkg [n] -> 1435
            | all darkg [c, e, s] && none darkg [n, w] -> 3002
            | all darkg [c, e, s, n] && none darkg [w] -> 7912
            | all darkg [c, e, n] && none darkg [s, w] -> 3602
            | all darkg [c, e, s, w, n, se, nw, ne] && not (darkg sw) -> 7088
            | all darkg [c, e, s, w, n, se, sw, ne] && not (darkg nw) -> 3632
            | all darkg [c, e, s, w, n, se, sw, nw] && not (darkg ne) -> 7401
            | all darkg [c, e, s, w, n, sw, nw, ne] && not (darkg se) -> 8417
            | otherwise -> ignore
      | openWater c ->
        case nearbyTileIds of
          (e, s, w, n, _, _, _, _)
            | all water [e, s, w, n] -> 2937
            | all water [s, w, n] && caveFloor e -> 0295
            | all water [e, w, n] && caveFloor s -> 6760
            | all water [e, s, n] && caveFloor w -> 2443
            | all water [e, s, w] && caveFloor n -> 6996
            | all water [w, n] && all caveFloor [e, s] -> 9878
            | all water [e, n] && all caveFloor [s, w] -> 4701
            | all water [e, s] && all caveFloor [n, w] -> 6921
            | all water [s, w] && all caveFloor [n, e] -> 3235
            | all water [s, n] && all caveFloor [e, w] -> 1701
            | all water [e, w] && all caveFloor [n, s] -> 5376
            | all water [s, w, n] && e `elem` [9022, 9090, 9166] -> 5153
            | all water [e, s, n] && w `elem` [9090, 2636, 5750] -> 5183
            | all water [s, w, n] && e `elem` [8648, 7655, 2680] -> 5641
            | all water [e, s, n] && w `elem` [8648, 5652, 1212] -> 8290
            | all water [e, w, n] && s `elem` [7655, 7069, 5750] -> 3530
            | all water [e, s, w] && n `elem` [7069, 9022, 1212] -> 4921
            | all water [e, w, n] && s `elem` [8111, 5652, 9166] -> 3361
            | all water [e, s, w] && n `elem` [2636, 8111, 2680] -> 2212
            | all water [s, w, n] && caveWall e -> 2494
            | all water [e, w, n] && caveWall s -> 2431
            | all water [e, s, n] && caveWall w -> 3058
            | all water [e, s, w] && caveWall n -> 0367
            | all water [w, n] && all caveWall [e, s] -> 2864
            | all water [e, n] && all caveWall [s, w] -> 4648
            | all water [e, s] && all caveWall [n, w] -> 9755
            | all water [s, w] && all caveWall [n, e] -> 8118
            | all water [s, w, n] && snowFloor e -> 2776
            | all water [e, w, n] && snowFloor s -> 2885
            | all water [e, s, n] && snowFloor w -> 2206
            | all water [e, s, w] && snowFloor n -> 8474
            | all water [w, n] && all snowFloor [e, s] -> 6450
            | all water [e, n] && all snowFloor [s, w] -> 4061
            | all water [e, s] && all snowFloor [n, w] -> 3848
            | all water [s, w] && all snowFloor [n, e] -> 0073
            | otherwise -> ignore
      | waterVBridge c ->
        case nearbyTileIds of
          (_, s, _, n, _, _, _, _)
            | water s && water n -> 7629
            | caveFloor s && water n -> 7108
            | water s && caveFloor n -> 7264
            | caveFloor s && caveFloor n -> 7739
            | otherwise -> ignore
      | waterHBridge c ->
        case nearbyTileIds of
          (e, _, w, _, _, _, _, _)
            | water e && water w -> 1917
            | caveFloor e && water w -> 5497
            | water e && caveFloor w -> 6446
            | caveFloor e && caveFloor w -> 8790
            | otherwise -> ignore
      | openOcean c ->
        case nearbyTileIds of
          (e, s, w, n, se, sw, nw, ne)
            | all ocean [e, s, w, n, se, sw, nw, ne] -> 4181
            | all ocean [e, w, n] && lightg s -> 7279
            | all ocean [w, n] && all lightg [e, s] -> 1729
            | all ocean [s, w, n] && lightg e -> 3908
            | all ocean [s, w] && all lightg [n, e] -> 1479
            | all ocean [e, s, w] && lightg n -> 6744
            | all ocean [e, s] && all lightg [n, w] -> 8336
            | all ocean [e, s, n] && lightg w -> 4855
            | all ocean [e, n] && all lightg [s, w] -> 9373
            | all ocean [e, s, w, n, se, nw, ne] && lightg sw -> 5854
            | all ocean [e, s, w, n, se, sw, ne] && lightg nw -> 1359
            | all ocean [e, s, w, n, se, sw, nw] && lightg ne -> 5285
            | all ocean [e, s, w, n, sw, nw, ne] && lightg se -> 6087
            | all ocean [e, w, n] && darkg s -> 7918
            | all ocean [w, n] && all darkg [e, s] -> 2692
            | all ocean [s, w, n] && darkg e -> 9192
            | all ocean [s, w] && all darkg [n, e] -> 4633
            | all ocean [e, s, w] && darkg n -> 0107
            | all ocean [e, s] && all darkg [n, w] -> 5792
            | all ocean [e, s, n] && darkg w -> 6899
            | all ocean [e, n] && all darkg [s, w] -> 9623
            | all ocean [e, s, w, n, se, nw, ne] && darkg sw -> 0255
            | all ocean [e, s, w, n, se, sw, ne] && darkg nw -> 0773
            | all ocean [e, s, w, n, se, sw, nw] && darkg ne -> 8557
            | all ocean [e, s, w, n, sw, nw, ne] && darkg se -> 2909
            | otherwise -> ignore
      | oceanVBridge c ->
        case nearbyTileIds of
          (_, s, _, n, _, _, _, _)
            | ocean s && ocean n -> 0153
            | lightg s && ocean n -> 7584
            | ocean s && lightg n -> 3226
            | darkg s && ocean n -> 4591
            | ocean s && darkg n -> 4132
            | otherwise -> ignore
      | oceanHBridge c ->
        case nearbyTileIds of
          (e, _, w, _, _, _, _, _)
            | ocean e && ocean w -> 7779
            | lightg e && ocean w -> 0387
            | ocean e && lightg w -> 3320
            | darkg e && ocean w -> 5134
            | ocean e && darkg w -> 7987
            | otherwise -> ignore
      | otherwise -> ignore
  where
    wall tid = caveWall tid || buildingWall tid || grassWall tid ||
               snowWall tid
    caveWall = (`elem` [1422, 8648, 7655, 7069, 9022, 9090, 2636, 8111, 5652,
                        2680, 9166, 5750, 1212, 0000])
    buildingWall = (`elem` [7292, 3112, 5588, 0983, 2330, 5719, 3254, 6250,
                            0111, 7883, 9398, 3037, 7791, 1306, 6933, 6383,
                            0865, 7148, 9011, 6051, 6455, 0170, 1752, 5489,
                            3891, 2993, 8625, 0605, 0364, 7185, 1814, 3403,
                            5216])
    grassWall = (`elem` [2851, 4461, 8920, 5255, 3054, 5504, 1491, 1825, 6055,
                         3302, 3597, 6745, 3394])
    snowWall = (`elem` [5203, 1455, 6668, 6722, 0245, 2645, 0059, 8854, 0941,
                        4238, 1331, 7167, 7043])
    darkGrass = (`elem` [3404, 1783, 8052, 6875, 2628, 1435, 3002, 7912, 3602,
                         7088, 3632, 7401, 8417])
    darkg tid = darkGrass tid || grassWall tid ||
                tid `elem` [2246, 1953, 8040, 2201, 2297, 2868, 0442, 2768,
                            4029, 2427, 0198, 3181, 7002, 8274, 1673, 6343,
                            4307, 9527, 6531, 5342, 9524, 8978, 4215, 8515,
                            1010, 0754, 6984, 9122, 4007, 0541, 8044, 4977]
    lightg = (`elem` [8983, 2583, 2938, 8301, 8740, 0678, 8415, 1397, 5398,
                      7100, 5308, 7966, 0723, 7888, 9930, 8596, 8799])
    caveFloor = (`elem` [1171, 6498, 8959, 4581, 9760, 1376, 0772, 0179, 6341,
                         5892, 6109, 6914, 7234, 5653, 5073, 6814, 3086, 6852,
                         0545, 5306, 4196, 3431, 4408, 3899, 0486, 2317, 9224,
                         3915, 8591, 6079, 9108, 3895, 8300, 5199, 3187, 3525])
    snowFloor = (`elem` [5709, 3930, 7591, 6995, 2571, 1332, 7122, 0781, 3384,
                         3236, 2011, 8721, 5390, 9409, 9456, 1287, 4556, 8284,
                         0563, 5643])
    water tid = openWater tid || waterVBridge tid || waterHBridge tid ||
                tid `elem` [5658, 4863]
    openWater = (`elem` [2937, 5658, 4863, 0295, 6760, 2443, 6996, 9878, 4701,
                         6921, 3235, 1701, 5376, 2494, 2431, 3058, 0367, 2864,
                         4648, 9755, 8118, 5153, 5183, 5641, 8290, 3530, 4921,
                         3361, 2212, 2776, 2885, 2206, 8474, 6450, 4061, 3848,
                         0073])
    waterVBridge = (`elem` [7629, 7108, 7264, 7739])
    waterHBridge = (`elem` [1917, 5497, 6446, 8790])
    ocean tid = openOcean tid || oceanVBridge tid || oceanHBridge tid ||
                tid `elem` [9702, 2467]
    openOcean = (`elem` [4181, 7279, 1729, 3908, 1479, 6744, 8336, 4855, 9373,
                         5854, 1359, 5285, 6087, 4632, 7733, 8007, 1868, 0176,
                         4465, 1750, 1919, 7918, 2692, 9192, 4633, 0107, 5792,
                         6899, 9623, 0255, 0773, 8557, 2909])
    oceanVBridge = (`elem` [0153, 3226, 7584, 4132, 4591])
    oceanHBridge = (`elem` [7779, 3320, 0387, 7987, 5134])

    tile = tmapGet tmap pos
    get tid = fromMaybe (error $ "no such tile: " ++ show tid) $
              tilesetLookup tid tileset
    nearbyTileIds =
      let dir = ttId . tmapGet tmap . (pos `plusDir`)
      in (dir DirE, dir DirS, dir DirW, dir DirN,
          dir DirSE, dir DirSW, dir DirNW, dir DirNE)
    none = (not .) . any
    ignore = ttId tile

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
      cardinalDirections = filter isCardinal allDirections
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
