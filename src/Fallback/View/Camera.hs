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

{-# LANGUAGE GADTs #-}

module Fallback.View.Camera
  (-- * Painting terrain
   paintTerrain, paintTerrainFullyExplored, tintNonVisibleTiles,
   -- * Painting fields and creatures
   paintRemains, paintFields,
   paintMonsters, paintStatusDecorations, paintHealthBar,
   -- * Painting GUI elements
   paintMessage, paintTargeting, paintWeaponRange, paintAreaExits,
   -- * Utility views
   newFadeToBlackView)
where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (forM_, guard, when)
import Data.Ix (range)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Fallback.Constants (tileHeight, tileWidth)
import Fallback.Data.Clock (Clock, clockMod, clockZigzag)
import Fallback.Data.Color (Tint(..), blackTint, whiteColor)
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import qualified Fallback.Data.SparseMap as SM
import Fallback.Draw
import Fallback.State.Action (Targeting(..))
import Fallback.State.Area
import Fallback.State.Camera (camTopleft)
import Fallback.State.Creature
import Fallback.State.Doodad (Message(..))
import Fallback.State.Party
import Fallback.State.Resources
import Fallback.State.Simple
  (AttackRange, CharacterNumber, Field(..), MentalEffect(..), rangeSqDist)
import Fallback.State.Status
import Fallback.State.Terrain
import Fallback.State.Tileset
import Fallback.Utility (maybeM)
import Fallback.View.Base (View, inertView)

-------------------------------------------------------------------------------

paintTerrain :: AreaCommonState -> Paint ()
paintTerrain acs = paintTiles (acsResources acs) (camTopleft $ acsCamera acs)
                              getTile (acsClock acs)
  where
    getTile pos = if exmap `hasExplored` pos
                  then Just (terrainGetTile pos terrain) else Nothing
    exmap = partyExploredMap (acsTerrain acs) (acsParty acs)
    terrain = acsTerrain acs

paintTerrainFullyExplored :: Resources -> IPoint -> TerrainMap -> Clock
                          -> Paint ()
paintTerrainFullyExplored resources cameraTopleft tmap clock =
  paintTiles resources cameraTopleft (Just . tmapGet tmap) clock

tintNonVisibleTiles :: AreaCommonState -> Paint ()
tintNonVisibleTiles acs = do
  let cameraTopleft = camTopleft $ acsCamera acs
  let exmap = partyExploredMap (acsTerrain acs) (acsParty acs)
  let offTile = terrainOffTile $ acsTerrain acs
  let getTile pos = if exmap `hasExplored` pos then Nothing else Just offTile
  paintTiles (acsResources acs) cameraTopleft getTile (acsClock acs)
  let visible = acsVisible acs
  let paintCell pos rect =
        when (exmap `hasExplored` pos && Set.notMember pos visible) $
        tintRect (Tint 0 0 0 128) rect
  paintCells paintCell cameraTopleft

paintTiles :: Resources -> IPoint -> (Position -> Maybe TerrainTile) -> Clock
           -> Paint ()
paintTiles resources cameraTopleft getTile clock = do
  let paintTile pos rect = maybeM (getTile pos) $ \tile -> do
        let (row, col) =
              case ttAppearance tile of
                Still r c -> (r, c)
                Anim r c0 slowdown _ -> (r, c0 + clockMod 4 slowdown clock)
        blitStretch (rsrcTerrainSprite resources (row, col)) rect
  paintCells paintTile cameraTopleft
  let paintOverlay pos rect = maybeM (getTile pos) $ \tile -> do
        case ttAppearance tile of
          Anim _ _ _ (Overlay row col) -> do
            blitStretch (rsrcTerrainOverlaySprite resources row col) rect
          _ -> return ()
  paintCells paintOverlay cameraTopleft

paintCells :: (Position -> IRect -> Paint ()) -> IPoint -> Paint ()
paintCells paintFn cameraTopleft = do
  let paintTile pos = paintFn pos $ positionRect pos `rectMinus` cameraTopleft
  (width, height) <- canvasSize
  let cameraBottomright = cameraTopleft `pAdd` Point width height
  mapM_ paintTile $ range (pointPosition cameraTopleft,
                           pointPosition cameraBottomright)

-------------------------------------------------------------------------------

paintRemains :: AreaCommonState -> Paint ()
paintRemains acs = do
  let cameraTopleft = camTopleft $ acsCamera acs
  let resources = acsResources acs
  let paintList (pos, remainsList) = do
        let rect = positionRect pos `rectMinus` cameraTopleft
        let paint remains = do
              blitStretch (rsrcRemainsSprite resources remains) rect
        mapM_ paint (reverse remainsList)
  mapM_ paintList (SM.sparseAssocs $ acsRemains acs)

paintFields :: Resources -> IPoint -> Set.Set Position -> Clock
           -> Map.Map Position Field -> Paint ()
paintFields resources cameraTopleft visible clock =
  mapM_ paintField . Map.assocs where
    paintField (pos, field) =
      if Set.notMember pos visible then return () else
        case field of
          BarrierWall _ -> blit SrpBarrierAura $ clockMod 4 6 clock
          FireWall _ -> blit SrpFireAura $ clockMod 4 3 clock
          IceWall _ -> blit SrpIceAura $ clockMod 4 5 clock
          PoisonCloud _ -> blit SrpGasAura $ clockMod 4 5 clock
          SmokeScreen _ -> blit SrpSmokeAura $ clockMod 4 8 clock
          Webbing _ -> blitStretch (rsrcSprite resources WebbingSprite) rect
      where
        blit tag idx = blitStretch (rsrcStrip resources tag ! idx) rect
        rect = positionRect pos `rectMinus` cameraTopleft

-- | Paint monsters, given the topleft position of the camera, a set of visible
-- tile locations, a list of party member positions (used to determine if the
-- party is adjacent to any invisible monsters), and finally the list of
-- monsters to draw.
paintMonsters :: AreaCommonState -> Bool -> Paint ()
paintMonsters acs inCombat = mapM_ paintMonster monsters where
  paintMonster entry = do
    let rect = Grid.geRect entry
    let monst = Grid.geValue entry
    let pose = monstPose monst
    -- If the monster is not on a visible tile, and it's not curently walking
    -- from a visible tile, don't draw it.
    if all (flip Set.notMember $ acsVisible acs) $ prectPositions rect ++
       case cpAnim pose of
         JumpAnim _ _ from -> prectPositions $ makeRect from $ rectSize rect
         WalkAnim _ _ from -> prectPositions $ makeRect from $ rectSize rect
         _ -> []
     then return () else do
    -- If the monster is completely invisible, then don't draw it.
    if cpAlpha pose == 0 then return () else do
    -- Draw the monster, taking its currenct animation (if any) into account.
    let mtype = monstType monst
    let sprite = (case cpAnim pose of
                    { AttackAnim _ -> ciAttack; _ -> ciStand})
                 (cpFaceDir pose)
                 (rsrcMonsterImages resources mtype)
    let offset = animOffset (cpAnim pose) (rectTopleft rect)
    blitStretchTinted (Tint 255 255 255 (cpAlpha pose)) sprite
                      (prectRect rect `rectPlus` (offset `pSub` cameraTopleft))
    paintStatusDecorations resources cameraTopleft (acsClock acs) offset rect
                           (monstHeadPos entry) (monstStatus monst)
    when inCombat $ do
      paintHealthBar cameraTopleft (monstIsAlly monst) rect offset
                     (monstHealth monst) (mtMaxHealth mtype)
                     ((msRemainingFrames &&& msMaxFrames) <$>
                      monstSummoning monst)
  cameraTopleft = camTopleft $ acsCamera acs
  monsters = Grid.entries $ acsMonsters acs
  resources = acsResources acs

paintStatusDecorations :: Resources -> IPoint -> Clock -> IPoint -> PRect
                       -> Position -> StatusEffects -> Paint ()
paintStatusDecorations resources cameraTopleft clock offset
                       prect headPos status = do
  let decor = rsrcStatusDecorations resources
  let rect = fmap fromIntegral $
             prectRect prect `rectPlus` (offset `pSub` cameraTopleft)
  let halfW = rectW rect / 2
      halfH = rectH rect / 2
  let centerPlus x y = rectCenter rect `pAdd` Point x y
  let spinTheta = fromIntegral (clockMod 60 1 clock) * (pi / 30)
  when (seIsEntangled status) $ do
      let offsetX = fromIntegral ((5 + clockZigzag 4 5 clock) * rectW prect)
      let paint signx = blitLoc (sdEntangledSprite decor) $ LocCenter $
                        centerPlus (signx * offsetX) (halfH - 5)
      paint 1
      paint (negate 1)
  case seHaste status of
    Harmful _ -> do
      let offsetX = fromIntegral ((4 + clockZigzag 4 5 clock) * rectW prect)
      let paint signx theta =
            blitRotate (sdSlowSprite decor)
            (centerPlus (signx * (halfW - offsetX)) 0) theta
      paint 1 pi
      paint (negate 1) 0
    Unaffected -> return ()
    Beneficial _ -> do
      forM_ [0 .. 15] $ \index -> do
        let radius = (index + clockMod 16 2 clock) `mod` 16
        when (radius < 8) $ do
        let theta = fromIntegral ((7 * index) `mod` 16) * (pi / 8)
        let r1 = fromIntegral (radius * 2)
            r2 = r1 + 4
        let fx = fromIntegral (rectW prect)
            fy = fromIntegral (rectH prect)
        drawLine (Tint 0 255 0 192)
                 (centerPlus (fx * r1 * cos theta) (fy * r1 * sin theta))
                 (centerPlus (fx * r2 * cos theta) (fy * r2 * sin theta))
  case seBlessing status of
    Harmful _ -> do
      let index = clockMod 4 4 clock
      forM_ (prectPositions prect) $ \pos -> do
        let topleft = positionTopleft pos `pAdd` offset `pSub` cameraTopleft
        let sparkle x y = blitTopleft (sdCurseStrip decor ! index)
                                      (topleft `pAdd` Point x y)
        sparkle 10  4
        sparkle 17 13
        sparkle  4 13
        sparkle 11 22
    Unaffected -> return ()
    Beneficial _ -> do
      let tau = clockMod 10 3 clock
      forM_ (prectPositions prect) $ \pos -> do
        let topleft = positionTopleft pos `pAdd` offset `pSub` cameraTopleft
        let sparkle x y index = when (0 <= index && index < 4) $ do
              blitTopleft (sdBlessingStrip decor ! index)
                          (topleft `pAdd` Point x y)
        sparkle  5  4 tau
        sparkle 17 10 (tau - 2)
        sparkle  4 16 (tau - 4)
        sparkle 16 22 (tau - 6)
  -- TODO decoration for poison
  case seDefense status of
    Harmful _ -> do
      let offset' = 3 + clockZigzag 4 5 clock
      let offsetX = fromIntegral (offset' * rectW prect)
          offsetY = fromIntegral (offset' * rectH prect)
      let paint signx signy theta =
            blitRotate (sdWeaknessSprite decor)
            (centerPlus (signx * (halfW - offsetX))
                        (signy * (halfH - offsetY))) theta
      paint 1 (negate 1) 0
      paint 1 1 (pi / 2)
      paint (negate 1) 1 pi
      paint (negate 1) (negate 1) (3 * pi / 2)
    Unaffected -> return ()
    Beneficial _ -> do
      let paint th =
            blitRotateTinted (Tint 255 255 255 192) (sdDefenseSprite decor)
              (centerPlus ((halfW - 2) * cos th) ((halfH - 2) * sin th)) th
      mapM_ paint [spinTheta, spinTheta + pi/2, spinTheta + pi,
                   spinTheta - pi/2]
  when (seIsShielded status) $ do
    let paint th =
          blitLoc (sdMagicShieldSprite decor) $
          LocCenter $ centerPlus ((halfW - 2) * cos th) ((halfH - 2) * sin th)
    mapM_ paint [spinTheta + pi / 4, spinTheta + 3 * (pi / 4),
                 spinTheta + 5 * (pi / 4), spinTheta + 7 * (pi / 4)]
  maybeM (seMentalEffect status) $ \eff -> do
    let mentalTop = positionCenter headPos `pSub` Point 0 18 `pAdd`
                    offset `pSub` cameraTopleft
    case eff of
      Dazed -> do
        let idx = clockMod 4 8 clock
        when (idx < 3) $ do
          blitLoc (sdDazedStrip decor ! idx) (LocMidtop mentalTop)
      Confused -> do
        blitLoc (sdConfusedSprite decor) $ LocMidtop $
          mentalTop `pAdd` Point 0 (clockZigzag 3 10 clock)
      Charmed -> do
        blitLoc (sdCharmedSprite decor) $ LocMidtop $
          mentalTop `pAdd` Point 0 (clockZigzag 3 10 clock)

paintHealthBar :: IPoint -> Bool -> PRect -> IPoint -> Int -> Int
               -> Maybe (Int, Int) -> Paint ()
paintHealthBar cameraTopleft ally prect offset health maxHealth mbSummon = do
  let barWidth = 20
  let Rect x y w h = prectRect prect `rectMinus` (cameraTopleft `pSub` offset)
  let rect = Rect (x + half (w - barWidth)) (y + h - 4) barWidth 5
  tintRect (if ally then Tint 192 192 255 255 else Tint 255 192 192 255) rect
  tintRect (if ally then Tint 64 64 255 255 else Tint 255 64 64 255)
           (rect { rectW = barWidth * health `div` maxHealth })
  when ally $ maybeM mbSummon $ \(remainingFrames, maxFrames) -> do
    let y' = rectY rect + half (rectH rect)
        x' = rectX rect
        x'' = x' + barWidth * remainingFrames `div` maxFrames
    drawLine (Tint 0 255 0 255) (Point x' y') (Point x'' y')
  drawRect blackTint rect

-------------------------------------------------------------------------------

paintMessage :: Resources -> Message -> Paint ()
paintMessage resources (Message time string) = do
  let font = rsrcFont resources FontGeorgia14
  (width, height) <- canvasSize
  let { fallTime = 0.25; fallDist = 30 }
  let bottom = height - 10 + if time >= fallTime then 0
                             else round (fallDist * (1 - time / fallTime))
  let textW = textRenderWidth font string
  tintRect (Tint 0 0 0 220)
           (Rect (half (width - textW - 10)) (bottom - 18) (textW + 10) 20)
  drawText font whiteColor (LocMidbottom $ Point (half width) bottom) string

paintTargeting :: (AreaState a) => IPoint -> Maybe IPoint -> a
               -> CharacterNumber -> Targeting b -> Clock -> Paint ()
paintTargeting cameraTopleft mbMousePt ars charNum targeting clock = do
  rect <- canvasRect
  let whenMouse = maybeM $ do
        mousePt <- mbMousePt
        guard (rectContains rect mousePt)
        Just $ pointPosition $ cameraTopleft `pAdd` mousePt
  let paintMouseTarget positions = whenMouse $ \targetPos ->
        if targetPos `Set.member` positions
        then paintTile targetPos else paintX targetPos
  case targeting of
    TargetingAlly radius -> do
      let targetable = getRegionOfRadius radius
      paintRegion targetable
      paintMouseTarget targetable
    TargetingArea areaFn radius -> do
      let targetable = getRegionOfRadius radius
      paintRegion targetable
      whenMouse $ \targetPos -> do
        if targetPos `Set.notMember` targetable then paintX targetPos else do
        let targets = areaFn ars charNum targetPos
        if null targets then paintX targetPos else mapM_ paintTile targets
    TargetingJump areaFn targetable -> do
      paintRegion targetable
      whenMouse $ \targetPos -> do
        if targetPos `Set.notMember` targetable then paintX targetPos else do
        mapM_ paintTile $ areaFn ars charNum targetPos
        paintO targetPos
    TargetingMulti _ radius targets -> do
      let targetable = getRegionOfRadius radius
      paintRegion targetable
      mapM_ paintTile targets
      paintMouseTarget targetable
    TargetingSingle radius -> do
      let targetable = getRegionOfRadius radius
      paintRegion targetable
      paintMouseTarget targetable
  where
    getRegionOfRadius radius =
      let sqdist = ofRadius radius
      in Set.filter ((sqdist >=) . pSqDist originPos) visible
    originPos = arsCharacterPosition charNum ars
    paintRegion = paintTargetingRegion (Tint 255 0 128 128) cameraTopleft
    paintTile pos = do
      gradientRing (const (0.3 + 0.05 * fromIntegral (clockZigzag 8 2 clock),
                           Tint 255 0 128 0))
                   (\th -> (1 + cos (4 * th + pi) / 16, Tint 255 0 128 192))
                   (positionCenter pos `pSub` fmap fromIntegral cameraTopleft)
                   (fromIntegral tileWidth / 2) (fromIntegral tileHeight / 2)
    paintX pos = do
      gradientRing (const (0.03 * fromIntegral (clockZigzag 8 2 clock),
                           Tint 255 0 128 0))
                   (\th -> ((1 + cos (4 * th + pi) / 2) * 0.8,
                            Tint 255 0 0 192))
                   (positionCenter pos `pSub` fmap fromIntegral cameraTopleft)
                   (fromIntegral tileWidth / 2) (fromIntegral tileHeight / 2)
    paintO pos = do
      let rect = positionRect pos `rectMinus` cameraTopleft
      let pulse = 6 + clockZigzag 8 2 clock
      let x1 = rectX rect
          x1' = x1 + pulse
          x2 = x1 + half (rectW rect)
          x3 = x1 + rectW rect - 1
          x3' = x3 - pulse
          y1 = rectY rect
          y1' = y1 + pulse
          y2 = y1 + half (rectH rect)
          y3 = y1 + rectH rect - 1
          y3' = y3 - pulse
      let tint1 = Tint 0 0 255 192
      let tint2 = Tint 0 128 255 0
      gradientPolygon [(tint1, Point x1 y2), (tint1, Point x2 y1),
                       (tint2, Point x2 y1'), (tint2, Point x1' y2)]
      gradientPolygon [(tint1, Point x3 y2), (tint1, Point x2 y1),
                       (tint2, Point x2 y1'), (tint2, Point x3' y2)]
      gradientPolygon [(tint1, Point x1 y2), (tint1, Point x2 y3),
                       (tint2, Point x2 y3'), (tint2, Point x1' y2)]
      gradientPolygon [(tint1, Point x3 y2), (tint1, Point x2 y3),
                       (tint2, Point x2 y3'), (tint2, Point x3' y2)]
    visible = arsVisibleForCharacter charNum ars

paintWeaponRange :: (AreaState a) => IPoint -> a -> CharacterNumber
                 -> AttackRange -> Paint ()
paintWeaponRange cameraTopleft ars charNum attackRange =
  paintRegion $ getRegion (rangeSqDist attackRange) where
    getRegion sqdist = Set.filter ((sqdist >=) . pSqDist originPos) $
                       arsVisibleForCharacter charNum ars
    originPos = arsCharacterPosition charNum ars
    paintRegion = paintTargetingRegion (Tint 255 0 0 64) cameraTopleft

paintTargetingRegion :: Tint -> IPoint -> Set.Set Position -> Paint ()
paintTargetingRegion tint1 cameraTopleft positions = do
  let tint2 = tint1 { tintAlpha = 0 }
  let thickness = 8
  forM_ (Set.elems positions) $ \pos -> do
    let rect = positionRect pos `rectMinus` cameraTopleft
    let xL = rectX rect
        xR = xL + tileWidth
        yT = rectY rect
        yB = yT + tileHeight
    when ((pos `plusDir` DirE) `Set.notMember` positions) $ do
      let x' = xR - thickness
      gradientPolygon [(tint1, Point xR yT), (tint1, Point xR yB),
                       (tint2, Point x' yB), (tint2, Point x' yT)]
    when ((pos `plusDir` DirN) `Set.notMember` positions) $ do
      let y' = yT + thickness
      gradientPolygon [(tint1, Point xL yT), (tint1, Point xR yT),
                       (tint2, Point xR y'), (tint2, Point xL y')]
    when ((pos `plusDir` DirW) `Set.notMember` positions) $ do
      let x' = xL + thickness
      gradientPolygon [(tint1, Point xL yT), (tint1, Point xL yB),
                       (tint2, Point x' yB), (tint2, Point x' yT)]
    when ((pos `plusDir` DirS) `Set.notMember` positions) $ do
      let y' = yB - thickness
      gradientPolygon [(tint1, Point xL yB), (tint1, Point xR yB),
                       (tint2, Point xR y'), (tint2, Point xL y')]

paintAreaExits :: IPoint -> TerrainMap -> [AreaExit] -> Paint ()
paintAreaExits cameraTopleft tmap exits = mapM_ paintExit exits where
  paintExit = mapM_ paintExitRectKey . aeRectKeys
  paintExitRectKey rectKey = maybeM (tmapLookupRect rectKey tmap) paintExitRect
  paintExitRect prect = do
    tintRect (Tint 128 64 255 96) $ prectRect prect `rectMinus` cameraTopleft

-------------------------------------------------------------------------------

newFadeToBlackView :: (MonadDraw m) => m (View Double a)
newFadeToBlackView = do
  let
    paint opacity = do
      tintCanvas $ Tint 0 0 0 $ round $ (255 *) $ max 0 $ min 1 $ opacity
  return (inertView paint)

-------------------------------------------------------------------------------
