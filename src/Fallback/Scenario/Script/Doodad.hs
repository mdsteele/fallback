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

module Fallback.Scenario.Script.Doodad
  (addBallisticDoodad, addBeamDoodad, addBlasterDoodad,
   addBoomDoodadAtPoint, addBoomDoodadAtPosition,
   addDeathDoodad, addLightningDoodad, addLightWallDoodad,
   addNumberDoodadAtPoint, addNumberDoodadAtPosition,
   addWordDoodadAtPoint, addWordDoodadAtPosition, addWordDoodadOnTarget,
   addShockwaveDoodad, doExplosionDoodad,
   addExtendingHookshotDoodad, addExtendedHookshotDoodad,
   addRetractingHookshotDoodad)
where

import Control.Applicative ((<$>))
import Control.Monad (foldM_, forM, replicateM)
import Data.Array (listArray)

import Fallback.Constants (framesPerSecond)
import Fallback.Control.Script
import Fallback.Data.Color (Tint(Tint, tintAlpha))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Scenario.Script.Base
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Terrain (positionCenter, prectRect)
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

-- | Return the number of frames the projectile will spend travelling.
addBallisticDoodad :: (FromAreaEffect f) => ProjTag -> Position -> Position
                   -> Double -> Script f Int
addBallisticDoodad ptag start end speed = do
  sprite <- flip rsrcProj ptag <$> areaGet arsResources
  let spt = positionCenter start
      ept = positionCenter end
  let gravity = 80 -- arbitrary value that gives reasonably nice-looking motion
  --let time = sqrt (height / gravity)
  let time = (spt `pDist` ept) / speed  -- seconds
  let height = gravity * time * time
  let limit = floor (time * fromIntegral framesPerSecond)  -- frames
  let paint count topleft = do
        let t = 1.0 - fromIntegral count / fromIntegral limit
        let x = pointX spt + t * (pointX ept - pointX spt)
        let dx = pointX ept - pointX spt
        let y = pointY spt + t * (pointY ept - pointY spt) -
                4 * (t - t * t) * height -- remember, positive y is down
        let dy = (pointY ept - pointY spt) - 4 * (1 - 2 * t) * height
        blitRotate sprite (Point x y `pSub` fmap fromIntegral topleft)
                   (atan2 dy dx)
  addDoodad MidDood limit paint
  return limit

addBeamDoodad :: (FromAreaEffect f) => Tint -> DPoint -> DPoint -> Int
              -> Script f ()
addBeamDoodad tint startPt endPt limit = do
  let semiWidth = 8
  let perp = pPolar semiWidth (pAtan2 (endPt `pSub` startPt) + pi / 2)
  let tint' = tint { tintAlpha = 0 }
  let paint count cameraTopleft = do
        let start = startPt `pSub` fmap fromIntegral cameraTopleft
            end = endPt `pSub` fmap fromIntegral cameraTopleft
        let f = 2 * fromIntegral (limit - count) / fromIntegral limit
        let (v1, v2) = if f <= 1.0 then (pZero, perp `pMul` f)
                       else (perp `pMul` (f - 1.0), perp)
        gradientPolygon [(tint, start `pAdd` v1), (tint, end `pAdd` v1),
                         (tint', end `pAdd` v2), (tint', start `pAdd` v2)]
        gradientPolygon [(tint, start `pSub` v1), (tint, end `pSub` v1),
                         (tint', end `pSub` v2), (tint', start `pSub` v2)]
  addDoodad MidDood limit paint

addBlasterDoodad :: (FromAreaEffect f) => Tint -> Double -> Double -> Position
                 -> Position -> Double -> Script f Int
addBlasterDoodad tint width len start end speed = do
  let tint' = tint { tintAlpha = 0 }
  let spt = positionCenter start
      ept = positionCenter end
  let delta = ept `pSub` spt
  let perp = pPolar (width / 2) (pAtan2 delta + pi / 2)
  let distance = pDist spt ept
  let duration = (distance + len) / speed  -- seconds
  let limit = floor (duration * fromIntegral framesPerSecond)  -- frames
  let frac = len / (distance + len)
  addContinuousDoodad MidDood limit $ \t topleft -> do
    let spt' = spt `pSub` fmap fromIntegral topleft
    let t1 = min 1 (t / (1 - frac))
    let t2 = max 0 ((t - frac) / (1 - frac))
    let p1 = spt' `pAdd` delta `pMul` t1
    let p2 = spt' `pAdd` delta `pMul` t2
    gradientPolygon [(tint, p1 `pAdd` perp), (tint', p2 `pSub` perp),
                     (tint, p1 `pSub` perp), (tint', p2 `pAdd` perp)]
  return $ floor (fromIntegral framesPerSecond * distance / speed)

addBoomDoodadAtPoint :: (FromAreaEffect f) => StripTag -> Int -> IPoint
                        -> Script f ()
addBoomDoodadAtPoint tag slowdown center = do
  strip <- flip rsrcStrip tag <$> areaGet arsResources
  let limit = stripLength strip * slowdown
  let paint count cameraTopleft = do
        blitLoc (strip ! (7 - count `div` slowdown))
                (LocCenter $ center `pSub` cameraTopleft)
  addDoodad MidDood limit paint

addBoomDoodadAtPosition :: (FromAreaEffect f) => StripTag -> Int -> Position
                        -> Script f ()
addBoomDoodadAtPosition tag slowdown pos =
  addBoomDoodadAtPoint tag slowdown (positionCenter pos)

addDeathDoodad :: (FromAreaEffect f) => CreatureImages -> FaceDir -> PRect
               -> Script f ()
addDeathDoodad images faceDir prect = addDoodad MidDood limit paint where
  paint count topleft = do
    let tint = let gba = fromIntegral (255 * count `div` limit)
               in Tint 255 gba gba gba
    let rect = adjustRect1 (2 * (count - limit)) initRect
    blitStretchTinted tint sprite (rect `rectMinus` topleft)
  limit = 15
  initRect = prectRect prect
  sprite = ciStand faceDir images

addLightningDoodad :: (FromAreaEffect f) => Tint -> Position -> Position
                   -> Script f ()
addLightningDoodad tint startPos endPos = do
  let limit = 18
  let startPt = positionCenter startPos
  let endPt = positionCenter endPos
  let dist = pDist startPt endPt
  let numSteps = floor (dist / 20) :: Int
  let step = (endPt `pSub` startPt) `pDiv` fromIntegral numSteps
  lists <- fmap (listArray (0, limit)) $ replicateM (limit + 1) $ do
    forM [0 .. numSteps] $ \idx -> do
      let center = startPt `pAdd` step `pMul` fromIntegral idx
      dx <- getRandomR (-10) 10
      dy <- getRandomR (-10) 10
      return $ pAdd center $ Point dx dy
  let paint count topleft =
        drawThickLineChain 3 tint $
        map (`pSub` fmap fromIntegral topleft) (lists ! count)
  addDoodad MidDood limit paint

addLightWallDoodad :: (FromAreaEffect f) => Bool -> Tint -> Int
                   -> Double -> DPoint -> DPoint -> Script f ()
addLightWallDoodad foreground tint duration maxHeight startPt endPt = do
  let tint' = tint { tintAlpha = 0 }
  let doodHeight = if foreground then HighDood else MidDood
  addContinuousDoodad doodHeight duration $ \t topleft -> do
    let height = maxHeight * 4 * (t - t * t)
    let offset = fmap fromIntegral topleft
    let p1 = startPt `pSub` offset
        p2 = endPt `pSub` offset
    let p3 = p2 `pSub` Point 0 height
        p4 = p1 `pSub` Point 0 height
    gradientPolygon [(tint, p1), (tint, p2), (tint', p3), (tint', p4)]

-- | Add a number doodad, centered on a given point (map pixel).
addNumberDoodadAtPoint :: (FromAreaEffect f) => Int -> IPoint -> Script f ()
addNumberDoodadAtPoint number (Point x y) = do
  digits <- areaGet (rsrcDigitsStripBig . arsResources)
  let limit = 30
  let paint count cameraTopleft =
        paintNumber digits number $ LocCenter $
        Point x (y - (limit - count)) `pSub` cameraTopleft
  addDoodad HighDood limit paint

-- | Add a number doodad, centered on a given position (map coordinates).
addNumberDoodadAtPosition :: (FromAreaEffect f) => Int -> Position
                          -> Script f ()
addNumberDoodadAtPosition number pos =
  addNumberDoodadAtPoint number (positionCenter pos)

addWordDoodadOnTarget :: (FromAreaEffect f) => WordTag -> HitTarget
                      -> Script f ()
addWordDoodadOnTarget tag hitTarget = do
  case hitTarget of
    HitCharacter charNum -> do
      addWordDoodadAtPosition tag =<< areaGet (arsCharacterPosition charNum)
    HitMonster monstKey -> do
      withMonsterEntry monstKey $ \entry -> do
        addWordDoodadAtPoint tag $ rectCenter $ prectRect $ Grid.geRect entry
    HitPosition pos -> addWordDoodadAtPosition tag pos

addWordDoodadAtPoint :: (FromAreaEffect f) => WordTag -> IPoint -> Script f ()
addWordDoodadAtPoint tag (Point x y) = do
  sprite <- areaGet (flip rsrcWordSprite tag . arsResources)
  let limit = 30
  let paint count cameraTopleft =
        blitLoc sprite $ LocCenter $
        Point x (y - (limit - count) - 8) `pSub` cameraTopleft
  addDoodad HighDood limit paint

addWordDoodadAtPosition :: (FromAreaEffect f) => WordTag -> Position
                        -> Script f ()
addWordDoodadAtPosition tag pos =
  addWordDoodadAtPoint tag (positionCenter pos)

addShockwaveDoodad :: (FromAreaEffect f) => Int -> DPoint
                   -> (Double -> (Tint, Double, Double, Double)) -> Script f ()
addShockwaveDoodad limit center fn = do
  let paint count cameraTopleft = do
        let (tint, thickness, hRad, vRad) =
              fn (fromIntegral (limit - count) / fromIntegral limit)
        tintRing tint thickness (center `pSub` fmap fromIntegral cameraTopleft)
                 hRad vRad
  addDoodad MidDood limit paint

doExplosionDoodad :: (FromAreaEffect f) => StripTag -> DPoint -> Script f ()
doExplosionDoodad tag (Point cx cy) = do
  startTheta <- getRandomR 0 (2 * pi)
  flip3 foldM_ startTheta (replicate 16 ()) $ \oldTheta () -> do
    theta <- getRandomR (oldTheta + 0.5 * pi) (oldTheta + 1.5 * pi)
    rho <- getRandomR 0 1
    let pt = Point (cx + 35 * rho * cos theta) (cy + 45 * rho * sin theta)
    addBoomDoodadAtPoint tag 4 (round <$> pt)
    wait 1
    return theta

-------------------------------------------------------------------------------

addExtendingHookshotDoodad :: (FromAreaEffect f) => Position -> Position
                           -> Script f Int
addExtendingHookshotDoodad startPos endPos = do
  let limit = round (3 * pDist (fromIntegral <$> startPos)
                               (fromIntegral <$> endPos) :: Double)
  let paint factor cameraTopleft = do
        let startPt = positionCenter startPos `pSub` cameraTopleft
        let endPt = positionCenter endPos `pSub` cameraTopleft
        let delta = fromIntegral <$> (endPt `pSub` startPt)
        let midPt = round <$> ((fromIntegral <$> startPt) `pAdd`
                               delta `pMul` factor)
        drawLine (Tint 128 64 0 255) startPt midPt
        let Point x y = midPt
        tintRect (Tint 128 128 128 255) (Rect (x - 2) (y - 2) 5 5)
  addContinuousDoodad MidDood limit paint
  return limit

addExtendedHookshotDoodad :: (FromAreaEffect f) => Int -> Position -> Position
                          -> Script f ()
addExtendedHookshotDoodad limit startPos endPos = do
  let paint _ cameraTopleft = do
        let startPt = positionCenter startPos `pSub` cameraTopleft
        let endPt = positionCenter endPos `pSub` cameraTopleft
        drawLine (Tint 128 64 0 255) startPt endPt
  addContinuousDoodad MidDood limit paint

addRetractingHookshotDoodad :: (FromAreaEffect f) => Position -> Position
                            -> Script f ()
addRetractingHookshotDoodad startPos endPos = do
  let limit = round (2 * pDist (fromIntegral <$> startPos)
                               (fromIntegral <$> endPos) :: Double)
  let paint factor cameraTopleft = do
        let startPt = fromIntegral <$> (positionCenter startPos `pSub`
                                        cameraTopleft)
        let endPt = fromIntegral <$> (positionCenter endPos `pSub`
                                      cameraTopleft)
        let delta = startPt `pSub` endPt
        let midPt = endPt `pAdd` delta `pMul` factor
        let dist = pDist startPt midPt
        let theta = pAtan2 (midPt `pSub` startPt)
        let perp = pPolar 1 (theta + pi / 2)
        let amplFn r = 0.25 * dist * factor * sin (r * 2 * pi)
        let pointFn r = startPt `pAdd` pPolar (r * dist) theta `pAdd`
                        perp `pMul` amplFn r
        let alpha = floor (255 * (1 - factor))
        drawLineChain (Tint 128 64 0 alpha) $ map pointFn [0, 0.01 .. 1]
  addContinuousDoodad MidDood limit paint

-------------------------------------------------------------------------------

addDoodad :: (FromAreaEffect f) => DoodadHeight -> Int
          -> (Int -> IPoint -> Paint ()) -> Script f ()
addDoodad height limit paint = emitAreaEffect $ EffAddDoodad $ Doodad
  { doodadCountdown = limit, doodadHeight = height, doodadPaint = paint }

addContinuousDoodad :: (FromAreaEffect f) => DoodadHeight -> Int
                    -> (Double -> IPoint -> Paint ()) -> Script f ()
addContinuousDoodad height limit paintFn = do
  let paintFn' n topleft =
        paintFn (fromIntegral (limit - n) / fromIntegral limit) topleft
  addDoodad height limit paintFn'

-------------------------------------------------------------------------------
