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

module Fallback.State.Camera
  (Camera, makeCameraWithCenter, makeCameraWithTopleft,
   camTopleft, setCameraShake, tickCamera)
where

import Control.Applicative ((<$>))

import Fallback.Constants (cameraCenterOffset)
import Fallback.Data.Point

-------------------------------------------------------------------------------

data Camera = Camera
  { camShakeFactor :: Double,
    camShakeRemaining :: Int,
    camVirtualTopleft :: DPoint }

makeCameraWithCenter :: DPoint -> Camera
makeCameraWithCenter center =
  makeCameraWithTopleft (center `pSub` cameraCenterOffset)

makeCameraWithTopleft :: DPoint -> Camera
makeCameraWithTopleft topleft =
  Camera { camShakeFactor = 0,
           camShakeRemaining = 0,
           camVirtualTopleft = topleft }

camTopleft :: Camera -> IPoint
camTopleft camera =
  round <$> (camVirtualTopleft camera `pAdd` Point shakeDelta 0) where
    shakeDelta = if remain <= 0 then 0 else
                   (0.5 - fromIntegral (remain `mod` 2)) *
                   exp (fromIntegral remain * camShakeFactor camera)
    remain = camShakeRemaining camera

setCameraShake :: Double -> Int -> Camera -> Camera
setCameraShake amplitude duration camera =
  camera { camShakeFactor = log (2 * amplitude) / fromIntegral duration,
           camShakeRemaining = duration }

tickCamera :: DPoint -> Camera -> Camera
tickCamera goalTopleft camera =
  camera { camShakeRemaining = max 0 (camShakeRemaining camera - 1),
           camVirtualTopleft = (camVirtualTopleft camera `pMul` 3 `pAdd`
                                goalTopleft) `pDiv` 4 }

-------------------------------------------------------------------------------
