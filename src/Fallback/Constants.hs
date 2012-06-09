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

module Fallback.Constants where

import Fallback.Data.Point (Point(Point), Rect(Rect), SqDist, ofRadius)

-------------------------------------------------------------------------------
-- Real time:

-- | The number of animation frames the game draws each second.
framesPerSecond :: Int
framesPerSecond = 40

-- | The reciprocal of 'framesPerSecond'.
secondsPerFrame :: Double
secondsPerFrame = recip (fromIntegral framesPerSecond)

-------------------------------------------------------------------------------
-- Screen layout:

screenWidth, screenHeight :: Int
screenWidth = 640
screenHeight = 480

screenRect :: Rect Int
screenRect = Rect 0 0 screenWidth screenHeight

sidebarWidth :: Int
sidebarWidth = 124

cameraWidth, cameraHeight :: Int
cameraWidth = screenWidth - sidebarWidth
cameraHeight = screenHeight

cameraSize :: (Int, Int)
cameraSize = (cameraWidth, cameraHeight)

cameraCenterOffset :: Point Double
cameraCenterOffset =
  Point (fromIntegral cameraWidth / 2) (fromIntegral cameraHeight / 2)

-------------------------------------------------------------------------------
-- Terrain:

-- | The width of each terrain tile, in pixels.
tileWidth :: Int
tileWidth = 28

-- | The height of each terrain tile, in pixels.
tileHeight :: Int
tileHeight = 36

-------------------------------------------------------------------------------
-- Combat arena:

combatArenaCols :: Int
combatArenaCols = cameraWidth `div` tileWidth

combatArenaRows :: Int
combatArenaRows = cameraHeight `div` tileHeight

combatArenaSize :: (Int, Int)
combatArenaSize = (combatArenaCols, combatArenaRows)

combatCameraOffset :: Point Int
combatCameraOffset = Point ((cameraWidth  `mod` tileWidth)  `div` 2)
                           ((cameraHeight `mod` tileHeight) `div` 2)

-------------------------------------------------------------------------------
-- Combat time:

-- | The number of moments required for one action point, where a moment is the
-- smallest unit of a creature's time bar.
momentsPerActionPoint :: Int
momentsPerActionPoint = 100000

-- | The number of moments a creature with a speed of 1 will gain per frame
-- during the combat waiting phase.
baseMomentsPerFrame :: Int
baseMomentsPerFrame =
  round (fromIntegral momentsPerActionPoint * secondsPerFrame /
         baseSecondsPerActionPoint) where baseSecondsPerActionPoint = 1.0

-- | The number of frames per combat round, where a round is the unit of time
-- between period damage hits (e.g. poison), and also the length of time
-- required for a creature with a speed of 1 to gain one action point.  A town
-- step is consided roughly equivalent to one combat round.
framesPerRound :: Int
framesPerRound = round (fromIntegral momentsPerActionPoint /
                        fromIntegral baseMomentsPerFrame :: Double)

-- | The reciprocal of 'framesPerRound'.
roundsPerFrame :: Double
roundsPerFrame = recip (fromIntegral framesPerRound)

-- | The maximum number of action points that a creature can have at once.
maxActionPoints :: Int
maxActionPoints = 4

-------------------------------------------------------------------------------

maxAdrenaline :: Int
maxAdrenaline = 100

sightRange :: Int
sightRange = 10

sightRangeSquared :: SqDist
sightRangeSquared = ofRadius sightRange

talkRadius :: Int
talkRadius = 6

experiencePerLevel :: Int
experiencePerLevel = 1000

maxPartyLevel :: Int
maxPartyLevel = 30

maxExperience :: Int
maxExperience = experiencePerLevel * maxPartyLevel

-------------------------------------------------------------------------------
