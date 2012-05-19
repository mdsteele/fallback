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

module Fallback.State.Doodad
  (Doodad(..), DoodadHeight(..),
   Doodads, emptyDoodads, tickDoodads, paintDoodads,
   appendDoodad, appendFloatingWord, appendFloatingNumber,
   Message(..), makeMessage, decayMessage)
where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Data.Ix (Ix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)

import Fallback.Constants (secondsPerFrame)
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM
import Fallback.Draw
import Fallback.State.Resources
  (Resources, WordTag, rsrcDigitsStripBig, rsrcWordSprite)
import Fallback.State.Terrain (prectRect)

-------------------------------------------------------------------------------

data Doodad = Doodad
  { doodadCountdown :: Int,
    doodadHeight :: DoodadHeight,
    doodadPaint :: Int -> IPoint -> Paint () }

data DoodadHeight = LowDood | MidDood | HighDood
  deriving (Bounded, Enum, Eq, Ix, Ord)

-- delayDoodad :: Int -> Doodad -> Doodad

-- slowDownDoodad :: Int -> Doodad -> Doodad

-- composeDoodads :: Doodad -> Doodad -> Doodad

-------------------------------------------------------------------------------

data Floater = Floater
  { flCount :: Int,
    flLimit :: Int,
    flPaint :: IPoint -> Paint () }

makeFloater :: (IPoint -> Paint ()) -> Floater
makeFloater fn = Floater { flCount = 0, flLimit = 30, flPaint = fn }

-------------------------------------------------------------------------------

data Doodads = Doodads
   { dsDoodads :: TM.TotalMap DoodadHeight [Doodad],
     dsFloaters :: Map.Map PRect [Floater] }

emptyDoodads :: Doodads
emptyDoodads = Doodads
  { dsDoodads = TM.make (const []),
    dsFloaters = Map.empty }

tickDoodads :: Doodads -> Doodads
tickDoodads ds = ds { dsDoodads = doodads', dsFloaters = floaters' } where
  doodads' = mapMaybe tickDoodad <$> dsDoodads ds
  tickDoodad doodad =
    let count' = doodadCountdown doodad - 1
    in if count' < 1 then Nothing else Just doodad { doodadCountdown = count' }
  floaters' = Map.mapMaybe updateFloaters $ dsFloaters ds
  updateFloaters floaters =
    case mapMaybe tickFloater floaters of { [] -> Nothing; fs -> Just fs }
  tickFloater fl =
    let count' = flCount fl + 1
    in if count' >= flLimit fl then Nothing else Just fl { flCount = count' }

paintDoodads :: IPoint -> DoodadHeight -> Doodads -> Paint ()
paintDoodads cameraTopleft dh ds = do
  let paintDoodad d = doodadPaint d (doodadCountdown d - 1) cameraTopleft
  mapM_ paintDoodad $ TM.get dh $ dsDoodads ds
  when (dh == HighDood) $ do
    forM_ (Map.assocs $ dsFloaters ds) $ \(prect, floaters) -> do
      let Point cx cy = rectCenter (prectRect prect) `pSub` cameraTopleft
      forM_ floaters $ \floater -> do
        flPaint floater $ Point cx (cy - flCount floater)

appendDoodad :: Doodad -> Doodads -> Doodads
appendDoodad doodad ds = ds { dsDoodads =
  TM.adjust (doodadHeight doodad) (++ [doodad]) (dsDoodads ds) }

appendFloatingWord :: Resources -> WordTag -> PRect -> Doodads -> Doodads
appendFloatingWord resources wordTag = appendFloater floater where
  floater = makeFloater (blitLoc sprite . LocCenter)
  sprite = rsrcWordSprite resources wordTag

appendFloatingNumber :: Resources -> Int -> PRect -> Doodads -> Doodads
appendFloatingNumber resources number = appendFloater floater where
  floater = makeFloater (paintNumber digits number . LocCenter)
  digits = rsrcDigitsStripBig resources

appendFloater :: Floater -> PRect -> Doodads -> Doodads
appendFloater floater prect ds = ds { dsFloaters = floaters' } where
  floaters' = Map.alter (Just . (floater :) . pushup step . fromMaybe [])
                        prect (dsFloaters ds)
  pushup _ [] = []
  pushup to (f : fs) =
    let delta = to - flCount f
    in if delta <= 0 then f : fs
       else f { flCount = to, flLimit = flLimit f + delta } :
            pushup (to + step) fs
  step = 8 :: Int

-------------------------------------------------------------------------------

data Message = Message Double String

makeMessage :: String -> Message
makeMessage string = Message (2.3 + fromIntegral (length string) / 30) string

decayMessage :: Message -> Maybe Message
decayMessage (Message t s) =
  let t' = t - secondsPerFrame in
  if t' <= 0 then Nothing else Just (Message t' s)

-------------------------------------------------------------------------------
