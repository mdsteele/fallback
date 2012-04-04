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

module Fallback.View.Region
  (RegionAction(..), newRegionView)
where

import Control.Applicative ((<$>))
import Data.Foldable (foldMap, traverse_)
import Data.List (find)
import qualified Data.Set as Set

import Fallback.Data.Clock (clockZigzag)
import Fallback.Data.Color (Tint(Tint))
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.Scenario.Areas (areaLinks, areaLocation)
import Fallback.State.Party (partyClearedArea)
import Fallback.State.Region
import Fallback.State.Resources (Resources)
import Fallback.State.Tags (AreaTag, regionAreas)
import Fallback.View.Base
import Fallback.View.Widget (newSimpleTextButton)

-------------------------------------------------------------------------------

data RegionAction = SelectAreaNode AreaTag
                  | TravelToSelectedArea
                  | ShowMenu

newRegionView :: (MonadDraw m) => Resources -> String
              -> m (View RegionState RegionAction)
newRegionView resources bgPath = do
  compoundViewM
    [ newRegionMapView bgPath
    , subView_ (Rect 20 436 120 24) <$>
      newSimpleTextButton resources "Menu" [KeyEscape] ShowMenu
    , subView_ (Rect 500 436 120 24) <$>
      newSimpleTextButton resources "Travel" [KeyReturn] TravelToSelectedArea]

newRegionMapView :: (MonadDraw m) => String
                 -> m (View RegionState RegionAction)
newRegionMapView bgPath = do
  clearedNodeSprite <- loadSubSprite "gui/area-nodes.png" $ Rect 0 0 14 12
  unclearedNodeSprite <- loadSubSprite "gui/area-nodes.png" $ Rect 14 0 14 12
  selectedNodeStrip <- loadVStrip "gui/area-select.png" 4
  backgroundSprite <- loadSprite bgPath
  let

    paint state = do
      -- Paint background:
      rect <- canvasRect
      blitStretch backgroundSprite rect
      -- Paint links:
      let found = rsFoundAreas state
      let makeLinks tag =
            let makeLink dest = if dest < tag then (dest, tag) else (tag, dest)
            in Set.fromList $ map makeLink $ filter (`Set.member` found) $
               areaLinks tag
      let selected = rsSelectedArea state
      let drawLink (tag1, tag2) = do
            let pos1 = fmap fromIntegral $ areaLocation tag1
            let pos2 = fmap fromIntegral $ areaLocation tag2
            let perp = pPolar (2 :: Double) (pAtan2 (pos2 `pSub` pos1) + pi/2)
            let selectedTint = Tint 255 0 0 192
            let normalTint = Tint 255 255 0 192
            let (tint1, tint2) =
                  if (tag1, tag2) == (selected, rsPreviousArea state)
                  then (selectedTint, normalTint) else
                    if (tag1, tag2) == (rsPreviousArea state, selected)
                    then (normalTint, selectedTint)
                    else (normalTint, normalTint)
            gradientPolygon [(tint1, pos1 `pAdd` perp),
                             (tint1, pos1 `pSub` perp),
                             (tint2, pos2 `pSub` perp),
                             (tint2, pos2 `pAdd` perp)]
      traverse_ drawLink $ foldMap makeLinks found
      -- Paint area nodes:
      let party = rsParty state
      let paintAreaNode tag = do
            let sprite = if partyClearedArea party tag
                         then clearedNodeSprite else unclearedNodeSprite
            blitLoc sprite $ LocCenter $ areaLocation tag
      traverse_ paintAreaNode found
      -- Paint selection marker:
      blitLoc (selectedNodeStrip ! clockZigzag 4 2 (rsClock state)) $
        LocCenter $ areaLocation $ selected

    handler state (EvMouseDown pt) =
      let hit tag = pDist (fromIntegral <$> pt)
                          (fromIntegral <$> areaLocation tag) <= (20 :: Double)
      in return $ maybe Ignore Action $ fmap SelectAreaNode $ find hit $
         regionAreas $ rsRegion state
    handler _ _ = return Ignore

  return $ View paint handler

-------------------------------------------------------------------------------
