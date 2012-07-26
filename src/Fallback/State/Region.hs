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

module Fallback.State.Region
  (RegionState(..), rsFoundAreaLinks, tickRegionState)
where

import qualified Data.Set as Set

import Fallback.Data.Clock (Clock, clockInc)
import Fallback.Data.Couple (Couple, fromCouple)
import Fallback.State.Party
import Fallback.State.Tags (AreaTag, RegionTag, regionAreas)

-------------------------------------------------------------------------------

data RegionState = RegionState
  { rsClock :: Clock,
    rsParty :: Party,
    rsPreviousArea :: AreaTag,
    rsRegion :: RegionTag,
    rsSelectedArea :: AreaTag,
    rsUnsaved :: Bool }

-- | Return the set of area links, from the current region only, that the party
-- has found.
rsFoundAreaLinks :: RegionState -> Set.Set (Couple AreaTag)
rsFoundAreaLinks state =
  Set.filter isInThisRegion $ partyFoundAreaLinks $ rsParty state
  where
    isInThisRegion link = let (a, b) = fromCouple link
                          in Set.member a areaSet && Set.member b areaSet
    areaSet = Set.fromList $ regionAreas $ rsRegion state

-------------------------------------------------------------------------------

tickRegionState :: RegionState -> RegionState
tickRegionState state = state { rsClock = clockInc (rsClock state) }

-------------------------------------------------------------------------------
