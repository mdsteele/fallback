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

module Fallback.State.Region where

import qualified Data.Set as Set

import Fallback.Data.Clock (Clock, clockInc)
import Fallback.State.Party
import Fallback.State.Tags (AreaTag, RegionTag, regionAreas)

-------------------------------------------------------------------------------

data RegionState = RegionState
  { rsClock :: Clock,
    rsParty :: Party,
    rsPreviousArea :: AreaTag,
    rsRegion :: RegionTag,
    rsSelectedArea :: AreaTag }

rsFoundAreas :: RegionState -> Set.Set AreaTag
rsFoundAreas state =
  let nodes = partyFoundAreas $ rsParty state
  in Set.fromList $ filter (flip Set.member nodes) $
     regionAreas $ rsRegion state

-------------------------------------------------------------------------------

tickRegionState :: RegionState -> RegionState
tickRegionState state = state { rsClock = clockInc (rsClock state) }

-------------------------------------------------------------------------------
