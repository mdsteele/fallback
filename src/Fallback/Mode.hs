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

module Fallback.Mode
  (module Fallback.Mode.Base, newBootUpMode)
where

import Fallback.Mode.Base
import Fallback.Mode.Combat (newCombatMode)
import Fallback.Mode.GameOver (newGameOverMode)
import Fallback.Mode.MainMenu (newMainMenuMode)
import Fallback.Mode.Region (newRegionMode)
import Fallback.Mode.Town (newTownMode)
import Fallback.State.Resources (newResources)

-------------------------------------------------------------------------------

newBootUpMode :: IO Mode
newBootUpMode = do
  resources <- newResources
  let modes = Modes { newCombatMode' = newCombatMode resources modes,
                      newGameOverMode' = newGameOverMode resources modes,
                      newMainMenuMode' = newMainMenuMode resources modes,
                      newRegionMode' = newRegionMode resources modes,
                      newTownMode' = newTownMode resources modes }
  newMainMenuMode' modes

-------------------------------------------------------------------------------
