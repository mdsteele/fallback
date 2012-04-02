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

module Fallback.Scenario.Potions (runPotionAction) where

import Fallback.Scenario.Script
import Fallback.State.Area (AreaEffect)
import Fallback.State.Item (PotionAction(..))
import Fallback.State.Resources (SoundTag(..))
import Fallback.State.Simple

-------------------------------------------------------------------------------

runPotionAction :: PotionAction -> CharacterNumber -> Script AreaEffect ()
runPotionAction (HealAction amount) charNum = do
  playSound SndHeal
  healDamage [(HitCharacter charNum, fromIntegral amount)]

-------------------------------------------------------------------------------
