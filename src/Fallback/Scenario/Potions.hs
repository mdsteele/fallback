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

module Fallback.Scenario.Potions (runPotionActions) where

import Fallback.Scenario.Script
import Fallback.State.Item (PotionAction(..), getPotionActions)
import Fallback.State.Resources (SoundTag(..))
import Fallback.State.Simple
import Fallback.State.Tags
  (ItemTag(PotionItemTag), PotionItemTag(..), isFoodItem)

-------------------------------------------------------------------------------

runPotionActions :: (FromAreaEffect f) => PotionItemTag -> CharacterNumber
                 -> Script f ()
runPotionActions potTag charNum = do
  case potTag of
    FocusStone -> playSound SndHeal
    PhoenixFeather -> playSound SndRevive
    _ | isFoodItem (PotionItemTag potTag) -> playSound SndEat
      | otherwise -> playSound SndDrink
  mapM_ (runPotionAction charNum) $ getPotionActions potTag

runPotionAction :: (FromAreaEffect f) => CharacterNumber -> PotionAction
                -> Script f ()
runPotionAction charNum action = do
  case action of
    BoostAdrenaline amount -> do
      alterAdrenaline charNum (+ amount)
    CurePoison amount -> do
      curePoison (HitCharacter charNum) (fromIntegral amount)
    FullyRestoreMojo -> do
      restoreMojoToFull charNum
    RegenHealth amount -> do
      grantRegeneration (HitCharacter charNum) (fromIntegral amount)
    RestoreFocus amount -> do
      alterFocus (HitCharacter charNum) (+ amount)
    RestoreHealth amount -> do
      healDamage [(HitCharacter charNum, fromIntegral amount)]
    RestoreMana amount -> do
      alterMana (HitCharacter charNum) (+ amount)
    _ -> debug "FIXME unimplemented potion action"

-------------------------------------------------------------------------------
