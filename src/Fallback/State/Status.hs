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

module Fallback.State.Status
  (StatusEffects, initStatusEffects, decayStatusEffects,
   -- * Getters
   seBlessingOrdering, --seBlessingMultiplier,
   seDefenseOrdering, seArmorMultiplier,
   seHasteOrdering, seSpeedMultiplier,
   sePoison,
   seInvisibility, Invisibility(..),
   seMentalEffect, MentalEffect(..),
   seIsEntangled,
   seIsShielded, seMagicShieldMultiplier,
   -- * Setters and modifiers
   seAlterPoison, seApplyArmor, seApplyEntanglement, seApplyHaste,
   seApplyMagicShield, seSetInvisibility, seWakeFromDaze,
   -- * Utilities
   compress, townifyStatus)
where

import Fallback.Constants (secondsPerFrame)

-------------------------------------------------------------------------------

data StatusEffects = StatusEffects
  { seEntanglement :: Double {-seconds remaining-},
    seInvisibility :: Maybe Invisibility,
    seMentalEffect :: Maybe (MentalEffect, Double {-seconds remaining-}),
    sePoison :: Int, -- damage remaining
    seRawBlessing :: Double,
    seRawDefense :: Double,
    seRawHaste :: Double,
    seRawMagicShield :: Double }
  deriving (Read, Show)

initStatusEffects :: StatusEffects
initStatusEffects = StatusEffects
  { seEntanglement = 0,
    seInvisibility = Nothing,
    seMentalEffect = Nothing,
    sePoison = 0,
    seRawBlessing = 0,
    seRawDefense = 0,
    seRawHaste = 0,
    seRawMagicShield = 0 }

decayStatusEffects :: StatusEffects -> StatusEffects
decayStatusEffects se =
  se { seEntanglement = max 0 (seEntanglement se - secondsPerFrame),
       seMentalEffect =
         case seMentalEffect se of
           Nothing -> Nothing
           Just (eff, t) ->
             let t' = t - secondsPerFrame
             in if t' <= 0 then Nothing else Just (eff, t'),
       seRawBlessing = decay 2 (seRawBlessing se),
       seRawDefense = decay 2 (seRawDefense se),
       seRawHaste = decay 2 (seRawHaste se),
       seRawMagicShield = decay 2 (seRawMagicShield se) }
  where decay halflife x = if abs x < 0.001 then 0
                           else x * exp (secondsPerFrame * log 0.5 / halflife)

-------------------------------------------------------------------------------
-- Getters:

-- | Return 'GT' if the creature is blessed, 'LT' if the creature is cursed, or
-- 'EQ' for neither.
seBlessingOrdering :: StatusEffects -> Ordering
seBlessingOrdering se = compare (seRawBlessing se) 0

-- | Return 'GT' if defense is increased, 'LT' if defense is decreased, or 'EQ'
-- for neither.
seDefenseOrdering :: StatusEffects -> Ordering
seDefenseOrdering se = compare (seRawDefense se) 0

seArmorMultiplier :: StatusEffects -> Double
seArmorMultiplier se = 1 - compress 0.75 (seRawDefense se)

-- | Return 'GT' if the creature is hasted, 'LT' if the creature is slowed, or
-- 'EQ' for neither.
seHasteOrdering :: StatusEffects -> Ordering
seHasteOrdering se = compare (seRawHaste se) 0

seSpeedMultiplier :: StatusEffects -> Double
seSpeedMultiplier se =
  if fmap fst (seMentalEffect se) == Just DazedEffect then 0 else
    exp $ compress (log 2) $ seRawHaste se

seIsEntangled :: StatusEffects -> Bool
seIsEntangled se = seEntanglement se > 0

seIsShielded :: StatusEffects -> Bool
seIsShielded se = seRawMagicShield se > 0

seMagicShieldMultiplier :: StatusEffects -> Double
seMagicShieldMultiplier se = 1 - compress 0.75 (seRawMagicShield se)

-------------------------------------------------------------------------------
-- Setters:

seAlterPoison :: (Int -> Int) -> StatusEffects -> StatusEffects
seAlterPoison fn status = status { sePoison = max 0 $ fn (sePoison status) }

seApplyArmor :: Double -> StatusEffects -> StatusEffects
seApplyArmor delta se = se { seRawDefense = delta + seRawDefense se }

seApplyEntanglement :: Double -> StatusEffects -> StatusEffects
seApplyEntanglement _ = id -- FIXME

seApplyHaste :: Double -> StatusEffects -> StatusEffects
seApplyHaste delta se = se { seRawHaste = delta + seRawHaste se }

seApplyMagicShield :: Double -> StatusEffects -> StatusEffects
seApplyMagicShield delta se =
  se { seRawMagicShield = delta + seRawMagicShield se }

seSetInvisibility :: Maybe Invisibility -> StatusEffects -> StatusEffects
seSetInvisibility mbInvis se = se { seInvisibility = mbInvis }

seWakeFromDaze :: StatusEffects -> StatusEffects
seWakeFromDaze se =
  case seMentalEffect se of
    Just (DazedEffect, _) -> se { seMentalEffect = Nothing }
    _ -> se

-------------------------------------------------------------------------------
-- Types:

data MentalEffect = DazedEffect | ConfusedEffect | CharmedEffect
  deriving (Eq, Read, Show)

data Invisibility = MinorInvisibility | MediumInvisibility | MajorInvisibility
  deriving (Eq, Ord, Read, Show)

-------------------------------------------------------------------------------
-- Utilities:

-- | \"Compress\" a value so that it falls between -@limit@ and +@limit@.  When
-- the input value is near zero, the curve is near-linear and the return value
-- will be approximately equal to the input value; as the magnitude of the
-- input value grows larger, the return value will taper off smoothly so that
-- its magnitude never exceeds the limit.  The limit must be positive.
compress :: Double {-^limit-} -> Double {-^value-} -> Double
compress limit value = 2 * limit / (1 + exp (-2 * value / limit)) - limit

townifyStatus :: StatusEffects -> StatusEffects
townifyStatus status = status
  { seEntanglement = 0,
    seInvisibility = Nothing,
    seMentalEffect = Nothing }

-------------------------------------------------------------------------------
