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
  (HarmOrBenefit(..),
   StatusEffects, initStatusEffects, decayStatusEffects,
   -- * Getters
   seBlessing, --seBlessingMultiplier,
   seDefense, seArmorMultiplier,
   seHaste, seSpeedMultiplier,
   sePoison,
   seInvisibility, Invisibility(..),
   seMentalEffect, MentalEffect(..),
   seIsEntangled,
   seIsShielded, seMagicShieldMultiplier,
   -- * Setters and modifiers
   seAlterPoison, seApplyBlessing, seApplyDefense, seApplyEntanglement,
   seApplyHaste, seApplyMagicShield, sePurgeMentalEffects, seSetInvisibility,
   seWakeFromDaze,
   -- * Utilities
   compress, townifyStatus)
where

import Control.Exception (assert)
import Data.Maybe (isJust)

import Fallback.Utility (isFinite)

-------------------------------------------------------------------------------

-- | Represents the state of a status effect that can be negative (harmful) or
-- postitive (beneficial).  The 'Double' represents the time remaining, in
-- combat rounds, of the effect, and must always be finite and positive.
data HarmOrBenefit = Harmful Double
                   | Unaffected
                   | Beneficial Double
  deriving (Read, Show)

mergeHarmOrBenefit :: HarmOrBenefit -> HarmOrBenefit -> HarmOrBenefit
mergeHarmOrBenefit hb1 hb2 = merge (validate hb1) (validate hb2) where
  merge hb Unaffected = hb
  merge Unaffected hb = hb
  merge (Harmful h) (Beneficial b) = counter h b
  merge (Beneficial b) (Harmful h) = counter h b
  merge (Harmful x) (Harmful y) = Harmful (stack x y)
  merge (Beneficial x) (Beneficial y) = Beneficial (stack x y)

  counter harm bene =
    case compare harm bene of
      GT -> Harmful (harm - bene)
      EQ -> Unaffected
      LT -> Beneficial (bene - harm)

  -- When stacking two harms (or two benefits), the resulting duration is the
  -- geometric mean of the max and the sum.  Thus, if one of the effects is
  -- zero (or rather, nearly zero), you get the full effect of the other, but
  -- otherwise you get a reduced effect.
  stack a b = sqrt (max a b * (a + b))

  validate (Harmful a) = assert (isFinite a && a > 0) (Harmful a)
  validate Unaffected = Unaffected
  validate (Beneficial a) = assert (isFinite a && a > 0) (Beneficial a)

mergeMaybe :: Double -> Maybe Double -> Maybe Double
mergeMaybe a Nothing = Just a
mergeMaybe a (Just b) =
  assert (isFinite b && b > 0) $ Just $ sqrt (max a b * (a + b))

-------------------------------------------------------------------------------

data StatusEffects = StatusEffects
  { seBlessing :: HarmOrBenefit,
    seDefense :: HarmOrBenefit,
    seEntanglement :: Maybe Double, -- rounds remaining
    seHaste :: HarmOrBenefit,
    seInvisibility :: Maybe Invisibility,
    seMagicShield :: Maybe Double, -- rounds remaining
    seMentalEffect :: Maybe (MentalEffect, Double {-rounds remaining-}),
    sePoison :: Int } -- damage remaining
  deriving (Read, Show)

initStatusEffects :: StatusEffects
initStatusEffects = StatusEffects
  { seBlessing = Unaffected,
    seDefense = Unaffected,
    seEntanglement = Nothing,
    seHaste = Unaffected,
    seInvisibility = Nothing,
    seMagicShield = Nothing,
    seMentalEffect = Nothing,
    sePoison = 0 }

decayStatusEffects :: Double -> StatusEffects -> StatusEffects
decayStatusEffects rounds se =
  se { seBlessing = decayHarmOrBenefit (seBlessing se),
       seDefense = decayHarmOrBenefit (seDefense se),
       seEntanglement = decayMaybe (seEntanglement se),
       seHaste = decayHarmOrBenefit (seHaste se),
       seMagicShield = decayMaybe (seMagicShield se),
       seMentalEffect =
         case seMentalEffect se of
           Nothing -> Nothing
           Just (eff, t) -> decay (Just . (,) eff) Nothing t }
  where
    decayMaybe = (>>= decay Just Nothing)
    decayHarmOrBenefit (Harmful x) = decay Harmful Unaffected x
    decayHarmOrBenefit Unaffected = Unaffected
    decayHarmOrBenefit (Beneficial x) = decay Beneficial Unaffected x
    decay just none x = if x > rounds then just (x - rounds) else none

-------------------------------------------------------------------------------
-- Getters:

seArmorMultiplier :: StatusEffects -> Double
seArmorMultiplier se =
  case seDefense se of
    Harmful _ -> 1.25
    Unaffected -> 1
    Beneficial _ -> 0.75

seSpeedMultiplier :: StatusEffects -> Double
seSpeedMultiplier se =
  if fmap fst (seMentalEffect se) == Just DazedEffect then 0 else
    case seHaste se of
      Harmful _ -> 2/3
      Unaffected -> 1
      Beneficial _ -> 1.5

seIsEntangled :: StatusEffects -> Bool
seIsEntangled = isJust . seEntanglement

seIsShielded :: StatusEffects -> Bool
seIsShielded = isJust . seMagicShield

seMagicShieldMultiplier :: StatusEffects -> Double
seMagicShieldMultiplier = maybe 1 (const 0.5) . seMagicShield

-------------------------------------------------------------------------------
-- Setters:

seAlterPoison :: (Int -> Int) -> StatusEffects -> StatusEffects
seAlterPoison fn status = status { sePoison = max 0 $ fn (sePoison status) }

seApplyBlessing :: HarmOrBenefit -> StatusEffects -> StatusEffects
seApplyBlessing hb se =
  se { seBlessing = mergeHarmOrBenefit hb (seBlessing se) }

seApplyDefense :: HarmOrBenefit -> StatusEffects -> StatusEffects
seApplyDefense hb se = se { seDefense = mergeHarmOrBenefit hb (seDefense se) }

-- | Add (approximately) the given amount of entanglement, in rounds.
seApplyEntanglement :: Double -> StatusEffects -> StatusEffects
seApplyEntanglement x se =
  se { seEntanglement = mergeMaybe x (seEntanglement se) }

seApplyHaste :: HarmOrBenefit -> StatusEffects -> StatusEffects
seApplyHaste hb se = se { seHaste = mergeHarmOrBenefit hb (seHaste se) }

seApplyMagicShield :: Double -> StatusEffects -> StatusEffects
seApplyMagicShield x se =
  se { seMagicShield = mergeMaybe x (seMagicShield se) }

sePurgeMentalEffects :: StatusEffects -> StatusEffects
sePurgeMentalEffects se = se { seMentalEffect = Nothing }

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

data Invisibility = MinorInvisibility | MajorInvisibility
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
  { seInvisibility = Nothing,
    seMentalEffect = Nothing }

-------------------------------------------------------------------------------
