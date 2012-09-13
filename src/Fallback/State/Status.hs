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
  (-- * Status effects
   StatusEffects, initStatusEffects, decayStatusEffects,
   HarmOrBenefit(..), isBeneficial, isHarmful,
   -- ** Getters
   seBlessing, seAttackAgilityModifier, seAttackDamageMultiplier,
   seDefense, seArmorMultiplier,
   seHaste, seSpeedMultiplier,
   seRegenPoison,
   seInvisibility,
   seMentalEffect,
   seIsEntangled,
   seIsShielded, seMagicShieldMultiplier,
   -- ** Setters and modifiers
   seApplyBlessing, seReduceBlessing, seReduceCurse,
   seApplyDefense, seReduceDefense, seReduceWeakness,
   seApplyHaste, seReduceHaste, seReduceSlow,
   seAlterRegenPoison,
   seSetInvisibility,
   seApplyMentalEffect, sePurgeMentalEffects, seWakeFromDaze,
   seApplyEntanglement, sePurgeEntanglement,
   seApplyMagicShield, seReduceMagicShield,
   sePurgeAllBadEffects,
   -- * Status deltas
   StatusDelta, zeroStatusDelta, makeStatusDelta, applyStatusDelta,
   addStatusDeltas, sumStatusDeltas, divStatusDelta,
   -- * Utilities
   townifyStatus)
where

import Control.Exception (assert)
import Data.Maybe (fromMaybe, isJust)
import Data.List (foldl')

import Fallback.State.Simple (Invisibility(..), MentalEffect(..))
import Fallback.Utility (isFinite)

-------------------------------------------------------------------------------

-- | Represents the state of a status effect that can be negative (harmful) or
-- postitive (beneficial).  The 'Double' represents the time remaining, in
-- combat rounds, of the effect, and must always be finite and positive.
data HarmOrBenefit = Harmful Double
                   | Unaffected
                   | Beneficial Double
  deriving (Eq, Read, Show)

isBeneficial :: HarmOrBenefit -> Bool
isBeneficial (Beneficial _) = True
isBeneficial _ = False

isHarmful :: HarmOrBenefit -> Bool
isHarmful (Harmful _) = True
isHarmful _ = False

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

  validate (Harmful a) = assert (isFinitePositive a) (Harmful a)
  validate Unaffected = Unaffected
  validate (Beneficial a) = assert (isFinitePositive a) (Beneficial a)

mergeMaybe :: Double -> Maybe Double -> Maybe Double
mergeMaybe 0 mb = mb
mergeMaybe a mb = assert (isFinitePositive a) $
  case mb of
    Nothing -> Just a
    Just b -> assert (isFinitePositive b) $ Just $ stack a b

hobToDouble :: HarmOrBenefit -> Double
hobToDouble (Harmful x) = assert (isFinitePositive x) $ negate x
hobToDouble Unaffected = 0
hobToDouble (Beneficial x) = assert (isFinitePositive x) x

hobFromDouble :: Double -> HarmOrBenefit
hobFromDouble x = assert (isFinite x) $ if x == 0 then Unaffected else
  if x < 0 then Harmful (negate x) else Beneficial x

reduceBenefit :: Double -> HarmOrBenefit -> HarmOrBenefit
reduceBenefit x (Beneficial y) =
  if x < y then Beneficial (y - x) else Unaffected
reduceBenefit _ hob = hob

reduceHarm :: Double -> HarmOrBenefit -> HarmOrBenefit
reduceHarm x (Harmful y) = if x < y then Harmful (y - x) else Unaffected
reduceHarm _ hob = hob

reduceMaybe :: Double -> Maybe Double -> Maybe Double
reduceMaybe x (Just y) = if x < y then Just (y - x) else Nothing
reduceMaybe _ Nothing = Nothing

-------------------------------------------------------------------------------

data StatusEffects = StatusEffects
  { seBlessing :: HarmOrBenefit,
    seDefense :: HarmOrBenefit,
    seEntanglement :: Maybe Double, -- rounds remaining
    seHaste :: HarmOrBenefit,
    seInvisibility :: Invisibility,
    seMagicShield :: Maybe Double, -- rounds remaining
    seMental :: Maybe (MentalEffect, Double {-rounds remaining-}),
    seRegenPoison :: Int } -- health delta remaining
  deriving (Read, Show)

initStatusEffects :: StatusEffects
initStatusEffects = StatusEffects
  { seBlessing = Unaffected,
    seDefense = Unaffected,
    seEntanglement = Nothing,
    seHaste = Unaffected,
    seInvisibility = NoInvisibility,
    seMagicShield = Nothing,
    seMental = Nothing,
    seRegenPoison = 0 }

decayStatusEffects :: Double -> StatusEffects -> StatusEffects
decayStatusEffects rounds se =
  se { seBlessing = decayHarmOrBenefit (seBlessing se),
       seDefense = decayHarmOrBenefit (seDefense se),
       seEntanglement = decayMaybe (seEntanglement se),
       seHaste = decayHarmOrBenefit (seHaste se),
       seMagicShield = decayMaybe (seMagicShield se),
       seMental =
         case seMental se of
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

seAttackAgilityModifier :: StatusEffects -> Int
seAttackAgilityModifier se =
  case seBlessing se of
    Harmful _ -> negate 20
    Unaffected -> 0
    Beneficial _ -> 20

seAttackDamageMultiplier :: StatusEffects -> Double
seAttackDamageMultiplier se =
  case seBlessing se of
    Harmful _ -> 0.9
    Unaffected -> 1
    Beneficial _ -> 1.15

seArmorMultiplier :: StatusEffects -> Double
seArmorMultiplier se =
  case seDefense se of
    Harmful _ -> 1.25
    Unaffected -> 1
    Beneficial _ -> 0.75

seSpeedMultiplier :: StatusEffects -> Double
seSpeedMultiplier se =
  if fmap fst (seMental se) == Just Dazed then 0 else
    case seHaste se of
      Harmful _ -> 2/3
      Unaffected -> 1
      Beneficial _ -> 1.5

seMentalEffect :: StatusEffects -> Maybe MentalEffect
seMentalEffect = fmap fst . seMental

seIsEntangled :: StatusEffects -> Bool
seIsEntangled = isJust . seEntanglement

seIsShielded :: StatusEffects -> Bool
seIsShielded = isJust . seMagicShield

seMagicShieldMultiplier :: StatusEffects -> Double
seMagicShieldMultiplier = maybe 1 (const 0.5) . seMagicShield

-------------------------------------------------------------------------------
-- Setters:

seAlterRegenPoison :: (Int -> Int) -> StatusEffects -> StatusEffects
seAlterRegenPoison fn status =
  status { seRegenPoison = fn (seRegenPoison status) }

seApplyBlessing :: HarmOrBenefit -> StatusEffects -> StatusEffects
seApplyBlessing hb se =
  se { seBlessing = mergeHarmOrBenefit hb (seBlessing se) }

seReduceBlessing :: Double -> StatusEffects -> StatusEffects
seReduceBlessing x se = se { seBlessing = reduceBenefit x (seBlessing se) }

seReduceCurse :: Double -> StatusEffects -> StatusEffects
seReduceCurse x se = se { seBlessing = reduceHarm x (seBlessing se) }

seApplyDefense :: HarmOrBenefit -> StatusEffects -> StatusEffects
seApplyDefense hb se = se { seDefense = mergeHarmOrBenefit hb (seDefense se) }

seReduceDefense :: Double -> StatusEffects -> StatusEffects
seReduceDefense x se = se { seDefense = reduceBenefit x (seDefense se) }

seReduceWeakness :: Double -> StatusEffects -> StatusEffects
seReduceWeakness x se = se { seDefense = reduceHarm x (seDefense se) }

-- | Apply the given number of rounds of entanglement.  The argument must be
-- finite and non-negative.
seApplyEntanglement :: Double -> StatusEffects -> StatusEffects
seApplyEntanglement x se =
  se { seEntanglement = mergeMaybe x (seEntanglement se) }

-- | Completely remove all entanglement.
sePurgeEntanglement :: StatusEffects -> StatusEffects
sePurgeEntanglement se = se { seEntanglement = Nothing }

seApplyHaste :: HarmOrBenefit -> StatusEffects -> StatusEffects
seApplyHaste hb se = se { seHaste = mergeHarmOrBenefit hb (seHaste se) }

seReduceHaste :: Double -> StatusEffects -> StatusEffects
seReduceHaste x se = se { seHaste = reduceBenefit x (seHaste se) }

seReduceSlow :: Double -> StatusEffects -> StatusEffects
seReduceSlow x se = se { seHaste = reduceHarm x (seHaste se) }

-- | Apply the given number of rounds of magical shielding.  The argument must
-- be finite and non-negative.
seApplyMagicShield :: Double -> StatusEffects -> StatusEffects
seApplyMagicShield x se =
  se { seMagicShield = mergeMaybe x (seMagicShield se) }

seReduceMagicShield :: Double -> StatusEffects -> StatusEffects
seReduceMagicShield x se =
  se { seMagicShield = reduceMaybe x (seMagicShield se) }

seApplyMentalEffect :: MentalEffect -> Double -> StatusEffects -> StatusEffects
seApplyMentalEffect eff dur se = se { seMental = Just me } where
  me = case seMental se of
         Just (eff', dur') | eff' == eff -> (eff, stack dur dur')
         _ -> (eff, dur)

-- | Completely remove all mental effects.
sePurgeMentalEffects :: StatusEffects -> StatusEffects
sePurgeMentalEffects se = se { seMental = Nothing }

seSetInvisibility :: Invisibility -> StatusEffects -> StatusEffects
seSetInvisibility invis se = se { seInvisibility = invis }

seWakeFromDaze :: StatusEffects -> StatusEffects
seWakeFromDaze se =
  case seMental se of
    Just (Dazed, _) -> se { seMental = Nothing }
    _ -> se

sePurgeAllBadEffects :: StatusEffects -> StatusEffects
sePurgeAllBadEffects se =
  se { seBlessing = purgeHarmful (seBlessing se),
       seDefense = purgeHarmful (seDefense se),
       seEntanglement = Nothing, seHaste = purgeHarmful (seHaste se),
       seMental = Nothing, seRegenPoison = max 0 (seRegenPoison se) }
  where purgeHarmful (Harmful _) = Unaffected
        purgeHarmful hob = hob

-------------------------------------------------------------------------------
-- Status deltas:

data StatusDelta = StatusDelta
  { sdBlessing :: !Double,
    sdDefense :: !Double,
    sdEntanglement :: !Double,
    sdHaste :: !Double,
    sdMagicShield :: !Double,
    sdRegenPoison :: !Int }
  deriving Show

zeroStatusDelta :: StatusDelta
zeroStatusDelta = StatusDelta
  { sdBlessing = 0, sdDefense = 0, sdEntanglement = 0, sdHaste = 0,
    sdMagicShield = 0, sdRegenPoison = 0 }

addStatusDeltas :: StatusDelta -> StatusDelta -> StatusDelta
addStatusDeltas sd1 sd2 = StatusDelta
  { sdBlessing = sdBlessing sd1 + sdBlessing sd2,
    sdDefense = sdDefense sd1 + sdDefense sd2,
    sdEntanglement = sdEntanglement sd1 + sdEntanglement sd2,
    sdHaste = sdHaste sd1 + sdHaste sd2,
    sdMagicShield = sdMagicShield sd1 + sdMagicShield sd2,
    sdRegenPoison = sdRegenPoison sd1 + sdRegenPoison sd2 }

sumStatusDeltas :: [StatusDelta] -> StatusDelta
sumStatusDeltas = foldl' addStatusDeltas zeroStatusDelta

-- | Divide the status delta by the given positive integer.
divStatusDelta :: StatusDelta -> Int -> StatusDelta
divStatusDelta sd di = assert (di > 0) $ StatusDelta
  { sdBlessing = sdBlessing sd / dd, sdDefense = sdDefense sd / dd,
    sdEntanglement = sdEntanglement sd / dd, sdHaste = sdHaste sd / dd,
    sdMagicShield = sdMagicShield sd / dd,
    sdRegenPoison = sdRegenPoison sd `quot` di }
  where dd = fromIntegral di

-- | Make a status delta by subtracting the second set of status effects from
-- the first.
makeStatusDelta :: StatusEffects -> StatusEffects -> StatusDelta
makeStatusDelta se1 se2 = StatusDelta
  { sdBlessing = hobToDouble (seBlessing se1) - hobToDouble (seBlessing se2),
    sdDefense = hobToDouble (seDefense se1) - hobToDouble (seDefense se2),
    sdEntanglement = fromMaybe 0 (seEntanglement se1) -
                     fromMaybe 0 (seEntanglement se2),
    sdHaste = hobToDouble (seHaste se1) - hobToDouble (seHaste se2),
    sdMagicShield = fromMaybe 0 (seMagicShield se1) -
                    fromMaybe 0 (seMagicShield se2),
    sdRegenPoison = seRegenPoison se1 - seRegenPoison se2 }

applyStatusDelta :: StatusDelta -> StatusEffects -> StatusEffects
applyStatusDelta sd =
  (seApplyBlessing $ hobFromDouble $ sdBlessing sd) .
  (seApplyDefense $ hobFromDouble $ sdDefense sd) .
  (seApplyEntanglement $ sdEntanglement sd) .
  (seApplyHaste $ hobFromDouble $ sdHaste sd) .
  (seApplyMagicShield $ sdMagicShield sd) .
  (seAlterRegenPoison (+ sdRegenPoison sd))

-------------------------------------------------------------------------------
-- Utilities:

-- | When stacking two harms (or two benefits), the resulting duration is the
-- geometric mean of the max and the sum.  Thus, if one of the effects is zero
-- (or rather, nearly zero), you get the full effect of the other, but
-- otherwise you get a reduced effect.
stack :: Double -> Double -> Double
stack a b = sqrt (max a b * (a + b))

townifyStatus :: StatusEffects -> StatusEffects
townifyStatus status = status
  { seInvisibility = NoInvisibility,
    seMental = Nothing }

isFinitePositive :: Double -> Bool
isFinitePositive x = isFinite x && x > 0

-------------------------------------------------------------------------------
