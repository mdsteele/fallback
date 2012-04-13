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

{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}

module Fallback.State.Action where

import Fallback.Control.Script (Script)
import Fallback.Data.Point (Position)
import Fallback.State.Area (AreaEffect, AreaState, CombatEffect)
import Fallback.State.Simple

-------------------------------------------------------------------------------
-- Targeting:

data TargetKind :: * -> * where
  AllyTarget :: Int -> TargetKind (Either Position CharacterNumber)
  AreaTarget :: (forall a. (AreaState a) => a -> Position -> Position ->
                 [Position]) -> Int -> TargetKind (Position, [Position])
  AutoTarget :: TargetKind ()
  MultiTarget :: Int {-num targets-} -> Int {-range-} -> TargetKind [Position]
  SingleTarget :: Int -> TargetKind Position

-------------------------------------------------------------------------------
-- Abilities:


data Ability = PassiveAbility
             | ActiveAbility CastingCost AbilityEffect

data AbilityEffect :: * where
  MetaAttack :: MetaAttackType -> (Int -> TargetKind a)
             -> (CharacterNumber -> PowerModifier -> a ->
                 Script CombatEffect ()) -> AbilityEffect
  GeneralAbility :: TargetKind a -> (CharacterNumber -> PowerModifier -> a ->
                                     Script AreaEffect ()) -> AbilityEffect
  CombatAbility :: TargetKind a -> (CharacterNumber -> PowerModifier -> a ->
                                    Script CombatEffect ()) -> AbilityEffect

data MetaAttackType = MeleeOnly | RangedOnly | MeleeOrRanged

metaAttackMatches :: MetaAttackType -> AttackRange -> Bool
metaAttackMatches MeleeOrRanged _ = True
metaAttackMatches MeleeOnly Melee = True
metaAttackMatches RangedOnly (Ranged _) = True
metaAttackMatches _ _ = False

-------------------------------------------------------------------------------
-- Combat feats:

data CombatFeat = CombatFeat
  { cfName :: String,
    cfDescription :: String,
    cfIconCoords :: (Int, Int), -- (row, col)
    cfCastingCost :: CastingCost,
    cfEffect :: FeatEffect }

data FeatEffect :: * where
  MetaAbility :: CostModifier -> PowerModifier -> FeatEffect
  StandardFeat :: (Int -> TargetKind a)
               -> (CharacterNumber -> a -> Script CombatEffect ())
               -> FeatEffect

-------------------------------------------------------------------------------
