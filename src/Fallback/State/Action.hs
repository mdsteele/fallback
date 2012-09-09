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

import Data.Ix (range)
import qualified Data.Set as Set

import Fallback.Constants (sightRange)
import Fallback.Control.Script (Script)
import Fallback.Data.Point
import Fallback.State.Area
import Fallback.State.Simple

-------------------------------------------------------------------------------
-- Abilities:


data Ability = PassiveAbility
             | ActiveAbility ActionPoints CastingCost AbilityEffect

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
  MetaAbility :: APModifier -> CostModifier -> PowerModifier -> FeatEffect
  StandardFeat :: (Int -> TargetKind a)
               -> (CharacterNumber -> a -> Script CombatEffect ())
               -> FeatEffect

-------------------------------------------------------------------------------
-- Targeting:

data TargetKind :: * -> * where
  -- TODO Perhaps remove range limit from AllyTarget?  Maybe add a Bool
  -- indicating whether targeting unconscious characters is permitted?
  AllyTarget :: Int -> TargetKind (Either Position CharacterNumber)
  AreaTarget :: (forall a. (AreaState a) => a -> CharacterNumber -> Position ->
                 [Position]) -> Int -> TargetKind (Position, [Position])
  AutoTarget :: TargetKind ()
  JumpTarget :: (forall a. (AreaState a) => a -> CharacterNumber -> Position ->
                 [Position]) -> Int -> TargetKind (Position, [Position])
  MultiTarget :: Int {-num targets-} -> Int {-range-} -> TargetKind [Position]
  SingleTarget :: Int -> TargetKind Position

data Targeting :: * -> * where
  TargetingAlly :: Int -> Targeting (Either Position CharacterNumber)
  TargetingArea :: (forall a. (AreaState a) => a -> CharacterNumber ->
                    Position -> [Position]) -> Int
                -> Targeting (Position, [Position])
  TargetingJump :: (forall a. (AreaState a) => a -> CharacterNumber ->
                    Position -> [Position])
                -> Set.Set Position -> Targeting (Position, [Position])
  TargetingMulti :: Int {-max num targets-} -> Int {-range-}
                 -> [Position] {-targets so far-} -> Targeting [Position]
  TargetingSingle :: Int -> Targeting Position

-------------------------------------------------------------------------------

circleArea :: Position -> SqDist -> [Position]
circleArea center dist =
  let limit = floor (sqDistRadius dist)
      corner = Point limit limit
  in filter ((dist >=) . pSqDist center) $
     range (center `pSub` corner, center `pAdd` corner)

aoeTarget :: Int -> SqDist -> TargetKind (Position, [Position])
aoeTarget maxRange blastRadiusSquared = AreaTarget fn maxRange where
  fn :: (AreaState a) => a -> CharacterNumber -> Position -> [Position]
  fn _ _ target = circleArea target blastRadiusSquared

beamTarget :: TargetKind (Position, [Position])
beamTarget = AreaTarget fn sightRange where
  fn ars charNum = arsBeamPositions ars (arsCharacterPosition charNum ars)

coneTarget :: Double -> Int -> TargetKind (Position, [Position])
coneTarget degrees maxRange = AreaTarget fn maxRange where
  fn :: (AreaState a) => a -> CharacterNumber -> Position -> [Position]
  fn ars charNum target = if target == origin then [] else
    let fov = arsVisibleForCharacter charNum ars
        vecTo pos = fmap fromIntegral $ pos `pSub` origin
        canHit pos = pos /= origin && Set.member pos fov &&
                     pVectorAngle (vecTo target) (vecTo pos) <= semiAngle
    in filter canHit $ circleArea origin (ofRadius maxRange)
    where origin = arsCharacterPosition charNum ars
  semiAngle = pi * degrees / 360

splashTarget :: Int -> TargetKind (Position, [Position])
splashTarget maxRange = AreaTarget fn maxRange where
  fn ars charNum target =
    if origin == target || cannotSeeThrough (arsTerrainOpenness target ars)
    then [target] else
      let dir = origin `dirTo` target
      in [target, target `plusDir` pred dir, target `plusDir` dir,
          target `plusDir` succ dir]
    where origin = arsCharacterPosition charNum ars

wallTarget :: Int -> Int -> TargetKind (Position, [Position])
wallTarget maxRange radius = AreaTarget fn maxRange where
  fn :: (AreaState a) => a -> CharacterNumber -> Position -> [Position]
  fn ars charNum target =
    if origin == target || blocked target then [] else
      let (d1, d2, d3, d4) =
            if isCardinal dir
            then (pred $ pred dir, pred $ pred dir,
                  succ $ succ dir, succ $ succ dir)
            else (pred dir, pred $ pred $ pred dir,
                  succ dir, succ $ succ $ succ dir)
      in target : wing d1 d2 target radius ++ wing d3 d4 target radius
    where
      origin = arsCharacterPosition charNum ars
      dir = origin `dirTo` target
      blocked pos =
        arsOccupied pos ars ||
        case arsTerrainOpenness pos ars of
          TerrainOpen -> False
          TerrainHover -> False
          _ -> True
      wing dir1 dir2 start n =
        if n <= 0 then [] else
          let pos = start `plusDir` (if n `mod` 2 == 1 then dir1 else dir2)
          in if blocked pos then [] else pos : wing dir1 dir2 pos (n - 1)

-------------------------------------------------------------------------------
