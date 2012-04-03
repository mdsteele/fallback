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

module Fallback.State.Creature where

import Fallback.Constants (tileHeight, tileWidth)
import Fallback.Data.Point
import Fallback.Draw (Sprite)
import Fallback.State.Simple
import Fallback.State.Tags (MonsterSpellTag)

-------------------------------------------------------------------------------

data CreatureAnim = NoAnim
                  | AttackAnim Int
                  | HurtAnim Int
                  | WalkAnim Int Int Position
  deriving (Eq, Read, Show)

-- | Return the number of frames the animation will take to complete.
animDuration :: CreatureAnim -> Int
animDuration NoAnim = 0
animDuration (AttackAnim n) = n
animDuration (HurtAnim n) = n
animDuration (WalkAnim n _ _) = n

animOffset :: CreatureAnim -> Position -> IPoint
animOffset NoAnim _ = pZero
animOffset (AttackAnim _) _ = pZero
animOffset (HurtAnim n) _ = Point (2 - 4 * (n `mod` 2)) 0
animOffset (WalkAnim n m p1) p2 =
  let Point dx dy = p1 `pSub` p2
  in Point (dx * tileWidth * n `div` m) (dy * tileHeight * n `div` m)

tickCreatureAnim :: CreatureAnim -> CreatureAnim
tickCreatureAnim NoAnim = NoAnim
tickCreatureAnim (AttackAnim n) = if n > 1 then AttackAnim (n - 1) else NoAnim
tickCreatureAnim (HurtAnim n) = if n > 1 then HurtAnim (n - 1) else NoAnim
tickCreatureAnim (WalkAnim n m p) =
  if n > 1 then WalkAnim (n - 1) m p else NoAnim

-------------------------------------------------------------------------------

data CreatureImages = CreatureImages
  { ciRightStand :: Sprite,
    ciLeftStand :: Sprite,
    ciRightAttack :: Sprite,
    ciLeftAttack :: Sprite }

ciStand :: FaceDir -> CreatureImages -> Sprite
ciStand FaceLeft = ciLeftStand
ciStand FaceRight = ciRightStand

ciAttack :: FaceDir -> CreatureImages -> Sprite
ciAttack FaceLeft = ciLeftAttack
ciAttack FaceRight = ciRightAttack

-------------------------------------------------------------------------------

data MonsterAttack = MonsterAttack
  { maAppearance :: AttackAppearance,
    maCriticalChance :: Double,
    maDamageCount :: Int,
    maDamageRange :: (Int, Int),
    maElement :: AttackElement,
    maEffects :: [AttackEffect],
    maRange :: AttackRange }

data MonsterTownAI = ChaseAI -- chase party relentlessly
                   | GuardAI Position
                   | ImmobileAI -- never move; start combat when could attack
                   | MindlessAI -- chase party only when visible
                   | PatrolAI Position Position

data MonsterType = MonsterType
  { mtAgility :: Int,
    mtAttacks :: [MonsterAttack],
    mtCanFly :: Bool,
    --mtDeathSound :: SoundTag,
    --mtDefaultCombatAI :: MonsterCombatAI,
    mtExperienceValue :: Int,
    mtImageRow :: Int,
    mtIsDaemonic :: Bool,
    mtIsUndead :: Bool,
    mtLevel :: Int,
    mtMaxHealth :: Int,
    mtName :: String,
    mtResistances :: Resistances,
    mtSize :: CreatureSize,
    mtSpeed :: Double,
    mtSpells :: [MonsterSpellTag],
    mtWalksFast :: Bool }

-------------------------------------------------------------------------------
