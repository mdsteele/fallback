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

module Fallback.State.Creature
  (CreaturePose(..), tickCreaturePose,
   CreatureAnim(..), animOffset,
   CreatureImages(..), ciStand, ciAttack,
   MonsterAttack(..), MonsterTownAI(..), MonsterType(..))
where

import Data.Word (Word8)

import Fallback.Constants (secondsPerFrame, tileHeight, tileWidth)
import Fallback.Data.Point
import Fallback.Draw (Sprite)
import Fallback.State.Simple
import Fallback.State.Status (Invisibility(..))
import Fallback.State.Tags (MonsterSpellTag)

-------------------------------------------------------------------------------

data CreaturePose = CreaturePose
  { cpAlpha :: Word8,
    cpAnim :: CreatureAnim,
    cpFaceDir :: FaceDir }
  deriving (Read, Show)

tickCreaturePose :: Invisibility -> Bool -> CreaturePose -> CreaturePose
tickCreaturePose invis canSee pose = pose { cpAlpha = alpha', cpAnim = anim' }
  where
    anim' = tickCreatureAnim (cpAnim pose)
    shouldSee = case anim' of AttackAnim _ -> True
                              HurtAnim _ -> True
                              _ -> False
    desiredAlpha = if invis == NoInvisibility || shouldSee then 255
                   else if canSee then 128 else 0
    alpha' = if alpha > desiredAlpha
             then max desiredAlpha (alpha - min alpha 16)
             else min desiredAlpha (alpha + min (255 - alpha) 64)
    alpha = cpAlpha pose

-------------------------------------------------------------------------------

data CreatureAnim = NoAnim
                  | AttackAnim Int
                  | HurtAnim Int
                  | JumpAnim Int Int Position
                  | WalkAnim Int Int Position
  deriving (Eq, Read, Show)

animOffset :: CreatureAnim -> Position -> IPoint
animOffset NoAnim _ = pZero
animOffset (AttackAnim _) _ = pZero
animOffset (HurtAnim n) _ = Point (2 - 4 * (n `mod` 2)) 0
animOffset (JumpAnim count limit p1) p2 =
  let gravity = 80 -- arbitrary value that gives reasonably nice-looking motion
      time = fromIntegral limit * secondsPerFrame
      height = gravity * time * time
      x0 = fromIntegral (tileWidth * (pointX p1 - pointX p2))
      y0 = fromIntegral (tileHeight * (pointY p1 - pointY p2))
      t = fromIntegral count / fromIntegral limit
      x = t * x0
      y = t * y0 - 4 * (t - t * t) * height
  in Point (round x) (round y)
animOffset (WalkAnim n m p1) p2 =
  let Point dx dy = p1 `pSub` p2
  in Point (dx * tileWidth * n `div` m) (dy * tileHeight * n `div` m)

tickCreatureAnim :: CreatureAnim -> CreatureAnim
tickCreatureAnim NoAnim = NoAnim
tickCreatureAnim (AttackAnim n) = if n > 1 then AttackAnim (n - 1) else NoAnim
tickCreatureAnim (HurtAnim n) = if n > 1 then HurtAnim (n - 1) else NoAnim
tickCreatureAnim (JumpAnim n m p) =
  if n > 1 then JumpAnim (n - 1) m p else NoAnim
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
                   | DrunkAI PRect -- random walk within rect
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
    mtInherentInvisibility :: Invisibility,
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
