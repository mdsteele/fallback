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

module Fallback.State.Creature.Utility
  (makeMonster, monstAttacks, monstCanFly, monstInvisibility, monstIsEntangled,
   monstIsSummoned, monstMaxHealth, monstRectSize, monstSpeed, monstType,
   monstWalksFast)
where

import Data.Maybe (isJust)

import Fallback.State.Creature.Base
import Fallback.State.Creature.Monsters (getMonsterType)
import Fallback.State.Simple (FaceDir(..), Invisibility(..), sizeSize)
import Fallback.State.Status
import Fallback.State.Tags (MonsterTag)

-------------------------------------------------------------------------------

-- | Make a monster of the given type, settings its fields to sane default
-- values (full health, non-ally, immobile AI, and everything else zeroed out).
-- You may need to override some of these values after calling this function.
makeMonster :: MonsterTag -> Monster
makeMonster tag = Monster
  { monstDeadVar = Nothing,
    monstHealth = mtMaxHealth mtype,
    monstIsAlly = False,
    monstMoments = 0,
    monstName = mtName mtype,
    monstPose = CreaturePose { cpAlpha = alpha, cpAnim = NoAnim,
                               cpFaceDir = FaceLeft },
    monstScript = Nothing,
    monstSpells = map (flip (,) 0) $ mtSpells mtype,
    monstStatus = initStatusEffects,
    monstSummoning = Nothing,
    monstTag = tag,
    monstTownAI = ImmobileAI }
  where
    mtype = getMonsterType tag
    alpha = if mtInherentInvisibility mtype == NoInvisibility then 255 else 0

-------------------------------------------------------------------------------

-- | Get all of the monster's available attacks.
monstAttacks :: Monster -> [MonsterAttack]
monstAttacks = mtAttacks . monstType

-- | Return 'True' if the monster can fly over e.g. water.
monstCanFly :: Monster -> Bool
monstCanFly = mtCanFly . monstType

-- | Get the current invisibility of the monster, taking into account both
-- status effects and the monster's inherent invisibility (if any).
monstInvisibility :: Monster -> Invisibility
monstInvisibility monst = max (mtInherentInvisibility $ monstType monst)
                              (seInvisibility $ monstStatus monst)

-- | Return 'True' if the monster is currently entangled, 'False' otherwise.
monstIsEntangled :: Monster -> Bool
monstIsEntangled = seIsEntangled . monstStatus

-- | Return 'True' if this is a summoned monster, 'False' otherwise.
monstIsSummoned :: Monster -> Bool
monstIsSummoned = isJust . monstSummoning

monstMaxHealth :: Monster -> Int
monstMaxHealth = mtMaxHealth . monstType

-- | Get the size of the 'PRect' taken up by the monster.
monstRectSize :: Monster -> (Int, Int)
monstRectSize = sizeSize . mtSize . monstType

-- | Get the monster's speed multiplier.
monstSpeed :: Monster -> Double
monstSpeed = mtSpeed . monstType

-- | Get the 'MonsterType' of the monster.
monstType :: Monster -> MonsterType
monstType = getMonsterType . monstTag

-- | Return 'True' if the monster can walk two spaces per town step, or 'False'
-- if the monster can only walk one space per town step.
monstWalksFast :: Monster -> Bool
monstWalksFast = mtWalksFast . monstType

-------------------------------------------------------------------------------
