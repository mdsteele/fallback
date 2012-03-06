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

module Fallback.Scenario.Monsters
  (getMonsterType)
where

import Fallback.State.Creature
import Fallback.State.Simple
import Fallback.State.Tags (MonsterSpellTag(..), MonsterTag(..))

-------------------------------------------------------------------------------

getMonsterType :: MonsterTag -> MonsterType
getMonsterType Wolf = MonsterType
  { mtAttacks = [MonsterAttack
      { maAppearance = MeleeAttack,
        maCriticalChance = 0.1,
        maDamageCount = 6,
        maDamageRange = (1, 8),
        maElement = PhysicalAttack,
        maEffects = [],
        maRange = Melee }],
    mtCanFly = False,
    mtExperienceValue = 12,
    mtImageRow = 9,
    mtIsDaemonic = False,
    mtIsUndead = False,
    mtLevel = 4,
    mtMaxHealth = 50,
    mtName = "Wolf",
    mtResistances = nullResistances,
    mtSize = SizeSmall,
    mtSpeed = 2,
    mtSpells = [],
    mtWalksFast = False }
getMonsterType DemonWolf = MonsterType
  { mtAttacks = [MonsterAttack
      { maAppearance = MeleeAttack,
        maCriticalChance = 0.15,
        maDamageCount = 8,
        maDamageRange = (1, 10),
        maElement = PhysicalAttack,
        maEffects = [],
        maRange = Melee }, MonsterAttack
      { maAppearance = BreathAttack,
        maCriticalChance = 0.05,
        maDamageCount = 8,
        maDamageRange = (2, 6),
        maElement = AcidAttack,
        maEffects = [InflictPoison 1.5],
        maRange = Ranged 4 }],
    mtCanFly = False,
    mtExperienceValue = 30,
    mtImageRow = 0,
    mtIsDaemonic = True,
    mtIsUndead = False,
    mtLevel = 6,
    mtMaxHealth = 100,
    mtName = "Demon Wolf",
    mtResistances = nullResistances,
    mtSize = SizeWide,
    mtSpeed = 2,
    mtSpells = [FireSpray],
    mtWalksFast = False }
getMonsterType TownManRed = townsperson { mtImageRow = 10 }
getMonsterType TownManApron = townsperson { mtImageRow = 14 }
getMonsterType TownWomanBlue = townsperson { mtImageRow = 17 }
getMonsterType _ = MonsterType -- FIXME
  { mtAttacks = [MonsterAttack
      { maAppearance = MeleeAttack,
        maCriticalChance = 0.05,
        maDamageCount = 10,
        maDamageRange = (1, 10),
        maElement = PhysicalAttack,
        maEffects = [],
        maRange = Melee }], -- FIXME
    mtCanFly = False,
    mtExperienceValue = 100,
    mtImageRow = 0,
    mtIsDaemonic = False,
    mtIsUndead = True,
    mtLevel = 99,
    mtMaxHealth = 80,
    mtName = "Revenant",
    mtResistances = nullResistances,
    mtSize = SizeSmall,
    mtSpeed = 1.25,
    mtSpells = [],
    mtWalksFast = True }

-- newMonsterImages :: MonsterTag -> IO CreatureImages
-- newMonsterImages tag =
--   case mtSize mtype of
--     SizeSmall -> CreatureImages <$> sprite 0 row 1 1 <*> sprite 1 row 1 1
--                                 <*> sprite 2 row 1 1 <*> sprite 3 row 1 1
--     SizeWide -> CreatureImages <$> sprite 0 row 2 1 <*> sprite 0 (row + 1) 2 1
--                                <*> sprite 2 row 2 1 <*> sprite 2 (row + 1) 2 1
--     SizeTall -> CreatureImages <$> sprite 0 row 1 2 <*> sprite 1 row 1 2
--                                <*> sprite 2 row 1 2 <*> sprite 3 row 1 2
--     SizeHuge -> CreatureImages <$> sprite 0 row 2 2 <*> sprite 0 (row + 2) 2 2
--                                <*> sprite 2 row 2 2 <*> sprite 2 (row + 2) 2 2
--   where
--     mtype = getMonsterType tag
--     row = mtImageRow mtype
--     sprite x y w h =
--       runDraw $ loadSubSprite "monsters.png" $
--       Rect (x * tileWidth) (y * tileHeight) (w * tileWidth) (h * tileHeight)

-------------------------------------------------------------------------------

townsperson :: MonsterType
townsperson = MonsterType
  { mtAttacks = [],
    mtCanFly = False,
    mtExperienceValue = 100,
    mtImageRow = 10,
    mtIsDaemonic = False,
    mtIsUndead = False,
    mtLevel = 0,
    mtMaxHealth = 40,
    mtName = "Townsperson",
    mtResistances = nullResistances,
    mtSize = SizeSmall,
    mtSpeed = 1,
    mtSpells = [],
    mtWalksFast = False }

-------------------------------------------------------------------------------
