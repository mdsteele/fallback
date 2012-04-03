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

import Control.Applicative (liftA2)
import Data.List (foldl')

import Fallback.Data.TotalMap (tmSet)
import Fallback.State.Creature
import Fallback.State.Simple
import Fallback.State.Tags (MonsterSpellTag(..), MonsterTag(..))

-------------------------------------------------------------------------------

getMonsterType :: MonsterTag -> MonsterType
getMonsterType Revenant = baseMonsterType
  { mtAgility = 30,
    mtAttacks = [MonsterAttack
      { maAppearance = BladeAttack,
        maCriticalChance = 0.05,
        maDamageCount = 10,
        maDamageRange = (1, 10),
        maElement = PhysicalAttack,
        maEffects = [],
        maRange = Melee }],
    mtExperienceValue = 100,
    mtImageRow = 0,
    mtIsUndead = True,
    mtMaxHealth = 200,
    mtName = "Revenant",
    mtResistances = (ResistStun =% 30),
    mtSize = SizeSmall,
    mtSpeed = 1.5,
    mtWalksFast = True }
getMonsterType Revenantor = baseMonsterType
  { mtAgility = 50,
    mtAttacks = map attack [EnergyAttack, FireAttack, IceAttack, AcidAttack],
    mtExperienceValue = 100,
    mtImageRow = 1,
    mtIsUndead = True,
    mtMaxHealth = 150,
    mtName = "Revenantor",
    mtResistances = resistances [ResistMental =% 40, ResistStun =% 15],
    mtSize = SizeSmall,
    mtSpeed = 1.5,
    -- TODO spells
    mtWalksFast = True }
  where
    attack element = MonsterAttack
      { maAppearance = WandAttack,
        maCriticalChance = 0.1,
        maDamageCount = 10,
        maDamageRange = (1, 10),
        maElement = element,
        maEffects = [],
        maRange = Ranged 3 }
getMonsterType MasterRevenant = baseMonsterType
  { mtAgility = 50,
    mtAttacks = [MonsterAttack
      { maAppearance = ClawAttack,
        maCriticalChance = 0.5,
        maDamageCount = 20,
        maDamageRange = (1, 20),
        maElement = PhysicalAttack,
        maEffects = [],
        maRange = Melee }],
    mtExperienceValue = 100,
    mtImageRow = 0,
    mtIsUndead = True,
    mtMaxHealth = 5000,
    mtName = "Od",
    mtResistances = resistances [ResistMental =% 100, ResistStun =% 50],
    mtSize = SizeTall,
    mtSpeed = 2.0,
    -- TODO spells
    mtWalksFast = True }
getMonsterType Wolf = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [MonsterAttack
      { maAppearance = ClawAttack,
        maCriticalChance = 0.1,
        maDamageCount = 6,
        maDamageRange = (1, 8),
        maElement = PhysicalAttack,
        maEffects = [],
        maRange = Melee }],
    mtExperienceValue = 12,
    mtImageRow = 9,
    mtLevel = 4,
    mtMaxHealth = 50,
    mtName = "Wolf",
    mtSpeed = 2 }
getMonsterType DemonWolf = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [MonsterAttack
      { maAppearance = ClawAttack,
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
    mtExperienceValue = 30,
    mtImageRow = 0,
    mtIsDaemonic = True,
    mtLevel = 6,
    mtMaxHealth = 100,
    mtName = "Demon Wolf",
    mtSize = SizeWide,
    mtSpeed = 2,
    mtSpells = [FireSpray] }
getMonsterType Ghoul = baseMonsterType
  { mtAgility = 35,
    mtAttacks = [MonsterAttack
      { maAppearance = BiteAttack,
        maCriticalChance = 0.2,
        maDamageCount = 15,
        maDamageRange = (1, 10),
        maElement = PhysicalAttack,
        maEffects = [],
        maRange = Melee }],
    mtExperienceValue = 120,
    mtImageRow = 6,
    mtIsUndead = True,
    mtLevel = 99,
    mtMaxHealth = 80,
    mtName = "Ghoul",
    mtSpeed = 1.2 }
getMonsterType Zombie = baseMonsterType
  { mtAgility = 25,
    mtAttacks = [MonsterAttack
      { maAppearance = BluntAttack,
        maCriticalChance = 0.05,
        maDamageCount = 10,
        maDamageRange = (1, 10),
        maElement = PhysicalAttack,
        maEffects = [],
        maRange = Melee }],
    mtExperienceValue = 100,
    mtImageRow = 4,
    mtIsUndead = True,
    mtLevel = 99,
    mtMaxHealth = 150,
    mtName = "Zombie",
    mtSpeed = 0.9 }
getMonsterType TownManRed = townsperson { mtImageRow = 10 }
getMonsterType TownManApron = townsperson { mtImageRow = 14 }
getMonsterType TownWomanBlue = townsperson { mtImageRow = 17 }
getMonsterType _ = baseMonsterType -- FIXME

-------------------------------------------------------------------------------

baseMonsterType :: MonsterType
baseMonsterType = MonsterType
  { mtAgility = 20,
    mtAttacks = [],
    mtCanFly = False,
    mtExperienceValue = 0,
    mtImageRow = 0,
    mtIsDaemonic = False,
    mtIsUndead = False,
    mtLevel = 0,
    mtMaxHealth = 100,
    mtName = "???",
    mtResistances = nullResistances,
    mtSize = SizeSmall,
    mtSpeed = 1,
    mtSpells = [],
    mtWalksFast = False }

townsperson :: MonsterType
townsperson = baseMonsterType
  { mtExperienceValue = 100,
    mtImageRow = 10,
    mtMaxHealth = 40,
    mtName = "Townsperson" }

(=%) :: Resistance -> Double -> Resistances
(=%) resist n = tmSet resist ((100 - n) / 100) nullResistances

resistances :: [Resistances] -> Resistances
resistances = foldl' (liftA2 (*)) nullResistances

-------------------------------------------------------------------------------
