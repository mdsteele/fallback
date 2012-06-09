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

module Fallback.State.Creature.Monsters
  (getMonsterType)
where

import Control.Applicative (liftA2)
import Data.List (foldl')

import qualified Fallback.Data.TotalMap as TM (set)
import Fallback.State.Creature.Base
import Fallback.State.Simple
import Fallback.State.Status (Invisibility(..))
import Fallback.State.Tags (MonsterTag(..))

-------------------------------------------------------------------------------

getMonsterType :: MonsterTag -> MonsterType
getMonsterType Dactylid = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maCriticalChance = 0.1,
        maDamageCount = 10,
        maDamageRange = (1, 10) }, baseMonsterAttack
      { maAppearance = BreathAttack,
        maCriticalChance = 0.05,
        maDamageCount = 6,
        maDamageRange = (1, 8),
        maElement = FireAttack,
        maRange = Ranged 8 }],
    mtExperienceValue = 1000,
    mtImageRow = 28,
    mtInherentInvisibility = MajorInvisibility,
    mtIsDaemonic = True,
    mtMaxHealth = 1000,
    mtName = "Dactylid",
    mtResistances = resistances [ResistFire =% 25, ResistMental =% 100],
    mtSize = SizeSmall,
    mtSpells = [CrossBeam, TeleportAway],
    mtSpeed = 2 }
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
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maCriticalChance = 0.1,
        maDamageCount = 6,
        maDamageRange = (1, 8) }],
    mtExperienceValue = 12,
    mtImageRow = 9,
    mtLevel = 4,
    mtMaxHealth = 50,
    mtName = "Wolf",
    mtSpeed = 2 }
getMonsterType DemonWolf = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maCriticalChance = 0.15,
        maDamageCount = 8,
        maDamageRange = (1, 10) }, MonsterAttack
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
    mtMaxHealth = 300,
    mtName = "Demon Wolf",
    mtSize = SizeWide,
    mtSpeed = 2,
    mtSpells = [FireSpray] }
getMonsterType CaveBat = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maCriticalChance = 0.1,
        maDamageCount = 6,
        maDamageRange = (1, 8) }],
    mtCanFly = True,
    mtExperienceValue = 12,
    mtImageRow = 29,
    mtLevel = 4,
    mtMaxHealth = 50,
    mtName = "Cave Bat",
    mtSpeed = 1.5 }
getMonsterType Ghoul = baseMonsterType
  { mtAgility = 35,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maCriticalChance = 0.2,
        maDamageCount = 15,
        maDamageRange = (1, 10) }],
    mtExperienceValue = 120,
    mtImageRow = 6,
    mtIsUndead = True,
    mtLevel = 99,
    mtMaxHealth = 80,
    mtName = "Ghoul",
    mtSpeed = 1.2 }
getMonsterType Invisighoul = (getMonsterType Ghoul)
  { mtInherentInvisibility = MinorInvisibility,
    mtName = "Invisighoul" }
getMonsterType Skeleton = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BladeAttack,
        maCriticalChance = 0.1,
        maDamageCount = 12,
        maDamageRange = (1, 8) }],
    mtExperienceValue = 120,
    mtImageRow = 2,
    mtIsUndead = True,
    mtLevel = 99,
    mtMaxHealth = 80,
    mtName = "Skeleton",
    mtResistances =
      resistances [Armor =% 30, ResistMental =% 100, ResistStun =% 30],
    mtSpeed = 1.1,
    mtSpells = [BladeSweep] }
getMonsterType Wraith = baseMonsterType
  { mtAgility = 50,
    mtAttacks = [baseMonsterAttack
      { maAppearance = WandAttack,
        maCriticalChance = 0.05,
        maDamageCount = 8,
        maDamageRange = (1, 6),
        maEffects = [InflictCurse 0.1],
        maElement = EnergyAttack,
        maRange = Ranged 6 }],
    mtExperienceValue = 150,
    mtImageRow = 5,
    mtIsUndead = True,
    mtLevel = 99,
    mtMaxHealth = 75,
    mtName = "Wraith",
    mtSpeed = 1.5,
    mtSpells = [BlessMonsters, EntangleSpray] }
getMonsterType Zombie = baseMonsterType
  { mtAgility = 25,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BluntAttack,
        maCriticalChance = 0.05,
        maDamageCount = 10,
        maDamageRange = (1, 8) }],
    mtExperienceValue = 100,
    mtImageRow = 4,
    mtIsUndead = True,
    mtLevel = 99,
    mtMaxHealth = 150,
    mtName = "Zombie",
    mtSpeed = 0.9 }
getMonsterType Vhaegyst = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maCriticalChance = 0.8,
        maDamageCount = 8,
        maDamageRange = (1, 20),
        maElement = IceAttack }],
    mtExperienceValue = 500,
    mtImageRow = 3,
    mtIsUndead = True,
    mtMaxHealth = 3000,
    mtName = "Vhaegyst",
    mtResistances = resistances [ResistCold =% 75, ResistMental =% 100,
                                 ResistStun =% 50],
    mtSpeed = 3.5,
    mtSpells = [SummonOne True 4 5 20 [Ghoul, Skeleton, Zombie],
                SummonOne True 2 5 20 [Ghoul, Skeleton, Zombie]] }
getMonsterType TownManRed = townsperson { mtImageRow = 10 }
getMonsterType TownManApron = townsperson { mtImageRow = 14 }
getMonsterType TownWomanApron = townsperson { mtImageRow = 15 }
getMonsterType TownWomanBlue = townsperson { mtImageRow = 17 }
getMonsterType GuardArcher = baseMonsterType
  { mtAgility = 30,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BowAttack,
        maCriticalChance = 0.03,
        maDamageCount = 8,
        maDamageRange = (1, 5),
        maRange = Ranged 5 }],
    mtImageRow = 27,
    mtIsHuman = True,
    mtMaxHealth = 150,
    mtName = "Guard",
    mtResistances = (Armor =% 30) }
getMonsterType RogueIllusion0 = rogueillusion { mtImageRow = 20 }
getMonsterType RogueIllusion1 = rogueillusion { mtImageRow = 21 }
getMonsterType RogueIllusion2 = rogueillusion { mtImageRow = 22 }
getMonsterType RogueIllusion3 = rogueillusion { mtImageRow = 23 }
getMonsterType _ = baseMonsterType -- FIXME

-------------------------------------------------------------------------------

baseMonsterType :: MonsterType
baseMonsterType = MonsterType
  { mtAgility = 20,
    mtAttacks = [],
    mtCanFly = False,
    mtExperienceValue = 0,
    mtImageRow = 0,
    mtInherentInvisibility = NoInvisibility,
    mtIsDaemonic = False,
    mtIsHuman = False,
    mtIsUndead = False,
    mtLevel = 0,
    mtMaxHealth = 100,
    mtName = "???",
    mtResistances = nullResistances,
    mtSize = SizeSmall,
    mtSpeed = 1,
    mtSpells = [],
    mtWalksFast = False }

baseMonsterAttack :: MonsterAttack
baseMonsterAttack = MonsterAttack
  { maAppearance = BladeAttack,
    maCriticalChance = 0,
    maDamageCount = 1,
    maDamageRange = (1, 1),
    maElement = PhysicalAttack,
    maEffects = [],
    maRange = Melee }

townsperson :: MonsterType
townsperson = baseMonsterType
  { mtExperienceValue = 100,
    mtImageRow = 10,
    mtIsHuman = True,
    mtMaxHealth = 40,
    mtName = "Townsperson" }

rogueillusion :: MonsterType
rogueillusion = baseMonsterType
  { mtAgility = 80,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BladeAttack,
        maDamageCount = 1,
        maDamageRange = (1, 3) }],
    mtIsHuman = True,
    mtMaxHealth = 100,
    mtName = "Illusion",
    mtResistances = (ResistMental =% 100),
    mtSpeed = 1.5 }

(=%) :: Resistance -> Double -> Resistances
(=%) resist n = TM.set resist ((100 - n) / 100) nullResistances

resistances :: [Resistances] -> Resistances
resistances = foldl' (liftA2 (*)) nullResistances

-------------------------------------------------------------------------------
