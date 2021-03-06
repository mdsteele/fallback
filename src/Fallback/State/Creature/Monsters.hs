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
import Fallback.State.Tags (MonsterTag(..))

-------------------------------------------------------------------------------

-- TODO Clean up the order of the monsters in here.
getMonsterType :: MonsterTag -> MonsterType
getMonsterType BlueDrake = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 10,
        maDamageRange = (1, 10) }, baseMonsterAttack
      { maAppearance = BreathAttack,
        maCriticalChance = 0.3,
        maDamageCount = 6,
        maDamageRange = (1, 8),
        maElement = ColdDamage,
        maRange = Ranged 6 }],
    mtExperienceValue = 300,
    mtImageRow = 2,
    mtMaxHealth = 1000,
    mtName = "Blue Drake",
    mtRemains = Just LargeBlood,
    mtResistances = resistances [ResistCold =% 50, ResistFire =% 20,
                                 ResistMental =% 70, ResistStun =% 20],
    mtSize = SizeWide,
    mtSpells = [IceBeam 4],
    mtSpeed = 2 }
getMonsterType Dactylid = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 10,
        maDamageRange = (1, 10) }, baseMonsterAttack
      { maAppearance = BreathAttack,
        maCriticalChance = 0.05,
        maDamageCount = 6,
        maDamageRange = (1, 8),
        maElement = FireDamage,
        maRange = Ranged 8 }],
    mtExperienceValue = 1000,
    mtImageRow = 79,
    mtInherentInvisibility = MajorInvisibility,
    mtIsDaemonic = True,
    mtMaxHealth = 1000,
    mtName = "Dactylid",
    mtResistances = resistances [ResistFire =% 25, ResistMental =% 100],
    mtSpells = [CrossBeam, TeleportAway],
    mtSpeed = 2 }
getMonsterType Revenant = baseMonsterType
  { mtAgility = 30,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BladeAttack,
        maCriticalChance = 0.05,
        maDamageCount = 8,
        maDamageRange = (1, 10) }],
    mtExperienceValue = 100,
    mtImageRow = 99,
    mtIsUndead = True,
    mtMaxHealth = 200,
    mtName = "Revenant",
    mtResistances = (ResistStun =% 30),
    mtSpeed = 1.5,
    mtWalksFast = True }
getMonsterType Revenantor = baseMonsterType
  { mtAgility = 50,
    mtAttacks = map attack [EnergyDamage, FireDamage, ColdDamage, AcidDamage],
    mtExperienceValue = 100,
    mtImageRow = 98,
    mtIsUndead = True,
    mtMaxHealth = 150,
    mtName = "Revenantor",
    mtResistances = resistances [ResistMental =% 40, ResistStun =% 15],
    mtSpeed = 1.5,
    -- TODO spells
    mtWalksFast = True }
  where
    attack element = baseMonsterAttack
      { maAppearance = WandAttack,
        maDamageCount = 8,
        maDamageRange = (1, 10),
        maElement = element,
        maRange = Ranged 3 }
getMonsterType MasterRevenant = baseMonsterType
  { mtAgility = 50,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maCriticalChance = 0.5,
        maDamageCount = 20,
        maDamageRange = (1, 20) }],
    mtCoins = (120, 150),
    mtExperienceValue = 100,
    mtImageRow = 0,
    mtIsUndead = True,
    mtMaxHealth = 5000,
    mtName = "Od",
    mtResistances = resistances [ResistMental =% 100, ResistStun =% 50],
    mtSize = SizeTall,
    mtSpeed = 3.0,
    -- TODO spells
    mtWalksFast = True }
-- Hound: deals little damage, but very fast and agile
getMonsterType Hound = baseMonsterType
  { mtAgility = 90,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 3,
        maDamageRange = (1, 8) }],
    mtExperienceValue = 12,
    mtImageRow = 41,
    mtMaxHealth = 40,
    mtName = "Hound",
    mtRemains = Just SmallBlood,
    mtSpeed = 5 }
getMonsterType Wolf = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 6,
        maDamageRange = (1, 8) }],
    mtExperienceValue = 12,
    mtImageRow = 42,
    mtMaxHealth = 50,
    mtName = "Wolf",
    mtRemains = Just MediumBlood,
    mtSpeed = 2 }
getMonsterType MutantWolf = baseMonsterType
  { mtAgility = 70,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 10,
        maDamageRange = (1, 8) }, MonsterAttack
      { maAppearance = BreathAttack,
        maCriticalChance = 0.05,
        maDamageCount = 8,
        maDamageRange = (2, 5),
        maElement = AcidDamage,
        maEffects = [InflictPoison 0.5],
        maRange = Ranged 3 }],
    mtExperienceValue = 20,
    mtImageRow = 44,
    mtMaxHealth = 200,
    mtName = "Mutant Wolf",
    mtRemains = Just LargeBlood,
    mtSpeed = 2.5,
    mtWalksFast = True }
getMonsterType DaemonWolf = baseMonsterType
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
        maElement = AcidDamage,
        maEffects = [InflictPoison 1.5],
        maRange = Ranged 4 }],
    mtCoins = (20, 30),
    mtExperienceValue = 30,
    mtImageRow = 0,
    mtIsDaemonic = True,
    mtMaxHealth = 600,
    mtName = "Daemon Wolf",
    mtRemains = Just LargeBlood,
    mtSize = SizeWide,
    mtSpeed = 2,
    mtSpells = [FireSpray] }
getMonsterType CaveBat = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maDamageCount = 6,
        maDamageRange = (1, 8) }],
    mtCanFly = True,
    mtExperienceValue = 12,
    mtImageRow = 50,
    mtMaxHealth = 50,
    mtName = "Cave Bat",
    mtRemains = Just SmallBlood,
    mtSpeed = 1.5,
    mtWalksFast = True }
getMonsterType FireBat = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maDamageCount = 6,
        maDamageRange = (1, 8) }, baseMonsterAttack
      { maAppearance = BreathAttack,
        maCriticalChance = 0.05,
        maDamageCount = 6,
        maDamageRange = (1, 8),
        maElement = FireDamage,
        maRange = Ranged 4 }],
    mtCanFly = True,
    mtExperienceValue = 12,
    mtImageRow = 51,
    mtMaxHealth = 50,
    mtName = "Fire Bat",
    mtRemains = Just SmallBlood,
    mtSpeed = 1.5,
    mtWalksFast = True }
getMonsterType RabidBat = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maDamageCount = 6,
        maDamageRange = (1, 8),
        maEffects = [InflictPoison 1.0] }],
    mtCanFly = True,
    mtExperienceValue = 12,
    mtImageRow = 52,
    mtMaxHealth = 50,
    mtName = "Rabid Bat",
    mtRemains = Just SmallBlood,
    mtSpeed = 1.5,
    mtWalksFast = True }
getMonsterType Ghoul = baseMonsterType
  { mtAgility = 35,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maCriticalChance = 0.3,
        maDamageCount = 15,
        maDamageRange = (1, 10) }],
    mtExperienceValue = 120,
    mtImageRow = 83,
    mtIsUndead = True,
    mtMaxHealth = 80,
    mtName = "Ghoul",
    mtRemains = Just Bones,
    mtSpeed = 1.4 }
getMonsterType Invisighoul = (getMonsterType Ghoul)
  { mtInherentInvisibility = MinorInvisibility,
    mtName = "Invisighoul" }
getMonsterType Ghast = baseMonsterType
  { mtAgility = 35,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 12,
        maDamageRange = (1, 10),
        maEffects = [InflictSlow 0.02] }],
    mtExperienceValue = 120,
    mtImageRow = 84,
    mtIsUndead = True,
    mtMaxHealth = 120,
    mtName = "Ghast",
    mtRemains = Just Bones,
    mtSpeed = 1.2 }
getMonsterType Wight = baseMonsterType
  { mtAgility = 35,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 12,
        maDamageRange = (1, 10),
        maEffects = [DrainMana 0.5, ReduceMagicShield 0.02],
        maElement = ColdDamage }],
    mtExperienceValue = 120,
    mtImageRow = 86,
    mtIsUndead = True,
    mtMaxHealth = 120,
    mtName = "Wight",
    mtRemains = Just Bones,
    mtResistances = resistances [ResistCold =% 50, ResistMental =% 50],
    mtSpeed = 1.2 }
getMonsterType Skeleton = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BladeAttack,
        maDamageCount = 12,
        maDamageRange = (1, 8) }],
    mtExperienceValue = 120,
    mtImageRow = 87,
    mtIsUndead = True,
    mtMaxHealth = 80,
    mtName = "Skeleton",
    mtRemains = Just Bones,
    mtResistances =
      resistances [Armor =% 30, ResistMental =% 100, ResistStun =% 40],
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
        maElement = EnergyDamage,
        maRange = Ranged 6 }],
    mtExperienceValue = 150,
    mtImageRow = 92,
    mtIsUndead = True,
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
    mtImageRow = 81,
    mtIsUndead = True,
    mtMaxHealth = 150,
    mtName = "Zombie",
    mtRemains = Just SmallIchor,
    mtResistances = (ResistStun =% 60),
    mtSpeed = 0.9 }
getMonsterType Bonemaster = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 10,
        maDamageRange = (1, 15),
        maEffects = [InflictMental Confused 0.07] }],
    mtCoins = (100, 200),
    mtExperienceValue = 500,
    mtImageRow = 85,
    mtIsUndead = True,
    mtMaxHealth = 2000,
    mtName = "Bonemaster",
    mtRemains = Just Bones,
    mtResistances = resistances [ResistChemical =% 20, ResistMental =% 100,
                                 ResistStun =% 20],
    mtSpeed = 1.5,
    mtSpells = [] } -- FIXME
getMonsterType Strigoi = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BladeAttack,
        maDamageCount = 10,
        maDamageRange = (1, 15),
        maEffects = [InflictCurse 0.05] }, baseMonsterAttack
      { maAppearance = WandAttack,
        maCriticalChance = 0.05,
        maDamageCount = 10,
        maDamageRange = (1, 15),
        maEffects = [KnockBack],
        maElement = FireDamage,
        maRange = Ranged 20 }],
    mtCoins = (150, 300),
    mtExperienceValue = 500,
    mtImageRow = 89,
    mtIsUndead = True,
    mtMaxHealth = 1500,
    mtName = "Strigoi",
    mtRemains = Just MediumBlood,
    mtResistances = resistances [ResistMental =% 100, ResistStun =% 40],
    mtSpeed = 3.0,
    mtSpells = [MetamorphIntoBat, TurnSelfInvisible 8] } -- TODO summon bats
getMonsterType VampireBat = (getMonsterType Strigoi)
  { mtAgility = 130,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maDamageCount = 10,
        maDamageRange = (1, 15),
        maEffects = [StealHealth 1] }],
    mtCanFly = True,
    mtImageRow = 51,
    mtName = "Vampire Bat",
    mtRemains = Just MediumBlood,
    mtSpeed = 4.0,
    mtSpells = [MetamorphIntoStrigoi],
    mtWalksFast = True }
getMonsterType Vhaegyst = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maCriticalChance = 0.8,
        maDamageCount = 8,
        maDamageRange = (1, 20) }],
    mtCoins = (120, 150),
    mtExperienceValue = 500,
    mtImageRow = 80,
    mtIsUndead = True,
    mtMaxHealth = 3000,
    mtName = "Vhaegyst",
    mtRemains = Just Bones,
    mtResistances = resistances [ResistCold =% 75, ResistMental =% 100,
                                 ResistStun =% 50],
    mtSpeed = 3.5,
    mtSpells = [FrostMissiles,
                Shell 2 15 5,
                SummonOne True 4 5 20 [Ghoul, Skeleton, Zombie],
                SummonOne True 2 5 20 [Ghoul, Skeleton, Zombie]] }
getMonsterType Rous = baseMonsterType
  { mtAgility = 45,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maCriticalChance = 0.2,
        maDamageCount = 10,
        maDamageRange = (1, 6),
        maEffects = [InflictWeakness 0.04] }],
    mtExperienceValue = 50,
    mtImageRow = 40,
    mtMaxHealth = 65,
    mtName = "Rous",
    mtRemains = Just MediumBlood,
    mtSpeed = 1.5 }
getMonsterType Unicorn = baseMonsterType
  { mtAgility = 35,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BluntAttack,
        maDamageCount = 9,
        maDamageRange = (1, 8),
        maEffects = [KnockBack] }],
    mtExperienceValue = 50,
    mtImageRow = 45,
    mtMaxHealth = 70,
    mtName = "Unicorn",
    mtRemains = Just MediumBlood,
    mtSpeed = 1.3 }
getMonsterType MonitorLizard = baseMonsterType { mtImageRow = 46 } -- TODO
getMonsterType Salamander = baseMonsterType { mtImageRow = 47 } -- TODO
getMonsterType IceLizard = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 10,
        maDamageRange = (1, 9) }],
    mtExperienceValue = 120,
    mtImageRow = 48,
    mtMaxHealth = 150,
    mtName = "Ice Lizard",
    mtRemains = Just MediumBlood,
    mtResistances = resistances [ResistFire -% 25, ResistCold =% 98,
                                 ResistStun =% 10],
    mtSpeed = 1.8,
    mtSpells = [IceBeam 3, IceBeam 4] }
getMonsterType Basilisk = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 15,
        maDamageRange = (1, 9) }],
    mtExperienceValue = 150,
    mtImageRow = 49,
    mtMaxHealth = 300,
    mtName = "Basilisk",
    mtRemains = Just MediumBlood,
    mtResistances = resistances [ResistFire =% 25, ResistStun =% 15],
    mtSpeed = 1.8,
    mtSpells = [] } -- FIXME petrify
getMonsterType Firefly = baseMonsterType { mtImageRow = 54 } -- TODO
getMonsterType LightningBug = baseMonsterType { mtImageRow = 55 } -- TODO
getMonsterType Roach = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 7,
        maDamageRange = (1, 7),
        maEffects = [SetField (PoisonCloud 15)] }],
    mtExperienceValue = 50,
    mtImageRow = 56,
    mtMaxHealth = 65,
    mtName = "Roach",
    mtRemains = Just SmallIchor,
    mtResistances = resistances [ResistChemical =% 50, ResistFire -% 15],
    mtSpeed = 1.8 }
getMonsterType Spider = baseMonsterType { mtImageRow = 57 } -- TODO
getMonsterType Mantis = baseMonsterType
  { mtAgility = 40,
    mtAttacks = [baseMonsterAttack
      { maAppearance = ClawAttack,
        maDamageCount = 8,
        maDamageRange = (1, 7) }],
    mtExperienceValue = 50,
    mtImageRow = 59,
    mtMaxHealth = 70,
    mtName = "Mantis",
    mtRemains = Just SmallIchor,
    mtResistances = resistances [Armor =% 15, ResistFire -% 15],
    mtSpeed = 1.8 }
getMonsterType Cobra = baseMonsterType
  { mtAgility = 60,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BiteAttack,
        maDamageCount = 4,
        maDamageRange = (1, 8),
        maEffects = [InflictPoison 2] }],
    mtExperienceValue = 50,
    mtImageRow = 67,
    mtMaxHealth = 50,
    mtName = "Cobra",
    mtRemains = Just MediumBlood,
    mtResistances = (ResistFire =% 8),
    mtSpeed = 2.2 }
getMonsterType TownWomanApron = townsperson { mtImageRow = 0 }
getMonsterType TownManApron = townsperson { mtImageRow = 1 }
getMonsterType TownManRed = townsperson { mtImageRow = 2 }
getMonsterType TownManYellow = townsperson { mtImageRow = 3 }
getMonsterType TownManBlue = townsperson { mtImageRow = 4 }
getMonsterType TownWomanGreen = townsperson { mtImageRow = 5 }
getMonsterType TownWomanPink = townsperson { mtImageRow = 6 }
getMonsterType TownWomanBlue = townsperson { mtImageRow = 7 }
getMonsterType TownWomanRed = townsperson { mtImageRow = 8 }
getMonsterType TownChildPurple = townsperson { mtImageRow = 9 }
getMonsterType TownChildBlue = townsperson { mtImageRow = 10 }
getMonsterType TownManGreen = townsperson { mtImageRow = 14 }
getMonsterType GuardLargeShield = (getMonsterType GuardSmallShield)
  { mtImageRow = 20,
    mtResistances = (Armor =% 40) }
getMonsterType GuardSmallShield = baseMonsterType
  { mtAgility = 30,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BladeAttack,
        maCriticalChance = 0.08,
        maDamageCount = 8,
        maDamageRange = (1, 6),
        maRange = Melee }],
    mtImageRow = 21,
    mtIsHuman = True,
    mtMaxHealth = 150,
    mtName = "Guard",
    mtRemains = Just MediumBlood,
    mtResistances = (Armor =% 30) }
getMonsterType GuardWoman = (getMonsterType GuardSmallShield)
  { mtImageRow = 22 }
getMonsterType GuardArcher = baseMonsterType
  { mtAgility = 30,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BowAttack,
        maCriticalChance = 0.03,
        maDamageCount = 8,
        maDamageRange = (1, 5),
        maRange = Ranged 5 }],
    mtImageRow = 26,
    mtIsHuman = True,
    mtMaxHealth = 150,
    mtName = "Guard",
    mtRemains = Just MediumBlood,
    mtResistances = (Armor =% 30) }
getMonsterType RogueIllusion0 = rogueillusion { mtImageRow = 100 }
getMonsterType RogueIllusion1 = rogueillusion { mtImageRow = 101 }
getMonsterType RogueIllusion2 = rogueillusion { mtImageRow = 102 }
getMonsterType RogueIllusion3 = rogueillusion { mtImageRow = 103 }
getMonsterType _ = baseMonsterType -- FIXME

-------------------------------------------------------------------------------

baseMonsterType :: MonsterType
baseMonsterType = MonsterType
  { mtAgility = 20,
    mtAttacks = [],
    mtCanFly = False,
    mtCoins = (1, 10),
    mtExperienceValue = 0,
    mtImageRow = 0,
    mtInherentInvisibility = NoInvisibility,
    mtIsDaemonic = False,
    mtIsHuman = False,
    mtIsUndead = False,
    mtMaxHealth = 100,
    mtName = "???",
    mtRemains = Nothing,
    mtResistances = nullResistances,
    mtSize = SizeSmall,
    mtSpeed = 1,
    mtSpells = [],
    mtWalksFast = False }

baseMonsterAttack :: MonsterAttack
baseMonsterAttack = MonsterAttack
  { maAppearance = BladeAttack,
    maCriticalChance = 0.1,
    maDamageCount = 1,
    maDamageRange = (1, 1),
    maElement = PhysicalDamage,
    maEffects = [],
    maRange = Melee }

townsperson :: MonsterType
townsperson = baseMonsterType
  { mtExperienceValue = 100,
    mtIsHuman = True,
    mtMaxHealth = 40,
    mtName = "Townsperson",
    mtRemains = Just MediumBlood }

rogueillusion :: MonsterType
rogueillusion = baseMonsterType
  { mtAgility = 80,
    mtAttacks = [baseMonsterAttack
      { maAppearance = BladeAttack,
        maDamageCount = 1,
        maDamageRange = (1, 3) }],
    mtCoins = (0, 0),
    mtIsHuman = True,
    mtMaxHealth = 100,
    mtName = "Illusion",
    mtResistances = (ResistMental =% 100),
    mtSpeed = 1.5 }

(=%) :: Resistance -> Double -> Resistances
(=%) resist n = TM.set resist ((100 - n) / 100) nullResistances

(-%) :: Resistance -> Double -> Resistances
(-%) resist n = resist =% negate n

resistances :: [Resistances] -> Resistances
resistances = foldl' (liftA2 (*)) nullResistances

-------------------------------------------------------------------------------
