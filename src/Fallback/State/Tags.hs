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

module Fallback.State.Tags
  (-- * Regions
   RegionTag(..), regionName, regionAreas,
   -- * Areas
   AreaTag(..), areaName, areaRegion,
   -- * Abilities
   AbilityTag(..), abilityName, abilityClassAndNumber, classAbility,
   -- * Feats
   FeatTag(..), featName,
   -- * Items
   ItemTag(..), WeaponItemTag(..), ArmorItemTag(..), AccessoryItemTag(..),
   PotionItemTag(..), InertItemTag(..),
   -- * Monsters
   MonsterTag(..), MonsterSpellTag(..))
where

import Data.Ix (Ix)

import qualified Fallback.Data.Bijection as Bij
import Fallback.State.Simple (AbilityNumber(..), CharacterClass(..))

-------------------------------------------------------------------------------

data RegionTag = Longvale | Svengaard | Tahariam | Bailagua | Emitsuibom
               | OtherFloors
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

regionName :: RegionTag -> String
regionName = show

regionAreas :: RegionTag -> [AreaTag]
regionAreas region = filter ((region ==) . areaRegion) [minBound .. maxBound]

-------------------------------------------------------------------------------

data AreaTag = Valhalla
             -- Longvale:
             | MountainPath | Corenglen
             -- Svengaard:
             | FrozenPass | Holmgare | SewerCaves | IcyFord | StoneBridge
             | Tragorda | WhistlingWoods | NorthernTundra
             | Duskwood | PerilousTrail
             | Icehold | Icehold2 | Icehold3
             -- Tahariam:
             | BurningMaze
             -- Bailagua:
             | Gazerpit
             -- Emitsuibom:
             | ArcaneLab | InnerLab
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

areaName :: AreaTag -> String
areaName MountainPath = "Mountain Path"
areaName FrozenPass = "Frozen Pass"
areaName SewerCaves = "Sewer Caves"
areaName IcyFord = "Icy Ford"
areaName StoneBridge = "Stone Bridge"
areaName WhistlingWoods = "Whistling Woods"
areaName NorthernTundra = "Northern Tundra"
areaName PerilousTrail = "Perilous Trail"
areaName BurningMaze = "Burning Maze"
areaName ArcaneLab = "Arcane Laboratory"
areaName InnerLab = "Inner Laboratory"
areaName tag = show tag

areaRegion :: AreaTag -> RegionTag
areaRegion Valhalla = OtherFloors
areaRegion MountainPath = Longvale
areaRegion Corenglen = Longvale
areaRegion FrozenPass = Svengaard
areaRegion Holmgare = Svengaard
areaRegion SewerCaves = Svengaard
areaRegion IcyFord = Svengaard
areaRegion StoneBridge = Svengaard
areaRegion Tragorda = Svengaard
areaRegion WhistlingWoods = Svengaard
areaRegion NorthernTundra = Svengaard
areaRegion Duskwood = Svengaard
areaRegion PerilousTrail = Svengaard
areaRegion Icehold = Svengaard
areaRegion Icehold2 = OtherFloors
areaRegion Icehold3 = OtherFloors
areaRegion BurningMaze = Tahariam
areaRegion Gazerpit = Bailagua
areaRegion ArcaneLab = Emitsuibom
areaRegion InnerLab = Emitsuibom

-------------------------------------------------------------------------------

data AbilityTag = AcidRain | AdrenalineRush | Alacrity | ArmorAura | Barrier
                | Bash | BeastCall | Blessing | Charm | Clarity | Conflagration
                | Critical | Cure | CurseShot | Detonate | Disjunction
                | Disruption | Dodge | Drain | EagleEye | Entangle | FinalBlow
                | FireShot | Fireball | Freeze | FrostShot | GroupHeal
                | Hardiness | Hasten | Healing | Hinder | IceBolts | Illusion
                | Immunity | Invisibility | Lightning | LucentShield
                | Luminaire | Parry | PoisonGas | PoisonShot | QuickAttack
                | Rainbow | Recuperation | Resistance | Restore | Revive
                | Riposte | SecondWind | Shieldbreaker | Shock | Smokescreen
                | Spellshatter | Stability | Subsume | Summon | Sunbeam
                | Valiance | Vanish | Vitriol
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

abilityName :: AbilityTag -> String
abilityName AcidRain = "Acid Rain"
abilityName AdrenalineRush = "Adrenaline Rush"
abilityName ArmorAura = "Armor Aura"
abilityName BeastCall = "Beast Call"
abilityName CurseShot = "Curse Shot"
abilityName EagleEye = "Eagle Eye"
abilityName FinalBlow = "Final Blow"
abilityName FireShot = "Fire Shot"
abilityName FrostShot = "Frost Shot"
abilityName GroupHeal = "Group Heal"
abilityName IceBolts = "Ice Bolts"
abilityName LucentShield = "Lucent Shield"
abilityName PoisonGas = "Poison Gas"
abilityName PoisonShot = "Poison Shot"
abilityName QuickAttack = "Quick Attack"
abilityName SecondWind = "Second Wind"
abilityName tag = show tag

abilityBijection :: Bij.Bijection (CharacterClass, AbilityNumber) AbilityTag
abilityBijection = Bij.make $ \tag ->
  case tag of
    (WarriorClass, Ability0) -> Bash
    (WarriorClass, Ability1) -> Valiance
    (WarriorClass, Ability2) -> SecondWind
    (WarriorClass, Ability3) -> Hardiness
    (WarriorClass, Ability4) -> Shieldbreaker
    (WarriorClass, Ability5) -> Parry
    (WarriorClass, Ability6) -> Spellshatter
    (WarriorClass, Ability7) -> Riposte
    (WarriorClass, Ability8) -> Critical
    (WarriorClass, Ability9) -> FinalBlow
    (RogueClass, Ability0) -> QuickAttack
    (RogueClass, Ability1) -> Dodge
    (RogueClass, Ability2) -> Vanish
    (RogueClass, Ability3) -> Immunity
    (RogueClass, Ability4) -> Smokescreen
    (RogueClass, Ability5) -> Stability
    (RogueClass, Ability6) -> Illusion
    (RogueClass, Ability7) -> Resistance
    (RogueClass, Ability8) -> Subsume
    (RogueClass, Ability9) -> Alacrity
    (HunterClass, Ability0) -> BeastCall
    (HunterClass, Ability1) -> FireShot
    (HunterClass, Ability2) -> Entangle
    (HunterClass, Ability3) -> Recuperation
    (HunterClass, Ability4) -> PoisonShot
    (HunterClass, Ability5) -> Charm
    (HunterClass, Ability6) -> EagleEye
    (HunterClass, Ability7) -> CurseShot
    (HunterClass, Ability8) -> Summon
    (HunterClass, Ability9) -> FrostShot
    (AlchemistClass, Ability0) -> Fireball
    (AlchemistClass, Ability1) -> Cure
    (AlchemistClass, Ability2) -> Conflagration
    (AlchemistClass, Ability3) -> PoisonGas
    (AlchemistClass, Ability4) -> ArmorAura
    (AlchemistClass, Ability5) -> Barrier
    (AlchemistClass, Ability6) -> Drain
    (AlchemistClass, Ability7) -> Detonate
    (AlchemistClass, Ability8) -> AdrenalineRush
    (AlchemistClass, Ability9) -> Rainbow
    (ClericClass, Ability0) -> Healing
    (ClericClass, Ability1) -> Blessing
    (ClericClass, Ability2) -> Disruption
    (ClericClass, Ability3) -> Restore
    (ClericClass, Ability4) -> Hinder
    (ClericClass, Ability5) -> Clarity
    (ClericClass, Ability6) -> Revive
    (ClericClass, Ability7) -> GroupHeal
    (ClericClass, Ability8) -> LucentShield
    (ClericClass, Ability9) -> Sunbeam
    (MagusClass, Ability0) -> Shock
    (MagusClass, Ability1) -> IceBolts
    (MagusClass, Ability2) -> Vitriol
    (MagusClass, Ability3) -> Invisibility
    (MagusClass, Ability4) -> Lightning
    (MagusClass, Ability5) -> Hasten
    (MagusClass, Ability6) -> Freeze
    (MagusClass, Ability7) -> Disjunction
    (MagusClass, Ability8) -> AcidRain
    (MagusClass, Ability9) -> Luminaire

abilityClassAndNumber :: AbilityTag -> (CharacterClass, AbilityNumber)
abilityClassAndNumber tag = Bij.getA abilityBijection tag

classAbility :: CharacterClass -> AbilityNumber -> AbilityTag
classAbility cls num = Bij.getB abilityBijection (cls, num)

-------------------------------------------------------------------------------

data FeatTag = Concentrate
             -- For astral weapons:
             | Offering | SolarFlare | Energize
             | StarShield | Zodiac | Imprison
             | TidalForce | Eclipse | LunarBeam
             | PulseOfLife | Avatar | AllCreation
             -- For bladed weapons:
             | Spincut | FireSpin
             -- For throwing stars:
             | Pierce | NeutronBomb
             -- For bows:
             | Longshot | Multishot
             -- For wands:
             | Glow | Amplify | Radiate | Resonate
  deriving (Eq, Show)

featName :: FeatTag -> String
featName SolarFlare = "Solar Flare"
featName StarShield = "Star Shield"
featName TidalForce = "Tidal Force"
featName LunarBeam = "Lunar Beam"
featName PulseOfLife = "Pulse of Life"
featName AllCreation = "All Creation"
featName FireSpin = "Fire Spin"
featName NeutronBomb = "Neutron Bomb"
featName tag = show tag

-------------------------------------------------------------------------------
{-
data ItemTag = Sunrod | Starspear | Moonbow | Lifeblade
             -- Light swords:
             | Dagger | Shortsword
             -- Heavy swords:
             | Longsword | Flameblade | Soultaker
             -- Polearms:
             | Quarterstaff | Spear
             -- Throwing stars:
             | ThrowingStar | RazorStar | NeutronStar
             -- Bows:
             | Shortbow | Longbow | CompositeBow | RainBow | TrineBow
             -- Wands:
             | SilverWand | JeweledRod | GoldenWand | DiamondRod
             | ChronosScepter
             -- Accessories:
             | TitanFists
             -- Potions:
             | HealingTincture | HealingPotion | HealingElixir
             | ManaPhilter | ManaElixir | Quintessence
             | Antidote | CuringPotion | MiracleElixir
             -- Keys:
             | IronKey | SilverKey
             -- Miscellaneous:
             | PerfectLily
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
-}
data ItemTag = WeaponItemTag WeaponItemTag
             | ArmorItemTag ArmorItemTag
             | AccessoryItemTag AccessoryItemTag
             | PotionItemTag PotionItemTag
             | InertItemTag InertItemTag
  deriving (Eq, Read, Show)

data WeaponItemTag = Sunrod | Starspear | Moonbow | Lifeblade
                   -- Light swords:
                   | Dagger | Shortsword
                   -- Heavy swords:
                   | Longsword | Flameblade | Soultaker
                   -- Polearms:
                   | Quarterstaff | Spear
                   -- Throwing stars:
                   | ThrowingStar | RazorStar | NeutronStar
                   -- Bows:
                   | Shortbow | Longbow | CompositeBow | RainBow | TrineBow
                   -- Wands:
                   | SilverWand | JeweledRod | GoldenWand | DiamondRod
                   | ChronosScepter
  deriving (Eq, Ord, Read, Show)

data ArmorItemTag = LeatherArmor | AdamantPlate
  deriving (Eq, Read, Show)

data AccessoryItemTag = GroundedAmulet | TitanFists
  deriving (Eq, Read, Show)

data PotionItemTag = HealingTincture | HealingPotion | HealingElixir
                   | ManaPhilter | ManaElixir | Quintessence
                   | Antidote | CuringPotion | MiracleElixir
  deriving (Eq, Read, Show)

data InertItemTag = IronKey | SilverKey
  deriving (Eq, Read, Show)

-------------------------------------------------------------------------------

data MonsterTag = Revenant | Revenantor | MasterRevenant
                -- Animals:
                | Wolf | DemonWolf
                -- Undead:
                | Ghoul | Ghast | Skeleton | Wight | Wraith | Zombie
                -- Bosses:
                | Vhaegyst
                -- Townspeople:
                | TownManApron | TownManRed | TownWomanBlue
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

data MonsterSpellTag = FireSpray | IceBomb

-------------------------------------------------------------------------------
