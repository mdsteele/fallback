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
   ItemTag(..), allItemTags, WeaponItemTag(..), ArmorItemTag(..),
   AccessoryItemTag(..), PotionItemTag(..), InertItemTag(..),
   -- * Monsters
   MonsterTag(..),
   -- * Quests
   QuestTag(..), questName, questDescription)
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
             | FrozenPass | Holmgare | SewerCaves | PerilousRoad
             | StoneBridge | Tragorda | WhistlingWoods | IcyConfluence
             | Marata | IronMine | NorthernTundra | Duskwood
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
areaName PerilousRoad = "Perilous Road"
areaName StoneBridge = "Stone Bridge"
areaName WhistlingWoods = "Whistling Woods"
areaName IcyConfluence = "Icy Confluence"
areaName IronMine = "Iron Mine"
areaName NorthernTundra = "Northern Tundra"
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
areaRegion PerilousRoad = Svengaard
areaRegion StoneBridge = Svengaard
areaRegion Tragorda = Svengaard
areaRegion WhistlingWoods = Svengaard
areaRegion IcyConfluence = Svengaard
areaRegion Marata = Svengaard
areaRegion IronMine = Svengaard
areaRegion NorthernTundra = Svengaard
areaRegion Duskwood = Svengaard
areaRegion Icehold = Svengaard
areaRegion Icehold2 = OtherFloors
areaRegion Icehold3 = OtherFloors
areaRegion BurningMaze = Tahariam
areaRegion Gazerpit = Bailagua
areaRegion ArcaneLab = Emitsuibom
areaRegion InnerLab = Emitsuibom

-------------------------------------------------------------------------------

data AbilityTag = AcidRain | AdrenalineRush | Alacrity | ArmorAura | Backstab
                | Barrier | Bash | BeastCall | Blessing | Charm | Clarity
                | Conflagration | Critical | Cure | CurseShot | Detonate
                | Disjunction | Disruption | Dodge | Drain | EagleEye
                | Entangle | FinalBlow | FireShot | Fireball | Freeze
                | FrostShot | GroupHeal | Hardiness | Hasten | Healing
                | Hinder | IceBolts | Illusion | Immunity | Invisibility
                | Lightning | LucentShield | Luminaire | Parry | PoisonGas
                | PoisonShot | QuickAttack | Rainbow | Recuperation | Restore
                | Revive | Riposte | RopeDart | SecondWind | Shieldbreaker
                | Shock | SmokeBomb | Spellshatter | Subsume | Summon | Sunbeam
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
abilityName RopeDart = "Rope Dart"
abilityName SecondWind = "Second Wind"
abilityName SmokeBomb = "Smoke Bomb"
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
    (RogueClass, Ability1) -> Backstab
    (RogueClass, Ability2) -> Vanish
    (RogueClass, Ability3) -> SmokeBomb
    (RogueClass, Ability4) -> Immunity
    (RogueClass, Ability5) -> RopeDart
    (RogueClass, Ability6) -> Dodge
    (RogueClass, Ability7) -> Subsume
    (RogueClass, Ability8) -> Illusion
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
             | StarShield | Zodiac | Banish
             | TidalForce | Eclipse | LunarBeam
             | PulseOfLife | Avatar | AllCreation
             -- For melee weapons:
             | Cleave | Envenom | FireSpin | JumpSlash | JumpStrike | Rampage
             | Spincut | SweepSlash | Whirlwind
             -- For throwing stars:
             | NeutronBomb | Pierce
             -- For bows:
             | Longshot | Shortshot | SprayArrows | TripleTap
             -- For wands:
             | Glow | Amplify | Radiate | Resonate | TimeStop | Catalyze
  deriving (Eq, Show)

featName :: FeatTag -> String
featName SolarFlare = "Solar Flare"
featName StarShield = "Star Shield"
featName TidalForce = "Tidal Force"
featName LunarBeam = "Lunar Beam"
featName PulseOfLife = "Pulse of Life"
featName AllCreation = "All Creation"
featName FireSpin = "Fire Spin"
featName JumpSlash = "Jump Slash"
featName JumpStrike = "Jump Strike"
featName SweepSlash = "Sweep Slash"
featName NeutronBomb = "Neutron Bomb"
featName SprayArrows = "Spray Arrows"
featName TripleTap = "Triple Tap"
featName TimeStop = "Time Stop"
featName tag = show tag

-------------------------------------------------------------------------------

data ItemTag = WeaponItemTag WeaponItemTag
             | ArmorItemTag ArmorItemTag
             | AccessoryItemTag AccessoryItemTag
             | PotionItemTag PotionItemTag
             | InertItemTag InertItemTag
  deriving (Eq, Read, Show)

allItemTags :: [ItemTag]
allItemTags =
  map WeaponItemTag [minBound .. maxBound] ++
  map ArmorItemTag [minBound .. maxBound] ++
  map AccessoryItemTag [minBound .. maxBound] ++
  map PotionItemTag [minBound .. maxBound] ++
  map InertItemTag [minBound .. maxBound]

data WeaponItemTag = Sunrod | Starspear | Moonbow | Lifeblade
                   -- Light swords:
                   | Dagger | Shortsword | DaemonicDagger
                   -- Heavy swords:
                   | Longsword | Flameblade | Soultaker
                   -- Polearms:
                   | Quarterstaff | Voulge | Spear | Glaive | Ranseur
                   | MixingPole
                   -- Throwing stars:
                   | ThrowingStar | RazorStar | NeutronStar
                   -- Bows:
                   | Shortbow | Longbow | AssassinsBow | CompositeBow | Bowser
                   | RainBow | TrineBow | ArtemisBow
                   -- Wands:
                   | SilverWand | JeweledRod | GoldenWand | DiamondRod
                   | ChronosScepter
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data ArmorItemTag = AdamantPlate | BrawlersTunic | CottonShirt | DeadeyeJacket
                  | LeatherArmor | IronMail | IronPlate | SteelMail
                  | SteelPlate | SwampLeather
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data AccessoryItemTag = Alkamulet | ArmorRing | EverwarmPendant | FightersRing
                      | GlovesOfTanth | GroundedCharm | IcyNecklace
                      | JeweledPin | LeatherGloves | MedalOfValor
                      | MercuricRing | ShieldRing | TrogloHelmet | WizardHat
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data PotionItemTag = HealingTincture | HealingPotion | HealingElixir
                   | ManaPhilter | ManaElixir | Quintessence
                   | Antidote | CuringPotion | MiracleElixir
                   -- Food:
                   | Grapes | Pineapple | Bread | Cheese | Carrot | Fish | Meat
                   | Eggs | Radish | Apple | Orange | Strawberry | Pear | Lemon
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data InertItemTag = IronKey | BrassKey
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-------------------------------------------------------------------------------

data MonsterTag = Revenant | Revenantor | MasterRevenant
                -- Animals:
                | Wolf | DemonWolf | CaveBat
                -- Undead:
                | Ghoul | Ghast | Invisighoul | Skeleton | Wight | Wraith
                | Zombie
                -- Bosses:
                | Vampire | Dactylid | Vhaegyst
                -- Townspeople:
                | TownManApron | TownWomanApron | TownManRed | TownWomanBlue
                -- Town guards:
                | GuardSmallShield | GuardLargeShield | GuardArcher
                -- Rogue illusions:
                | RogueIllusion0 | RogueIllusion1
                | RogueIllusion2 | RogueIllusion3
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

-------------------------------------------------------------------------------

data QuestTag = FindAdventure | SaveCorenglen
              -- Svengaard:
              | ReclaimSunrod | CaptureSophia | HelpSophiaEscape
              | IronForGregor | ClearPerilousRoad | ClearIronMine
              | StealIronKey | DryIceForLucca | BookForSageBora
              -- Tahariam:
              | ReclaimStarspear
              -- Bailagua:
              | ReclaimMoonbow
              -- Emitsuibom:
              | ReclaimLifeblade
  deriving (Eq, Ord, Read, Show)

questName :: QuestTag -> String
questName FindAdventure = "Seek out Adventure"
questName ReclaimSunrod = "Astral IV: Find the Sunrod"
questName CaptureSophia = "Bring Sophia to Holmgare"
questName HelpSophiaEscape = "Help Sophia Escape"
questName DryIceForLucca = "Dry Ice for Lucca"
questName ReclaimStarspear = "Astral III: Find the Starspear"
questName ReclaimMoonbow = "Astral II: Find the Moonbow"
questName ReclaimLifeblade = "Astral I: Find the Lifeblade"
questName _ = "FIXME"

questDescription :: QuestTag -> String
questDescription FindAdventure = "You are a group of brand-new adventurers,\
  \ venturing out into the world to seek your fortune.  After stocking up on\
  \ supplies and hiking through the wild, your first stop is in the village of\
  \ Corenglen, in Longvale.  You hope that someone there, or nearby, will have\
  \ some kind of quest in mind for a plucky band of adventurers such as\
  \ yourselves."
questDescription ReclaimSunrod = "According to Jessica, your last great\
  \ adventure was to journey through Svengaard, defeat the Dread Ur-Lich\
  \ Vhaegyst, and claim the Sunrod.\n\n\
  \For context, a lich is what happens when an incredibly powerful and evil\
  \ archmagus uses dark magic to become a skeletal, undead, eldrich horror,\
  \ thus unnaturally prolonging their existence so that they can continue to\
  \ wreak destruction and suffering far beyond their mortal lifespan. \
  \ Considering your complete inexperience as adventurers, asking you to go\
  \ defeat a lich, even a \"normal\" lich, would be a bit like asking you to\
  \ jump over the moon and then survive the fall back down."
questDescription CaptureSophia = "Sophia Vrell has run away from the village\
  \ of Holmgare, and may be in danger.  The villagers have begged you to find\
  \ her, and Mayor Jarmir has offered you a reward if you can bring her back\
  \ to them safe and sound."
questDescription DryIceForLucca = "Lucca, the apothecary in Tragorda, needs a\
  \ piece of dry ice for potion she'd like to make.  If you can find any of\
  \ this alchemical ingredient somewhere in Svengaard and give it to her, she\
  \ will make the potion and give some of it to you."
questDescription ReclaimStarspear = "According to Jessica, your third great\
  \ adventure was to journey into Tahariam, defeat the Daemon Lord Kuriyos,\
  \ and claim the Starspear.\n\n\
  \Daemons are creatures from another, infernal plane of existance.  They are\
  \ extremely dangerous, and the big ones are nigh unkillable.  Fortunately,\
  \ daemons can generally only reach the world of humans if explicitly\
  \ summoned; unfortunately, there's no shortage of evil wizards foolish\
  \ enough to do so.  If a daemon {i}lord{_} is lurking around here, that is\
  \ {i}very{_} bad news for everyone, and if you are to supposed to fight it\
  \ yourselves, you are almost certainly going to die."
questDescription ReclaimMoonbow = "According to Jessica, your second great\
  \ adventure was to journey to Bailagua, defeat the Eye Queen Straeyeng, and\
  \ claim the Moonbow.\n\n\
  \You have no idea what an \"eye queen\" is, and Jessica didn't explain."
questDescription ReclaimLifeblade = "According to Jessica, your very first\
  \ great adventure was to journey through Emitsuibom, defeat the archmagus\
  \ Uhnkanae, and claim the Lifeblade.\n\n\
  \You've been hearing a lot about your victory over Uhnkanae during your\
  \ \"later\" adventures, and from what you can tell he is a serious force to\
  \ be reckoned with.  On the one hand, he is still just one human--not as\
  \ inherently terrifying as some of your previous foes--but on the other hand\
  \ you will be facing him without any of the astral weapons, four\
  \ only-moderately-experienced adventurers against one of the most powerful\
  \ magi in the world.  Your success is not particularly likely."
questDescription _ = "FIXME"

-------------------------------------------------------------------------------
