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

module Fallback.State.Simple where

import Data.Char (toLower)
import Data.Ix (Ix)
import Data.List (intercalate)

import Fallback.Data.Point
import Fallback.Data.TotalMap (TotalMap, makeTotalMap, tmAssocs)
import Fallback.Utility (ceilDiv)

-------------------------------------------------------------------------------
-- Abilities:

data AbilityNumber = Ability0 | Ability1 | Ability2 | Ability3 | Ability4
                   | Ability5 | Ability6 | Ability7 | Ability8 | Ability9
  deriving (Bounded, Enum, Eq, Ix, Ord)

data AbilityRank = Rank1 | Rank2 | Rank3
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

abilityRankNumber :: AbilityRank -> Int
abilityRankNumber Rank1 = 1
abilityRankNumber Rank2 = 2
abilityRankNumber Rank3 = 3

nextAbilityRank :: Maybe AbilityRank -> AbilityRank
nextAbilityRank Nothing = Rank1
nextAbilityRank (Just Rank1) = Rank2
nextAbilityRank (Just Rank2) = Rank3
nextAbilityRank (Just Rank3) = Rank3

abilityRankPlus :: Maybe AbilityRank -> Int -> Maybe AbilityRank
abilityRankPlus mbRank n =
  if n <= 0 || mbRank == Just maxBound then mbRank
  else abilityRankPlus (Just $ nextAbilityRank mbRank) (n - 1)

type PowerModifier = Double

-------------------------------------------------------------------------------
-- Attacks:

-- | Determines (along with the element of the attack) how an attack is
-- animated and what sound effects are used.  Does not affect e.g. the chances
-- of hitting or how damage is determined.  In particular, this does not
-- determine whether an attack is ranged or not for the purposes of e.g. the
-- Parry skill; the 'AttackRange' is used for that instead.
data AttackAppearance = BiteAttack | BladeAttack | BluntAttack | BowAttack
                      | BreathAttack | ClawAttack | ThrownAttack | WandAttack
  deriving (Eq)

-- | Determines the base damage element for an attack.  This is usually
-- 'PhysicalAttack', except for wands and e.g. monster breath weapons.
data AttackElement = AcidAttack | EnergyAttack | FireAttack | IceAttack
                   | PhysicalAttack
-- TODO eliminate AttackElement, replace with DamageType

-- | Describes an additional effect of an attack, such as bonus elemental
-- damage or a status effect.
data AttackEffect = DrainMana Double -- mana drained per base damage
                  | ExtraAcidDamage Double -- extra damage per base damage
                  | ExtraEnergyDamage Double -- extra damage per base damage
                  | ExtraFireDamage Double -- extra damage per base damage
                  | ExtraIceDamage Double -- extra damage per base damage
                  | InflictCurse Double -- duration (in rounds) per base damage
                  | InflictDaze Double
                  | InflictPoison Double -- poison per base damage
                  | InflictSlow Double -- duration (in rounds) per base damage
                  | InflictStun Double -- AP stun per base damage
                  | InflictWeakness Double -- duration (in rounds) per base dmg
                  | ReduceBuffs Double

-- | Determines the maximum range of an attack, and whether the attack counts
-- as \"melee\" or \"ranged\" for the purposes of e.g. meta-attacks or the
-- Parry skill.
data AttackRange = Melee | Ranged Int
  deriving (Eq)

-- | Translates an 'AttackRange' into a maximum radius.
rangeRadius :: AttackRange -> Int
rangeRadius Melee = 1
rangeRadius (Ranged r) = r

-- | Translates an 'AttackRange' into a maximum squared distance.
rangeSqDist :: AttackRange -> SqDist
rangeSqDist = ofRadius . rangeRadius

-------------------------------------------------------------------------------
-- Casting cost:

data CastingCost = AdrenalineCost Int
                 | FocusCost Int
                 | IngredientCost Ingredients
                 | ManaCost Int
                 | NoCost

data CostModifier = ZeroCost | OneThirdCost | NormalCost

modifyCost :: CostModifier -> CastingCost -> CastingCost
modifyCost ZeroCost _ = NoCost
modifyCost OneThirdCost cost =
  case cost of
    AdrenalineCost x -> AdrenalineCost (x `ceilDiv` 3)
    FocusCost x -> FocusCost (x `ceilDiv` 3)
    IngredientCost ing -> IngredientCost (fmap (`ceilDiv` 3) ing)
    ManaCost x -> ManaCost (x `ceilDiv` 3)
    NoCost -> NoCost
modifyCost NormalCost cost = cost

costDescription :: CastingCost -> String
costDescription (AdrenalineCost n) = "Cost: " ++ show n ++ " adrenaline"
costDescription (FocusCost n) = "Cost: " ++ show n ++ " focus"
costDescription (IngredientCost ings) =
  ("Cost: " ++) $ intercalate " + " $ map ingredientString $
  filter ((0 /=) . snd) $ tmAssocs ings
  where ingredientString (ing, n) =
          (if n == 1 then "" else show n ++ " ") ++
          map toLower (ingredientName ing)
costDescription (ManaCost n) = "Cost: " ++ show n ++ " mana"
costDescription NoCost = "No cost"

-------------------------------------------------------------------------------
-- Characters:

data CharacterClass = WarriorClass | RogueClass | HunterClass
                    | AlchemistClass | ClericClass | MagusClass
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

className :: CharacterClass -> String
className WarriorClass = "Warrior"
className RogueClass = "Rogue"
className HunterClass = "Hunter"
className AlchemistClass = "Alchemist"
className ClericClass = "Cleric"
className MagusClass = "Magus"

data CharacterAppearance = Appearance0 | Appearance1
                         | Appearance2 | Appearance3
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

data CharacterNumber = Character0 | Character1 | Character2 | Character3
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

-------------------------------------------------------------------------------

data CreatureSize = SizeSmall | SizeWide | SizeTall | SizeHuge
  deriving (Bounded, Enum, Eq, Ix, Ord)

sizeSize :: CreatureSize -> (Int, Int)
sizeSize SizeSmall = (1, 1)
sizeSize SizeWide = (2, 1)
sizeSize SizeTall = (1, 2)
sizeSize SizeHuge = (2, 2)

-------------------------------------------------------------------------------

-- | The type of damage dealt by some weapon or spell.
data DamageType = AcidDamage -- subject to chemical resistance and magic armor
                | ColdDamage -- subject to cold resistance and magic armor
                | EnergyDamage -- subject to energy resistance and magic armor
                | FireDamage -- subject to fire resistance and magic armor
                | MagicDamage -- subject to magic armor only
                | PhysicalDamage -- subject to physical armor only
                | RawDamage -- not subject to resistances at all

-------------------------------------------------------------------------------

data Difficulty = Casual | NotSoEasy | QuiteHard | Ruthless
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

difficultyName :: Difficulty -> String
difficultyName Casual = "Casual"
difficultyName NotSoEasy = "Not so Easy"
difficultyName QuiteHard = "Quite Hard"
difficultyName Ruthless = "Ruthless"

-------------------------------------------------------------------------------

data FaceDir = FaceLeft | FaceRight
  deriving (Eq, Read, Show)

deltaFaceDir :: IPoint -> FaceDir
deltaFaceDir (Point dx dy) =
  if dx < 0 then FaceLeft else
    if dx > 0 then FaceRight else if dy < 0 then FaceLeft else FaceRight

-------------------------------------------------------------------------------

data Field = BarrierWall Int -- duration in frames
           | FireWall Double -- base damage per round
           | IceWall Double -- base damage per round
           | PoisonCloud Double -- base poison damage per round
           | SmokeScreen Double -- half-life in frames
           | Webbing Double -- duration of entanglement, in rounds
  deriving (Eq, Read, Show)

-------------------------------------------------------------------------------

data Ingredient = AquaVitae | Naphtha | Limestone | Mandrake
                | Potash | Brimstone | DryIce | Quicksilver
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

type Ingredients = TotalMap Ingredient Int

ingredientName :: Ingredient -> String
ingredientName AquaVitae = "Aqua Vitae"
ingredientName DryIce = "Dry Ice"
ingredientName ing = show ing

ingredientDescription :: Ingredient -> String
ingredientDescription AquaVitae = "Aqua vitae, or ethanol, is a common\
  \ alchemical ingredient and the basis of many potions and tinctures."
ingredientDescription Naphtha = "Naphtha is an inflammable oil distilled from\
  \ pitch.  It is especially useful for setting things on fire."
ingredientDescription Limestone = "Limestone is a durable sedimentary rock. \
  \ Its chemical derivatives are highly reactive."
ingredientDescription Mandrake = "Mandrake is a plant root with many magical\
  \ properties.  It is the foundation of both life-saving potions and deadly\
  \ poisons."
ingredientDescription Potash = "Potash is a caustic, alkaline substance used\
  \ in several different alchemical formulae."
ingredientDescription Brimstone = "Brimstone is a foul-smelling mineral\
  \ compound and a critical ingredient in magical explosives."
ingredientDescription DryIce = "Dry ice is a frozen gas, kept magically cold. \
  \ It is a rare and powerful component in alchemical spells."
ingredientDescription Quicksilver = "Quicksilver is a flowing liquid metal. \
  \ It is very costly, and reserved for only the most powerful magic."

-- | Return the cost, in coins, of a single unit of the given ingredient.
ingredientCost :: Ingredient -> Integer
ingredientCost AquaVitae = 3
ingredientCost Naphtha = 5
ingredientCost Limestone = 6
ingredientCost Mandrake = 8
ingredientCost Potash = 10
ingredientCost Brimstone = 15
ingredientCost DryIce = 25
ingredientCost Quicksilver = 50

-------------------------------------------------------------------------------

data ItemSlot = CharWeaponSlot CharacterNumber
              | CharArmorSlot CharacterNumber
              | CharAccessorySlot CharacterNumber
              | PartyItemSlot Int

-------------------------------------------------------------------------------

data Resistance = Armor | ResistFire | ResistCold | ResistEnergy
                | ResistChemical | ResistMental | ResistStun
  deriving (Bounded, Enum, Eq, Ix, Ord)

type Resistances = TotalMap Resistance Double

-- | No resistances (or vulnerabilities) to anything.
nullResistances :: Resistances
nullResistances = makeTotalMap (const 1)

-------------------------------------------------------------------------------

data Stat = Strength | Agility | Intellect
  deriving (Bounded, Enum, Eq, Ix, Ord)

statDescription :: Stat -> String
statDescription Strength = "Strength determines your maximum health and the\
  \ power of your weapon attacks.  It also makes you more resistant to fire,\
  \ acid, and poison."
statDescription Agility = "Agility determines your ability to dodge enemy\
  \ attacks and hit with your own, and helps you to act more quickly in\
  \ combat.  It also helps you to avoid cold damage and being stunned."
statDescription Intellect = "Intellect determines your maximum mana/focus, the\
  \ power of your spells, and your chances of landing a critical hit with a\
  \ weapon.  It also helps you to resist energy damage and mental effects."

type Stats = TotalMap Stat Int

-- | Zero for all stats.
nullStats :: Stats
nullStats = makeTotalMap (const 0)

-------------------------------------------------------------------------------

data TerrainOpenness = TerrainHover | TerrainHoverSmoke | TerrainOpen
                     | TerrainSmoke | TerrainSolid | TerrainWindow
  deriving (Eq)

canSeeThrough :: TerrainOpenness -> Bool
canSeeThrough TerrainHover = True
canSeeThrough TerrainHoverSmoke = False
canSeeThrough TerrainOpen = True
canSeeThrough TerrainSmoke = False
canSeeThrough TerrainSolid = False
canSeeThrough TerrainWindow = True

cannotSeeThrough :: TerrainOpenness -> Bool
cannotSeeThrough = not . canSeeThrough

cannotFlyOver :: TerrainOpenness -> Bool
cannotFlyOver t = t == TerrainSolid || t == TerrainWindow

canWalkOn :: TerrainOpenness -> Bool
canWalkOn t = t == TerrainOpen || t == TerrainSmoke

cannotWalkOn :: TerrainOpenness -> Bool
cannotWalkOn = not . canWalkOn

-- | Change a 'TerrainOpenness' to one that cannot be seen through (but is
-- otherwise the same).
smokifyOpenness :: TerrainOpenness -> TerrainOpenness
smokifyOpenness TerrainHover = TerrainHoverSmoke
smokifyOpenness TerrainHoverSmoke = TerrainHoverSmoke
smokifyOpenness TerrainOpen = TerrainSmoke
smokifyOpenness TerrainSmoke = TerrainSmoke
smokifyOpenness TerrainSolid = TerrainSolid
smokifyOpenness TerrainWindow = TerrainSolid

-- | Change a 'TerrainOpenness' to one with the same visibility properties, but
-- that cannot be walked through (or flown over).  Essentially, everything
-- becomes either 'TerrainWindow' or 'TerrainSolid'.
solidifyOpenness :: TerrainOpenness -> TerrainOpenness
solidifyOpenness TerrainHover = TerrainWindow
solidifyOpenness TerrainHoverSmoke = TerrainSolid
solidifyOpenness TerrainOpen = TerrainWindow
solidifyOpenness TerrainSmoke = TerrainSolid
solidifyOpenness TerrainSolid = TerrainSolid
solidifyOpenness TerrainWindow = TerrainWindow

-------------------------------------------------------------------------------
