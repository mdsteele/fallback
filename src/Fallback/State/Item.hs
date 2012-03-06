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

module Fallback.State.Item
  (itemName, itemIconCoords, ItemValue(..), itemValue, showCoins,
   itemFullDescription,
   WeaponData(..), getWeaponData, unarmedWeaponData, DamageModifier(..),
   ArmorData(..), getArmorData, getAccessoryData,
   PotionAction(..), getPotionAction,
   Bonuses(..), nullBonuses, addBonuses, sumBonuses)
where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl', intercalate, partition)
import Data.Monoid

import Fallback.Data.TotalMap (TotalMap, makeTotalMap, tmAssocs, tmGet, tmSet)
import Fallback.State.Simple
import Fallback.State.Tags

-------------------------------------------------------------------------------

itemName :: ItemTag -> String
itemName (WeaponItemTag tag) = weaponName tag
itemName (ArmorItemTag tag) = armorName tag
itemName (AccessoryItemTag tag) = accessoryName tag
itemName (PotionItemTag tag) = potionName tag
itemName (InertItemTag tag) = inertName tag

weaponName :: WeaponItemTag -> String
weaponName ThrowingStar = "Throwing Star"
weaponName tag = show tag

armorName :: ArmorItemTag -> String
armorName LeatherArmor = "Leather Armor"
armorName AdamantPlate = "Adamant Plate"

accessoryName :: AccessoryItemTag -> String
accessoryName GroundedAmulet = "Grounded Amulet"
accessoryName TitanFists = "Titan Fists"

potionName :: PotionItemTag -> String
potionName HealingTincture = "Healing Tincture"
potionName tag = show tag -- FIXME

inertName :: InertItemTag -> String
inertName tag = show tag -- FIXME

-------------------------------------------------------------------------------

itemIconCoords :: ItemTag -> (Int, Int)
itemIconCoords (WeaponItemTag tag) = weaponIconCoords tag
itemIconCoords (ArmorItemTag tag) = armorIconCoords tag
itemIconCoords (AccessoryItemTag tag) = accessoryIconCoords tag
itemIconCoords (PotionItemTag tag) = potionIconCoords tag
itemIconCoords (InertItemTag tag) = inertIconCoords tag

weaponIconCoords :: WeaponItemTag -> (Int, Int)
weaponIconCoords Sunrod = (1, 0)
weaponIconCoords Starspear = (1, 1)
weaponIconCoords Moonbow = (1, 2)
weaponIconCoords Lifeblade = (1, 3)
weaponIconCoords ThrowingStar = (3, 0)
weaponIconCoords Dagger = (3, 3)
weaponIconCoords Shortsword = (3, 4)
weaponIconCoords Quarterstaff = (3, 5)
weaponIconCoords Shortbow = (3, 6)
weaponIconCoords _ = (0, 5) -- FIXME

armorIconCoords :: ArmorItemTag -> (Int, Int)
armorIconCoords _ = (0, 5) -- FIXME

accessoryIconCoords :: AccessoryItemTag -> (Int, Int)
accessoryIconCoords _ = (0, 5) -- FIXME

potionIconCoords :: PotionItemTag -> (Int, Int)
potionIconCoords HealingTincture = (2, 0)
potionIconCoords _ = (0, 5) -- FIXME

inertIconCoords :: InertItemTag -> (Int, Int)
inertIconCoords _ = (0, 5) -- FIXME

-------------------------------------------------------------------------------

-- | The value of an item, in coins, or an indication that the item cannot be
-- sold (or otherwise discarded).
data ItemValue = CanSell Integer | CannotSell

itemValue :: ItemTag -> ItemValue
itemValue (WeaponItemTag tag) = weaponValue tag
itemValue (ArmorItemTag tag) = armorValue tag
itemValue (AccessoryItemTag tag) = accessoryValue tag
itemValue (PotionItemTag tag) = potionValue tag
itemValue (InertItemTag tag) = inertValue tag

weaponValue :: WeaponItemTag -> ItemValue
weaponValue Sunrod = CannotSell
weaponValue Starspear = CannotSell
weaponValue Moonbow = CannotSell
weaponValue Lifeblade = CannotSell
weaponValue ThrowingStar = CanSell 40
weaponValue Dagger = CanSell 30
weaponValue Shortsword = CanSell 60
weaponValue Quarterstaff = CanSell 20
weaponValue Shortbow = CanSell 60
weaponValue _ = CanSell 10 -- FIXME

armorValue :: ArmorItemTag -> ItemValue
armorValue _ = CanSell 10 -- FIXME

accessoryValue :: AccessoryItemTag -> ItemValue
accessoryValue TitanFists = CanSell 660
accessoryValue _ = CanSell 10 -- FIXME

potionValue :: PotionItemTag -> ItemValue
potionValue HealingTincture = CanSell 50
potionValue _ = CanSell 10 -- FIXME

inertValue :: InertItemTag -> ItemValue
inertValue _ = CannotSell

showCoins :: Integer -> String
showCoins coins = showCommaSep coins ++
                  if coins == 1 then " coin" else " coins"

showCommaSep :: (Integral a) => a -> String
showCommaSep = reverse . commaSep . reverse . show
  where commaSep (a:b:c:d:xs) = a : b : c : ',' : commaSep (d : xs)
        commaSep xs = xs

-------------------------------------------------------------------------------

itemFullDescription :: ItemTag -> String
itemFullDescription tag =
  ("{b}" ++ itemName tag ++ "{_}\n" ++ value ++ "\n" ++
   itemSubDescription tag ++ "\n" ++ itemFlavorText tag)
  where value = case itemValue tag of
                  CannotSell -> "Cannot sell"
                  CanSell coins -> "Value: " ++ showCoins coins

itemSubDescription :: ItemTag -> String
itemSubDescription (WeaponItemTag tag) = wdSubDesc (getWeaponData tag)
itemSubDescription (ArmorItemTag tag) = adSubDesc (getArmorData tag)
itemSubDescription (AccessoryItemTag tag) = adSubDesc (getAccessoryData tag)
itemSubDescription (PotionItemTag tag) = potionSubDesc (getPotionAction tag)
itemSubDescription (InertItemTag _) = ""

wdSubDesc :: WeaponData -> String
wdSubDesc wd = attackDesc ++ bonusesSubDesc (wdBonuses wd) ++ featsDesc ++
               usableSubDesc (wdUsableBy wd) where
  attackDesc = "Attack (" ++ rangeDesc ++ "):\n" ++ indent damageLine ++
               concatMap (indent . effectLine) (wdEffects wd)
  damageLine = let (lo, hi) = wdDamageRange wd
               in show (lo * wdDamageBonus wd) ++ "-" ++
                  show (hi * wdDamageBonus wd) ++ " " ++
                  (case wdElement wd of
                     AcidAttack -> "acid "
                     EnergyAttack -> "energy "
                     FireAttack -> "fire "
                     IceAttack -> "ice "
                     PhysicalAttack -> "") ++ "damage + " ++
                  show lo ++ "-" ++ show hi ++ " per 5 strength\n"
  rangeDesc = case wdRange wd of Melee -> "melee"
                                 Ranged r -> "range " ++ show r
  featsDesc = if null (wdFeats wd) then ""
              else "Feats:\n" ++ concatMap featLine (wdFeats wd)
  featLine featTag = indent (featName featTag ++ "\n")
  effectLine (DrainMana x) = "Drains mana" ++ effectNum x
  effectLine (ExtraAcidDamage p) = showSignedPercent p ++ " acid damage\n"
  effectLine (ExtraEnergyDamage p) = showSignedPercent p ++ " energy damage\n"
  effectLine (ExtraFireDamage p) = showSignedPercent p ++ " fire damage\n"
  effectLine (ExtraIceDamage p) = showSignedPercent p ++ " ice damage\n"
  effectLine (InflictCurse x) = "Curses target" ++ effectNum x
  effectLine (InflictPoison x) = "Poisons target" ++ effectNum x
  effectLine _ = "FIXME some effect\n"
  effectNum x = " (effect " ++ show (round (100 * x) :: Int) ++ ")\n"

adSubDesc :: ArmorData -> String
adSubDesc ad = bonusesSubDesc (adBonuses ad) ++ usableSubDesc (adUsableBy ad)

potionSubDesc :: PotionAction -> String
potionSubDesc (HealAction n) = "Restores " ++ show n ++ " health\n"

bonusesSubDesc :: Bonuses -> String
bonusesSubDesc bonuses = if null bonusLines then "" else
                           "Bonuses:\n" ++ concatMap indent bonusLines where
  bonusLines = (map statLine $ filter ((0 /=) . snd) $ tmAssocs $
                bonusStats bonuses) ++
               (map resistLine $ filter ((1 /=) . snd) $ tmAssocs $
                bonusResistances bonuses)
  statLine (stat, delta) = showSignedInt delta ++ " " ++ statName stat ++ "\n"
  resistLine (resist, mult) = showSignedPercent (1 - mult) ++ " " ++
                              resistName resist ++ "\n"
  statName Strength = "strength"
  statName Agility = "agility"
  statName Intellect = "intellect"
  resistName Armor = "armor"
  resistName ResistFire = "fire resistance"
  resistName ResistCold = "cold resistance"
  resistName ResistEnergy = "energy resistance"
  resistName ResistChemical = "poison/acid resistance"
  resistName ResistMental = "mental resistance"
  resistName ResistStun = "stun resistance"

usableSubDesc :: TotalMap CharacterClass Bool -> String
usableSubDesc usable =
  case partition (flip tmGet usable) [minBound .. maxBound] of
    ([], _) -> "Not usable by anyone."
    (_, []) -> "Usable by anyone.\n"
    ([a], _) -> "Usable by " ++ plural a ++ " only.\n"
    (_, [a]) -> "Not usable by " ++ plural a ++ ".\n"
    ([a, b], _) -> "Usable by " ++ plural a ++ " and " ++ plural b ++ ".\n"
    (_, [a, b]) -> "Not usable by " ++ plural a ++ " or " ++ plural b ++ ".\n"
    (xs, _) -> "Usable by " ++ intercalate ", " (map plural $ init xs) ++
               ", and " ++ plural (last xs) ++ ".\n"
  where
    plural WarriorClass = "warriors"
    plural RogueClass = "rogues"
    plural HunterClass = "hunters"
    plural AlchemistClass = "alchemists"
    plural ClericClass = "clerics"
    plural MagusClass = "magi"

itemFlavorText :: ItemTag -> String
itemFlavorText (WeaponItemTag tag) = weaponFlavorText tag
itemFlavorText (ArmorItemTag tag) = armorFlavorText tag
itemFlavorText (AccessoryItemTag tag) = accessoryFlavorText tag
itemFlavorText (PotionItemTag tag) = potionFlavorText tag
itemFlavorText (InertItemTag tag) = inertFlavorText tag

weaponFlavorText :: WeaponItemTag -> String
weaponFlavorText Sunrod = "A magical wand of unearthly power.  It bears an\
  \ inscription: \"Sunrod, the bane of all undead.\""
weaponFlavorText Starspear = "A magical spear of unearthly power.  It bears an\
  \ inscription: \"Starspear, the guard that hell hath dread.\""
weaponFlavorText Moonbow = "A magical bow of unearthly power.  It bears an\
  \ inscription: \"Moonbow, the master of the night.\""
weaponFlavorText Lifeblade = "A magical sword of unearthly power.  It bears an\
  \ inscription: \"Lifeblade, the fount of strength and light.\""
weaponFlavorText _ = "FIXME a weapon"

armorFlavorText :: ArmorItemTag -> String
armorFlavorText AdamantPlate = "Full plate armor, made of the toughest stuff\
  \ imaginable."
armorFlavorText _ = "FIXME some armor"

accessoryFlavorText :: AccessoryItemTag -> String
accessoryFlavorText _ = "FIXME an accessory"

potionFlavorText :: PotionItemTag -> String
potionFlavorText _ = "FIXME a potion"

inertFlavorText :: InertItemTag -> String
inertFlavorText _ = "FIXME something inert"

-------------------------------------------------------------------------------

data WeaponData = WeaponData
  { wdAppearance :: AttackAppearance,
    wdBonuses :: Bonuses,
    wdDamageBonus :: Int,
    wdDamageRange :: (Int, Int),
    wdEffects :: [AttackEffect],
    wdElement :: AttackElement,
    wdFeats :: [FeatTag],
    wdRange :: AttackRange,
    wdUsableBy :: TotalMap CharacterClass Bool,
    wdVsUndead :: DamageModifier }

getWeaponData :: WeaponItemTag -> WeaponData
getWeaponData Sunrod = WeaponData
  { wdAppearance = WandAttack,
    wdBonuses = nullBonuses,
    wdDamageBonus = 40,
    wdDamageRange = (1, 6),
    wdEffects = [],
    wdElement = FireAttack,
    wdFeats = [Offering, SolarFlare, Energize],
    wdRange = Ranged 6,
    wdUsableBy = usableByAll,
    wdVsUndead = InstantKill }
getWeaponData Starspear = WeaponData
  { wdAppearance = MeleeAttack,
    wdBonuses = sumBonuses [Armor +% 20, Intellect += 10],
    wdDamageBonus = 5,
    wdDamageRange = (1, 8),
    wdEffects = [],
    wdElement = PhysicalAttack,
    wdFeats = [StarShield, Zodiac, Imprison],
    wdRange = Melee,
    wdUsableBy = usableByAll,
    wdVsUndead = NormalDamage }
getWeaponData Moonbow = WeaponData
  { wdAppearance = BowAttack,
    wdBonuses = (Agility += 10),
    wdDamageBonus = 10,
    wdDamageRange = (1, 7),
    wdEffects = [ExtraIceDamage 0.5],
    wdElement = PhysicalAttack,
    wdFeats = [TidalForce, Eclipse, LunarBeam],
    wdRange = Ranged 5,
    wdUsableBy = usableByAll,
    wdVsUndead = NormalDamage }
getWeaponData Lifeblade = WeaponData
  { wdAppearance = MeleeAttack,
    wdBonuses = (Strength += 10),
    wdDamageBonus = 6,
    wdDamageRange = (1, 10),
    wdEffects = [],
    wdElement = PhysicalAttack,
    wdFeats = [PulseOfLife, Avatar, AllCreation],
    wdRange = Melee,
    wdUsableBy = usableByAll,
    wdVsUndead = DoubleDamage }
getWeaponData Dagger = WeaponData
  { wdAppearance = MeleeAttack,
    wdBonuses = nullBonuses,
    wdDamageBonus = 1,
    wdDamageRange = (1, 4),
    wdEffects = [],
    wdElement = PhysicalAttack,
    wdFeats = [Concentrate, Glow, Radiate], -- FIXME
    wdRange = Melee,
    wdUsableBy = usableByAll,
    wdVsUndead = NormalDamage }
getWeaponData ThrowingStar = WeaponData
  { wdAppearance = ThrownAttack,
    wdBonuses = nullBonuses,
    wdDamageBonus = 2,
    wdDamageRange = (1, 4),
    wdEffects = [],
    wdElement = PhysicalAttack,
    wdFeats = [Pierce],
    wdRange = Ranged 4,
    wdUsableBy = roguesOnly,
    wdVsUndead = NormalDamage }
getWeaponData RazorStar = WeaponData
  { wdAppearance = ThrownAttack,
    wdBonuses = nullBonuses,
    wdDamageBonus = 3,
    wdDamageRange = (1, 5),
    wdEffects = [],
    wdElement = PhysicalAttack,
    wdFeats = [Pierce],
    wdRange = Ranged 5,
    wdUsableBy = roguesOnly,
    wdVsUndead = NormalDamage }
getWeaponData NeutronStar = WeaponData
  { wdAppearance = ThrownAttack,
    wdBonuses = nullBonuses,
    wdDamageBonus = 5,
    wdDamageRange = (1, 5),
    wdEffects = [InflictCurse 0.03],
    wdElement = PhysicalAttack,
    wdFeats = [Pierce, NeutronBomb],
    wdRange = Ranged 5,
    wdUsableBy = roguesOnly,
    wdVsUndead = NormalDamage }
getWeaponData Shortbow = WeaponData
  { wdAppearance = BowAttack,
    wdBonuses = nullBonuses,
    wdDamageBonus = 2,
    wdDamageRange = (1, 4),
    wdEffects = [],
    wdElement = PhysicalAttack,
    wdFeats = [Amplify, Resonate], -- FIXME
    wdRange = Ranged 4,
    wdUsableBy = archersOnly,
    wdVsUndead = NormalDamage }
getWeaponData _ = unarmedWeaponData -- FIXME

-- | 'WeaponData' for unarmed attacks.
unarmedWeaponData :: WeaponData
unarmedWeaponData = WeaponData
  { wdAppearance = MeleeAttack,
    wdBonuses = nullBonuses,
    wdDamageBonus = 1,
    wdDamageRange = (1, 3),
    wdEffects = [],
    wdElement = PhysicalAttack,
    wdFeats = [Concentrate],
    wdRange = Melee,
    wdUsableBy = makeTotalMap (const True),
    wdVsUndead = NormalDamage }

data DamageModifier = ZeroDamage | NormalDamage | DoubleDamage | InstantKill

-------------------------------------------------------------------------------

data ArmorData = ArmorData
  { adBonuses :: Bonuses,
    adUsableBy :: TotalMap CharacterClass Bool }

getArmorData :: ArmorItemTag -> ArmorData
getArmorData AdamantPlate = ArmorData
  { adBonuses = sumBonuses [Armor +% 60, ResistStun +% 80],
    adUsableBy = warriorsOnly }
getArmorData _ = ArmorData -- FIXME
  { adBonuses = nullBonuses, adUsableBy = usableByAll }

getAccessoryData :: AccessoryItemTag -> ArmorData
getAccessoryData GroundedAmulet = ArmorData
  { adBonuses = sumBonuses [ResistStun +% 15],
    adUsableBy = usableByAll }
getAccessoryData TitanFists = ArmorData
  { adBonuses = sumBonuses [Strength += 10, Armor +% 8],
    adUsableBy = warriorsOnly }

-------------------------------------------------------------------------------

data PotionAction = HealAction Int -- TODO add more

getPotionAction :: PotionItemTag -> PotionAction
getPotionAction HealingTincture = HealAction 100
getPotionAction _ = error "FIXME getPotionAction"

-------------------------------------------------------------------------------

-- | Represents the bonuses conferred onto a character for equipping an item.
data Bonuses = Bonuses
  { bonusResistances :: Resistances,
    bonusStats :: Stats }

instance Monoid Bonuses where
  mempty = nullBonuses
  mappend = addBonuses
  mconcat = sumBonuses

-- | No bonuses of any kind.
nullBonuses :: Bonuses
nullBonuses = Bonuses
  { bonusResistances = nullResistances,
    bonusStats = nullStats }

addBonuses :: Bonuses -> Bonuses -> Bonuses
addBonuses b1 b2 = Bonuses
  { bonusResistances = (*) <$> bonusResistances b1 <*> bonusResistances b2,
    bonusStats = (+) <$> bonusStats b1 <*> bonusStats b2 }

sumBonuses :: [Bonuses] -> Bonuses
sumBonuses = foldl' addBonuses nullBonuses

-------------------------------------------------------------------------------
-- Private:

usableByAll :: TotalMap CharacterClass Bool
usableByAll = makeTotalMap (const True)

archersOnly :: TotalMap CharacterClass Bool
archersOnly = makeTotalMap $ \c -> c == RogueClass || c == HunterClass

warriorsOnly :: TotalMap CharacterClass Bool
warriorsOnly = makeTotalMap (== WarriorClass)

roguesOnly :: TotalMap CharacterClass Bool
roguesOnly = makeTotalMap (== RogueClass)

(+=) :: Stat -> Int -> Bonuses
stat += n = Bonuses { bonusResistances = nullResistances,
                      bonusStats = tmSet stat n nullStats }

(+%) :: Resistance -> Double -> Bonuses
resist +% n = Bonuses
  { bonusResistances = tmSet resist (1 - n / 100) nullResistances,
    bonusStats = nullStats }

showSignedInt :: Int -> String
showSignedInt i = if i < 0 then show i else '+' : show i

showSignedPercent :: Double -> String
showSignedPercent p =
  (if p < 0 then "-" else "+") ++ show (round (100 * abs p) :: Int) ++ "%"

indent :: String -> String
indent = ("   " ++)

-------------------------------------------------------------------------------
