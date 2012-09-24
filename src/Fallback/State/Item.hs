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
  (itemName, itemIconCoords, ItemValue(..), itemValue, itemCost, showCoins,
   itemFullDescription,
   WeaponData(..), getWeaponData, unarmedWeaponData, DamageModifier(..),
   ArmorData(..), getArmorData, getAccessoryData,
   PotionAction(..), getPotionAction,
   Bonuses(..), nullBonuses, addBonuses, sumBonuses)
where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl', intercalate, partition)

import qualified Fallback.Data.TotalMap as TM
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
weaponName KitchenKnife = "Kitchen Knife"
weaponName NastyKnife = "Nasty Knife"
weaponName DaemonicDagger = "Daemonic Dagger"
weaponName MagnumGreatsword = "Magnum Greatsword"
weaponName FallingStar = "Falling Star"
weaponName IronSpear = "Iron Spear"
weaponName MixingPole = "Mixing Pole"
weaponName ThrowingStar = "Throwing Star"
weaponName RazorStar = "Razor Star"
weaponName NeutronStar = "Neutron Star"
weaponName AssassinsBow = "Assassin's Bow"
weaponName CompositeBow = "Composite Bow"
weaponName RainBow = "Rain Bow"
weaponName BronzeBow = "Bronze Bow"
weaponName ArtemisBow = "Artemis Bow"
weaponName SilverWand = "Silver Wand"
weaponName JeweledRod = "Jeweled Rod"
weaponName GoldenWand = "Golden Wand"
weaponName DiamondRod = "Diamond Rod"
weaponName ChronosScepter = "Chronos Scepter"
weaponName tag = show tag

armorName :: ArmorItemTag -> String
armorName AdamantPlate = "Adamant Plate"
armorName BrawlersTunic = "Brawler's Tunic"
armorName CottonShirt = "Cotton Shirt"
armorName DeadeyeJacket = "Deadeye Jacket"
armorName LeatherArmor = "Leather Armor"
armorName IronMail = "Iron Mail"
armorName IronPlate = "Iron Plate"
armorName SteelMail = "Steel Mail"
armorName SteelPlate = "Steel Plate"
armorName SwampLeather = "Swamp Leather"

accessoryName :: AccessoryItemTag -> String
accessoryName Alkamulet = "Alkamulet"
accessoryName ArmorRing = "Armor Ring"
accessoryName EverwarmPendant = "Everwarm Pendant"
accessoryName FightersRing = "Fighter's Ring"
accessoryName GlovesOfTanth = "Gloves of Tanth"
accessoryName GroundedCharm = "Grounded Charm"
accessoryName IcyNecklace = "Icy Necklace"
accessoryName JeweledPin = "Jeweled Pin"
accessoryName LeatherGloves = "Leather Gloves"
accessoryName MedalOfValor = "Medal of Valor"
accessoryName MercuricRing = "Mercuric Ring"
accessoryName ShieldRing = "Shield Ring"
accessoryName TrogloHelmet = "Troglo Helmet"
accessoryName WizardHat = "Wizard Hat"
accessoryName WizardsRing = "Wizard's Ring"

potionName :: PotionItemTag -> String
potionName HealingTincture = "Healing Tincture"
potionName HealingPotion = "Healing Potion"
potionName HealingElixir = "Healing Elixer"
potionName ManaPhilter = "Mana Philter"
potionName ManaPotion = "Mana Potion"
potionName ManaElixir = "Mana Elixer"
potionName FocusStone = "Focus Stone"
potionName CuringPotion = "Curing Potion"
potionName MiracleElixir = "Miracle Elixer"
potionName PhoenixFeather = "Phoenix Feather"
potionName tag = show tag

inertName :: InertItemTag -> String
inertName IronKey = "Iron Key"
inertName BrassKey = "Brass Key"

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
weaponIconCoords ThrowingStar = (1, 4)
weaponIconCoords RazorStar = (1, 5)
weaponIconCoords NeutronStar = (1, 6)
weaponIconCoords Dagger = (14, 0)
weaponIconCoords Shortsword = (15, 0)
weaponIconCoords Longsword = (15, 1)
weaponIconCoords DaemonicDagger = (14, 6)
weaponIconCoords Quarterstaff = (13, 0)
weaponIconCoords Voulge = (13, 1)
weaponIconCoords Spear = (13, 2)
weaponIconCoords IronSpear = (13, 3)
weaponIconCoords Glaive = (13, 4)
weaponIconCoords Ranseur = (13, 5)
weaponIconCoords Naginata = (13, 6)
weaponIconCoords MixingPole = (13, 7)
weaponIconCoords Shortbow = (12, 0)
weaponIconCoords Longbow = (12, 1)
weaponIconCoords AssassinsBow = (12, 2)
weaponIconCoords CompositeBow = (12, 3)
weaponIconCoords Bowser = (12, 4)
weaponIconCoords RainBow = (12, 5)
weaponIconCoords BronzeBow = (12, 6)
weaponIconCoords ArtemisBow = (12, 7)
weaponIconCoords Flail = (16, 3)
weaponIconCoords Mace = (16, 4)
weaponIconCoords Warhammer = (16, 5)
weaponIconCoords Morningstar = (16, 6)
weaponIconCoords FallingStar = (16, 7)
weaponIconCoords SilverWand = (17, 0)
weaponIconCoords JeweledRod = (17, 1)
weaponIconCoords GoldenWand = (17, 2)
weaponIconCoords DiamondRod = (17, 3)
weaponIconCoords ChronosScepter = (17, 4)
weaponIconCoords _ = (0, 5) -- FIXME

armorIconCoords :: ArmorItemTag -> (Int, Int)
armorIconCoords AdamantPlate = (9, 7)
armorIconCoords BrawlersTunic = (10, 0)
armorIconCoords CottonShirt = (9, 0)
armorIconCoords DeadeyeJacket = (10, 2)
armorIconCoords LeatherArmor = (9, 3)
armorIconCoords IronMail = (9, 4)
armorIconCoords IronPlate = (9, 5)
armorIconCoords SteelMail = (10, 4)
armorIconCoords SteelPlate = (10, 5)
armorIconCoords SwampLeather = (10, 3)

accessoryIconCoords :: AccessoryItemTag -> (Int, Int)
accessoryIconCoords Alkamulet = (6, 3)
accessoryIconCoords ArmorRing = (7, 1)
accessoryIconCoords EverwarmPendant = (6, 0)
accessoryIconCoords FightersRing = (7, 2)
accessoryIconCoords GlovesOfTanth = (8, 6)
accessoryIconCoords GroundedCharm = (6, 1)
accessoryIconCoords IcyNecklace = (6, 2)
accessoryIconCoords LeatherGloves = (8, 4)
accessoryIconCoords JeweledPin = (6, 4)
accessoryIconCoords MedalOfValor = (6, 5)
accessoryIconCoords MercuricRing = (7, 4)
accessoryIconCoords ShieldRing = (7, 0)
accessoryIconCoords TrogloHelmet = (8, 0)
accessoryIconCoords WizardHat = (8, 3)
accessoryIconCoords WizardsRing = (7, 3)

potionIconCoords :: PotionItemTag -> (Int, Int)
potionIconCoords HealingTincture = (4, 0)
potionIconCoords HealingPotion = (4, 1)
potionIconCoords HealingElixir = (4, 2)
potionIconCoords ManaPhilter = (4, 3)
potionIconCoords ManaPotion = (4, 4)
potionIconCoords ManaElixir = (4, 5)
potionIconCoords FocusStone = (4, 6)
potionIconCoords Quintessence = (4, 7)
potionIconCoords Antidote = (5, 0)
potionIconCoords CuringPotion = (5, 1)
potionIconCoords MiracleElixir = (5, 2)
potionIconCoords Regenelixer = (5, 3)
potionIconCoords Epinephrine = (5, 5)
potionIconCoords PhoenixFeather = (5, 7)
potionIconCoords Grapes = (2, 0)
potionIconCoords Pineapple = (2, 1)
potionIconCoords Bread = (2, 2)
potionIconCoords Cheese = (2, 3)
potionIconCoords Carrot = (2, 4)
potionIconCoords Fish = (2, 5)
potionIconCoords Meat = (2, 6)
potionIconCoords Eggs = (2, 7)
potionIconCoords Radish = (3, 0)
potionIconCoords Apple = (3, 1)
potionIconCoords Orange = (3, 2)
potionIconCoords Strawberry = (3, 3)
potionIconCoords Pear = (3, 4)
potionIconCoords Lemon = (3, 5)
potionIconCoords Mushroom = (3, 6)

inertIconCoords :: InertItemTag -> (Int, Int)
inertIconCoords IronKey = (18, 0)
inertIconCoords BrassKey = (18, 1)

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
accessoryValue _ = CanSell 10 -- FIXME

potionValue :: PotionItemTag -> ItemValue
potionValue HealingTincture = CanSell 50
potionValue HealingPotion = CanSell 180
potionValue ManaPhilter = CanSell 200
potionValue Antidote = CanSell 35
potionValue Grapes = CanSell 15
potionValue Pineapple = CanSell 20
potionValue Bread = CanSell 15
potionValue Cheese = CanSell 15
potionValue Carrot = CanSell 12
potionValue Fish = CanSell 15
potionValue Meat = CanSell 15
potionValue Eggs = CanSell 10
potionValue Radish = CanSell 12
potionValue Apple = CanSell 15
potionValue Orange = CanSell 18
potionValue Strawberry = CanSell 10
potionValue Pear = CanSell 15
potionValue Lemon = CanSell 18
potionValue Mushroom = CanSell 12
potionValue _ = CanSell 10 -- FIXME

inertValue :: InertItemTag -> ItemValue
inertValue _ = CannotSell

showCoins :: Integer -> String
showCoins coins = showCommaSep coins ++
                  if coins == 1 then " coin" else " coins"

showCommaSep :: (Integral a, Show a) => a -> String
showCommaSep = reverse . commaSep . reverse . show
  where commaSep (a:b:c:d:xs) = a : b : c : ',' : commaSep (d : xs)
        commaSep xs = xs

itemCost :: ItemTag -> Integer
itemCost tag = case itemValue tag of
                 CanSell cost -> cost
                 CannotSell -> 0

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
               concatMap (indent . effectLine) (wdEffects wd) ++ damageModsDesc
  damageLine = let (lo, hi) = wdDamageRange wd
               in show (lo * wdDamageBonus wd) ++ "-" ++
                  show (hi * wdDamageBonus wd) ++ dtypeName (wdElement wd) ++
                  " damage + " ++ show lo ++ "-" ++ show hi ++
                  " per 5 strength\n"
  rangeDesc = case wdRange wd of Melee -> "melee"
                                 Ranged r -> "range " ++ show r
  featsDesc = if null (wdFeats wd) then ""
              else "Feats:\n" ++ concatMap featLine (wdFeats wd)
  featLine featTag = indent (featName featTag ++ "\n")
  effectLine (DrainMana x) = "Drains mana" ++ effectNum x
  effectLine (ExtraDamage dtype p) =
    showSignedPercent p ++ dtypeName dtype ++ " damage\n"
  effectLine (InflictCurse x) = "Curses target" ++ effectNum x
  effectLine (InflictMental eff x) =
    mentalEffectName eff ++ " target" ++ effectNum x
  effectLine (InflictPoison x) = "Poisons target" ++ effectNum x
  effectLine (InflictSlow x) = "Slows target" ++ effectNum x
  effectLine (InflictStun x) = "Stuns target" ++ effectNum (x * 100)
  effectLine (InflictWeakness x) = "Weakens target's armor" ++ effectNum x
  effectLine KnockBack = "Knocks target back\n"
  effectLine PurgeInvisibility = "Purges invisibility from target\n"
  effectLine (StealHealth x) = "Steals health " ++ effectNum x
  effectLine _ = "FIXME some effect\n"
  effectNum x = " (effect " ++ show (round (100 * x) :: Int) ++ ")\n"
  damageModsDesc = concatMap damageModDesc $
                   [(wdVsDaemonic, "daemonic creatures"),
                    (wdVsHuman, "humans"), (wdVsUndead, "the undead")]
  damageModDesc (fn, name) =
    case fn wd of
      ZeroDamage -> indent $ "Cannot damage " ++ name ++ "\n"
      HalfDamage -> indent $ "Half damage against " ++ name ++ "\n"
      NormalDamage -> ""
      DoubleDamage -> indent $ "Double damage against " ++ name ++ "\n"
      InstantKill -> indent $ "Instant death to " ++ name ++ "\n"
  dtypeName AcidDamage = " acid"
  dtypeName ColdDamage = " ice"
  dtypeName EnergyDamage = " energy"
  dtypeName FireDamage = " fire"
  dtypeName MagicDamage = " magical"
  dtypeName PhysicalDamage = ""
  dtypeName RawDamage = " raw"
  mentalEffectName Dazed = "Dazes"
  mentalEffectName Confused = "Confuses"
  mentalEffectName Charmed = "Charms"

adSubDesc :: ArmorData -> String
adSubDesc ad = bonusesSubDesc (adBonuses ad) ++ usableSubDesc (adUsableBy ad)

potionSubDesc :: PotionAction -> String
potionSubDesc (RestoreHealth h) = "Restores " ++ show h ++ " health\n"
potionSubDesc (RestoreMana m) = "Restores " ++ show m ++ " mana\n"
potionSubDesc (RestoreHealthAndMana h m) =
  "Restores " ++ show h ++ " health and " ++ show m ++ " mana\n"
potionSubDesc _ = "FIXME"

bonusesSubDesc :: Bonuses -> String
bonusesSubDesc bonuses = if null bonusLines then "" else
                           "Bonuses:\n" ++ concatMap indent bonusLines where
  bonusLines = (map statLine $ filter ((0 /=) . snd) $ TM.assocs $
                bonusStats bonuses) ++
               multLine bonusAdrenalineMultiplier " adrenaline gain\n" ++
               multLine bonusFistsDamageMultiplier " bare-handed damage\n" ++
               multLine bonusMeleeWeaponDamageMultiplier
                        " melee weapon damage\n" ++
               multLine bonusRangedWeaponDamageMultiplier
                        " ranged weapon damage\n" ++
               multLine bonusPowerModifier " power for special abilities\n" ++
               multLine bonusSpeedMultiplier " speed\n" ++
               (map resistLine $ filter ((1 /=) . snd) $ TM.assocs $
                bonusResistances bonuses)
  statLine (stat, delta) = showSignedInt delta ++ " " ++ statName stat ++ "\n"
  multLine fn desc =
    let mult = fn bonuses
    in if mult == 1 then [] else [showSignedPercent (mult - 1) ++ desc]
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

usableSubDesc :: TM.TotalMap CharacterClass Bool -> String
usableSubDesc usable =
  case partition (flip TM.get usable) [minBound .. maxBound] of
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
potionFlavorText ManaPhilter = "This vial of blue liquid smells of citrus. \
  \ One gulp is all it takes to leave you feeling energized."
potionFlavorText Grapes = "The seeded kind, in this case.  Don't worry; no one\
  \ here minds if you spit."
potionFlavorText Pineapple = "Unless you live in a tropical climate, you'll\
  \ need a hothouse to grow pineapples; but delicious things are worth the\
  \ effort."
potionFlavorText Bread = "If beer is \"liquid bread,\" does that mean that\
  \ bread is solid beer?"
potionFlavorText Cheese = "Hey, look: you {i}have{_} in fact got any cheese at\
  \ all!"
potionFlavorText Carrot = "They say that carrots help you to see in the dark. \
  \ So does a torch, but carrots taste better."
potionFlavorText Fish = "\"Brain food,\" they call it; and it's delicious when\
  \ fried."
potionFlavorText Meat = "With all the protein you could ever want."
potionFlavorText Eggs = "Eggs are basically cheese that comes from chickens."
potionFlavorText Radish = "Or is it a turnip?  Hard to tell."
potionFlavorText Apple = "Apples are notable for being especially difficult to\
  \ compare to oranges."
potionFlavorText Orange = "Oranges are perhaps the most aptly named of all\
  \ fruits."
potionFlavorText Strawberry = "Strawberries are blushing because their seeds\
  \ are on the outside."
potionFlavorText Pear = "Pears are like apples, but they're shaped wrong and\
  \ they taste different."
potionFlavorText Lemon = "If life gives you lemons, make lemonade.  Ooh, or\
  \ pie!  Lemon cream pie is delicious."
potionFlavorText Mushroom = "There's a fungus among us."
potionFlavorText _ = "FIXME a potion"

inertFlavorText :: InertItemTag -> String
inertFlavorText IronKey = "A heavy iron key that you claimed from the daemon\
  \ Dactylid in Svengaard."
inertFlavorText BrassKey = "A polished brass key that you found in Icehold."

-------------------------------------------------------------------------------

data WeaponData = WeaponData
  { wdAppearance :: AttackAppearance,
    wdBonuses :: Bonuses,
    wdDamageBonus :: Int,
    wdDamageRange :: (Int, Int),
    wdEffects :: [AttackEffect],
    wdElement :: DamageType,
    wdFeats :: [FeatTag],
    wdRange :: AttackRange,
    wdUsableBy :: TM.TotalMap CharacterClass Bool,
    wdVsDaemonic :: DamageModifier,
    wdVsHuman :: DamageModifier,
    wdVsUndead :: DamageModifier }

data DamageModifier = ZeroDamage | HalfDamage | NormalDamage | DoubleDamage
                    | InstantKill

getWeaponData :: WeaponItemTag -> WeaponData
getWeaponData Sunrod = baseWeaponData
  { wdAppearance = WandAttack,
    wdDamageBonus = 20,
    wdDamageRange = (1, 6),
    wdElement = FireDamage,
    wdFeats = [Offering, SolarFlare, Energize],
    wdRange = Ranged 6,
    wdVsUndead = InstantKill }
getWeaponData Starspear = baseWeaponData
  { wdAppearance = BladeAttack,
    wdBonuses = sumBonuses [Armor +% 30, Intellect += 10],
    wdDamageBonus = 5,
    wdDamageRange = (1, 8),
    wdFeats = [StarShield, Zodiac, Banish],
    wdRange = Melee,
    wdVsDaemonic = DoubleDamage }
getWeaponData Moonbow = baseWeaponData
  { wdAppearance = BowAttack,
    wdBonuses = (Agility += 10),
    wdDamageBonus = 7,
    wdDamageRange = (1, 7),
    wdEffects = [ExtraDamage ColdDamage 0.5],
    wdFeats = [TidalForce, Eclipse, LunarBeam],
    wdRange = Ranged 5 }
getWeaponData Lifeblade = baseWeaponData
  { wdAppearance = BladeAttack,
    wdBonuses = (Strength += 10),
    wdDamageBonus = 6,
    wdDamageRange = (1, 10),
    wdFeats = [PulseOfLife, Avatar, AllCreation],
    wdRange = Melee,
    wdVsUndead = DoubleDamage }
getWeaponData Dagger = baseWeaponData
  { wdAppearance = BladeAttack,
    wdDamageBonus = 1,
    wdDamageRange = (1, 4),
    wdFeats = [Cripple],
    wdRange = Melee }
getWeaponData Shortsword = baseWeaponData
  { wdAppearance = BladeAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 5),
    wdFeats = [Assassinate, Rampage, Pierce], -- FIXME
    wdRange = Melee }
getWeaponData Longsword = baseWeaponData
  { wdAppearance = BladeAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 6),
    wdFeats = [JumpSlash, JumpStrike], -- FIXME
    wdRange = Melee }
getWeaponData DaemonicDagger = baseWeaponData
  { wdAppearance = BladeAttack,
    wdDamageBonus = 5,
    wdDamageRange = (1, 4),
    wdFeats = [Concentrate], -- FIXME
    wdRange = Melee,
    wdUsableBy = anyoneExcept [ClericClass],
    wdVsHuman = DoubleDamage,
    wdVsUndead = HalfDamage }
getWeaponData Quarterstaff = baseWeaponData
  { wdAppearance = BluntAttack,
    wdDamageBonus = 1,
    wdDamageRange = (1, 4),
    wdFeats = [Concentrate], -- FIXME
    wdRange = Melee }
getWeaponData Voulge = baseWeaponData
  { wdAppearance = BladeAttack,
    wdDamageBonus = 1,
    wdDamageRange = (1, 5),
    wdFeats = [Concentrate], -- FIXME
    wdRange = Melee,
    wdUsableBy = only [WarriorClass, AlchemistClass] }
getWeaponData MixingPole = baseWeaponData
  { wdAppearance = BladeAttack,
    wdDamageBonus = 3,
    wdDamageRange = (1, 7),
    wdFeats = [Catalyze], -- FIXME
    wdRange = Melee,
    wdUsableBy = only [AlchemistClass] }
getWeaponData ThrowingStar = baseWeaponData
  { wdAppearance = ThrownAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 4),
    wdFeats = [Pierce],
    wdRange = Ranged 4,
    wdUsableBy = only [RogueClass] }
getWeaponData RazorStar = baseWeaponData
  { wdAppearance = ThrownAttack,
    wdDamageBonus = 3,
    wdDamageRange = (1, 5),
    wdFeats = [Pierce],
    wdRange = Ranged 5,
    wdUsableBy = only [RogueClass] }
getWeaponData NeutronStar = baseWeaponData
  { wdAppearance = ThrownAttack,
    wdDamageBonus = 5,
    wdDamageRange = (1, 5),
    wdEffects = [InflictCurse 0.03],
    wdFeats = [Pierce, NeutronBomb],
    wdRange = Ranged 5,
    wdUsableBy = only [RogueClass] }
getWeaponData Shortbow = baseWeaponData
  { wdAppearance = BowAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 4),
    wdFeats = [Shortshot],
    wdRange = Ranged 4,
    wdUsableBy = anyoneExcept [AlchemistClass] }
getWeaponData Longbow = baseWeaponData
  { wdAppearance = BowAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 5),
    wdFeats = [Longshot],
    wdRange = Ranged 5,
    wdUsableBy = only [WarriorClass, RogueClass, HunterClass] }
getWeaponData AssassinsBow = baseWeaponData
  { wdAppearance = BowAttack,
    wdDamageBonus = 3,
    wdDamageRange = (1, 5),
    wdFeats = [Longshot, Assassinate],
    wdRange = Ranged 6,
    wdUsableBy = only [RogueClass, HunterClass] }
getWeaponData CompositeBow = baseWeaponData
  { wdAppearance = BowAttack,
    wdDamageBonus = 3,
    wdDamageRange = (1, 5),
    wdFeats = [Shortshot, Multishot],
    wdRange = Ranged 4,
    wdUsableBy = only [WarriorClass, RogueClass, HunterClass] }
getWeaponData Bowser = baseWeaponData
  { wdAppearance = BowAttack,
    wdDamageBonus = 2,
    wdDamageRange = (2, 5),
    wdFeats = [Pierce, Multishot],
    wdRange = Ranged 4,
    wdUsableBy = only [WarriorClass, RogueClass, HunterClass] }
getWeaponData RainBow = baseWeaponData
  { wdAppearance = BowAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 6),
    wdFeats = [HealingRain],
    wdRange = Ranged 5,
    wdUsableBy = only [HunterClass] }
getWeaponData BronzeBow = baseWeaponData
  { wdAppearance = BowAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 6),
    wdEffects = [ExtraDamage FireDamage 0.1],
    wdFeats = [ExplosiveArrow],
    wdRange = Ranged 5,
    wdUsableBy = only [HunterClass] }
getWeaponData ArtemisBow = baseWeaponData
  { wdAppearance = BowAttack,
    wdDamageBonus = 4,
    wdDamageRange = (2, 6),
    wdEffects = [ExtraDamage EnergyDamage 0.15],
    wdFeats = [SprayArrows, ThunderArrow],
    wdRange = Ranged 5,
    wdUsableBy = only [HunterClass] }
getWeaponData SilverWand = baseWeaponData
  { wdAppearance = WandAttack,
    wdDamageBonus = 1,
    wdDamageRange = (1, 4),
    wdElement = ColdDamage,
    wdFeats = [Glow],
    wdRange = Ranged 4,
    wdUsableBy = manaUsersOnly }
getWeaponData JeweledRod = baseWeaponData
  { wdAppearance = WandAttack,
    wdDamageBonus = 1,
    wdDamageRange = (1, 4),
    wdElement = EnergyDamage,
    wdFeats = [Amplify],
    wdRange = Ranged 4,
    wdUsableBy = castersOnly }
getWeaponData GoldenWand = baseWeaponData
  { wdAppearance = WandAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 4),
    wdElement = FireDamage,
    wdFeats = [Radiate],
    wdRange = Ranged 4,
    wdUsableBy = manaUsersOnly }
getWeaponData DiamondRod = baseWeaponData
  { wdAppearance = WandAttack,
    wdDamageBonus = 2,
    wdDamageRange = (1, 4),
    wdElement = EnergyDamage,
    wdFeats = [Resonate],
    wdRange = Ranged 4,
    wdUsableBy = manaUsersOnly }
getWeaponData ChronosScepter = baseWeaponData
  { wdAppearance = WandAttack,
    wdDamageBonus = 3,
    wdDamageRange = (1, 5),
    wdElement = MagicDamage,
    wdFeats = [TimeStop],
    wdRange = Ranged 3,
    wdUsableBy = castersOnly }
getWeaponData _ = unarmedWeaponData -- FIXME

-- | 'WeaponData' for unarmed attacks.
unarmedWeaponData :: WeaponData
unarmedWeaponData = baseWeaponData
  { wdAppearance = BluntAttack,
    wdDamageBonus = 1,
    wdDamageRange = (1, 3),
    wdFeats = [Concentrate],
    wdRange = Melee }

baseWeaponData :: WeaponData
baseWeaponData = WeaponData
  { wdAppearance = BladeAttack,
    wdBonuses = nullBonuses,
    wdDamageBonus = 0,
    wdDamageRange = (1, 1),
    wdEffects = [],
    wdElement = PhysicalDamage,
    wdFeats = [],
    wdRange = Melee,
    wdUsableBy = anyone,
    wdVsDaemonic = NormalDamage,
    wdVsHuman = NormalDamage,
    wdVsUndead = NormalDamage }

-------------------------------------------------------------------------------

data ArmorData = ArmorData
  { adBonuses :: Bonuses,
    adUsableBy :: TM.TotalMap CharacterClass Bool }

getArmorData :: ArmorItemTag -> ArmorData
getArmorData AdamantPlate = ArmorData
  { adBonuses = sumBonuses [Agility -= 10, Armor +% 60, ResistStun +% 80],
    adUsableBy = only [WarriorClass] }
getArmorData BrawlersTunic = ArmorData
  { adBonuses = sumBonuses [Armor +% 2, meleeDamageMult 1.25],
    adUsableBy = anyone }
getArmorData CottonShirt = ArmorData
  { adBonuses = sumBonuses [Armor +% 3, ResistStun +% 2],
    adUsableBy = anyone }
getArmorData DeadeyeJacket = ArmorData
  { adBonuses = sumBonuses [Agility += 6, Armor +% 20, ResistStun +% 15,
                            rangedDamageMult 1.08],
    adUsableBy = only [HunterClass] }
getArmorData LeatherArmor = ArmorData
  { adBonuses = sumBonuses [Agility -= 1, Armor +% 15, ResistStun +% 10],
    adUsableBy = anyoneExcept [MagusClass] }
getArmorData IronMail = ArmorData
  { adBonuses = sumBonuses [Agility -= 4, Armor +% 20, ResistStun +% 20],
    adUsableBy = anyoneExcept [MagusClass] }
getArmorData IronPlate = ArmorData
  { adBonuses = sumBonuses [Agility -= 8, Armor +% 40, ResistStun +% 50],
    adUsableBy = only [WarriorClass, AlchemistClass] }
getArmorData SteelMail = ArmorData
  { adBonuses = sumBonuses [Agility -= 4, Armor +% 30, ResistStun +% 22],
    adUsableBy = anyoneExcept [MagusClass] }
getArmorData SteelPlate = ArmorData
  { adBonuses = sumBonuses [Agility -= 7, Armor +% 45, ResistStun +% 50],
    adUsableBy = only [WarriorClass, AlchemistClass] }
getArmorData SwampLeather = ArmorData
  { adBonuses = sumBonuses [Agility -= 1, Armor +% 15, ResistChemical +% 20,
                            ResistStun +% 10],
    adUsableBy = anyoneExcept [ClericClass, MagusClass] }

getAccessoryData :: AccessoryItemTag -> ArmorData
getAccessoryData Alkamulet = ArmorData
  { adBonuses = sumBonuses [ResistChemical +% 25],
    adUsableBy = anyone }
getAccessoryData ArmorRing = ArmorData
  { adBonuses = sumBonuses [Armor +% 20, speedMult 0.95],
    adUsableBy = anyone }
getAccessoryData EverwarmPendant = ArmorData
  { adBonuses = (ResistCold +% 25),
    adUsableBy = anyone }
getAccessoryData FightersRing = ArmorData
  { adBonuses = sumBonuses [meleeDamageMult 1.1, rangedDamageMult 1.1],
    adUsableBy = anyoneExcept [MagusClass] }
getAccessoryData GlovesOfTanth = ArmorData
  { adBonuses = sumBonuses [Armor +% 6, Strength += 8, fistsDamageMult 3],
    adUsableBy = anyoneExcept [MagusClass] }
getAccessoryData GroundedCharm = ArmorData
  { adBonuses = sumBonuses [ResistEnergy +% 25],
    adUsableBy = anyone }
getAccessoryData IcyNecklace = ArmorData
  { adBonuses = sumBonuses [ResistFire +% 25],
    adUsableBy = anyone }
getAccessoryData JeweledPin = ArmorData
  { adBonuses = sumBonuses [ResistFire +% 10, ResistCold +% 10,
                            ResistEnergy +% 10],
    adUsableBy = anyone }
getAccessoryData LeatherGloves = ArmorData
  { adBonuses = (Armor +% 2),
    adUsableBy = anyone }
getAccessoryData MedalOfValor = ArmorData
  { adBonuses = sumBonuses [adrenMult 1.2, ResistStun +% 10],
    adUsableBy = anyone }
getAccessoryData MercuricRing = ArmorData
  { adBonuses = sumBonuses [Strength -= 5, speedMult 1.15],
    adUsableBy = anyone }
getAccessoryData ShieldRing = ArmorData
  { adBonuses = (Armor +% 10),
    adUsableBy = anyone }
getAccessoryData TrogloHelmet = ArmorData
  { adBonuses = sumBonuses [Strength += 12, Intellect -= 12, Armor +% 10],
    adUsableBy = anyoneExcept [ClericClass, MagusClass] }
getAccessoryData WizardHat = ArmorData
  { adBonuses = sumBonuses [Intellect += 10, Armor +% 2],
    adUsableBy = manaUsersOnly }
getAccessoryData WizardsRing = ArmorData
  { adBonuses = powerModifier 1.15,
    adUsableBy = anyone }

-------------------------------------------------------------------------------

-- TODO: Make potions have a list of actions; separate RestoreHealthAndMana
data PotionAction = BoostAdrenaline Int
                  | RestoreFocus Int
                  | RestoreHealth Int -- TODO add more
                  | RestoreMana Int
                  | RestoreHealthAndMana Int Int
                  | RegenHealthAndFullyRestoreMojo Int
                  | ReviveCharacter Int

getPotionAction :: PotionItemTag -> PotionAction
getPotionAction HealingTincture = RestoreHealth 100
getPotionAction HealingPotion = RestoreHealth 300
getPotionAction HealingElixir = RestoreHealth 750
getPotionAction ManaPhilter = RestoreMana 50
getPotionAction ManaPotion = RestoreMana 120
getPotionAction ManaElixir = RestoreMana 250
getPotionAction FocusStone = RestoreFocus 10
getPotionAction Epinephrine = BoostAdrenaline 50
getPotionAction PhoenixFeather = ReviveCharacter 50
getPotionAction Grapes = RestoreHealthAndMana 15 3
getPotionAction Pineapple = RestoreHealthAndMana 20 5
getPotionAction Bread = RestoreHealth 30
getPotionAction Cheese = RestoreHealth 30
getPotionAction Carrot = RestoreHealth 25
getPotionAction Fish = RestoreHealth 30
getPotionAction Meat = RestoreHealth 30
getPotionAction Eggs = RestoreHealth 20
getPotionAction Radish = RestoreHealth 25
getPotionAction Apple = RestoreHealthAndMana 15 3
getPotionAction Orange = RestoreHealthAndMana 15 5
getPotionAction Strawberry = RestoreHealthAndMana 10 3
getPotionAction Pear = RestoreHealthAndMana 15 3
getPotionAction Lemon = RestoreHealthAndMana 15 5
getPotionAction Mushroom = RestoreHealth 25
getPotionAction _ = RestoreHealth 42 -- FIXME

-------------------------------------------------------------------------------

-- | Represents the bonuses conferred onto a character for equipping an item.
data Bonuses = Bonuses
  { bonusAdrenalineMultiplier :: Double,
    bonusFistsDamageMultiplier :: Double,
    bonusMeleeWeaponDamageMultiplier :: Double,
    bonusPowerModifier :: PowerModifier,
    bonusRangedWeaponDamageMultiplier :: Double,
    bonusRecuperation :: Double,
    bonusResistances :: Resistances,
    bonusSpeedMultiplier :: Double,
    bonusStats :: Stats }

-- | No bonuses of any kind.
nullBonuses :: Bonuses
nullBonuses = Bonuses
  { bonusAdrenalineMultiplier = 1,
    bonusFistsDamageMultiplier = 1,
    bonusMeleeWeaponDamageMultiplier = 1,
    bonusPowerModifier = 1,
    bonusRangedWeaponDamageMultiplier = 1,
    bonusRecuperation = 1,
    bonusResistances = nullResistances,
    bonusSpeedMultiplier = 1,
    bonusStats = nullStats }

addBonuses :: Bonuses -> Bonuses -> Bonuses
addBonuses b1 b2 = Bonuses
  { bonusAdrenalineMultiplier = bonusAdrenalineMultiplier b1 *
                                bonusAdrenalineMultiplier b2,
    bonusFistsDamageMultiplier = bonusFistsDamageMultiplier b1 *
                                 bonusFistsDamageMultiplier b2,
    bonusMeleeWeaponDamageMultiplier = bonusMeleeWeaponDamageMultiplier b1 *
                                       bonusMeleeWeaponDamageMultiplier b2,
    bonusPowerModifier = bonusPowerModifier b1 * bonusPowerModifier b2,
    bonusRangedWeaponDamageMultiplier = bonusRangedWeaponDamageMultiplier b1 *
                                        bonusRangedWeaponDamageMultiplier b2,
    bonusRecuperation = bonusRecuperation b1 * bonusRecuperation b2,
    bonusResistances = (*) <$> bonusResistances b1 <*> bonusResistances b2,
    bonusSpeedMultiplier = bonusSpeedMultiplier b1 * bonusSpeedMultiplier b2,
    bonusStats = (+) <$> bonusStats b1 <*> bonusStats b2 }

sumBonuses :: [Bonuses] -> Bonuses
sumBonuses = foldl' addBonuses nullBonuses

-------------------------------------------------------------------------------

adrenMult :: Double -> Bonuses
adrenMult x = nullBonuses { bonusAdrenalineMultiplier = x }

fistsDamageMult :: Double -> Bonuses
fistsDamageMult x = nullBonuses { bonusFistsDamageMultiplier = x }

meleeDamageMult :: Double -> Bonuses
meleeDamageMult x = nullBonuses { bonusMeleeWeaponDamageMultiplier = x }

powerModifier :: PowerModifier -> Bonuses
powerModifier x = nullBonuses { bonusPowerModifier = x }

rangedDamageMult :: Double -> Bonuses
rangedDamageMult x = nullBonuses { bonusRangedWeaponDamageMultiplier = x }

speedMult :: Double -> Bonuses
speedMult x = nullBonuses { bonusSpeedMultiplier = x }

(+=) :: Stat -> Int -> Bonuses
stat += n = nullBonuses { bonusStats = TM.set stat n nullStats }

(-=) :: Stat -> Int -> Bonuses
stat -= n = nullBonuses { bonusStats = TM.set stat (negate n) nullStats }

(+%) :: Resistance -> Double -> Bonuses
resist +% n = nullBonuses { bonusResistances =
                              TM.set resist (1 - n / 100) nullResistances }

-------------------------------------------------------------------------------

anyone :: TM.TotalMap CharacterClass Bool
anyone = TM.make (const True)

anyoneExcept :: [CharacterClass] -> TM.TotalMap CharacterClass Bool
anyoneExcept cs = TM.make (`notElem` cs)

only :: [CharacterClass] -> TM.TotalMap CharacterClass Bool
only cs = TM.make (`elem` cs)

castersOnly :: TM.TotalMap CharacterClass Bool
castersOnly = TM.make $ \c ->
  c == AlchemistClass || c == ClericClass || c == MagusClass

manaUsersOnly :: TM.TotalMap CharacterClass Bool
manaUsersOnly = TM.make $ \c -> c == ClericClass || c == MagusClass

-------------------------------------------------------------------------------

showSignedInt :: Int -> String
showSignedInt i = if i < 0 then show i else '+' : show i

showSignedPercent :: Double -> String
showSignedPercent p =
  (if p < 0 then "-" else "+") ++ show (round (100 * abs p) :: Int) ++ "%"

indent :: String -> String
indent = ("   " ++)

-------------------------------------------------------------------------------
