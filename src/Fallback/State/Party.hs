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

module Fallback.State.Party where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (assert)
import Control.Monad (guard)
import qualified Data.Foldable as Fold
import qualified Data.IntMap as IntMap
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as Set

import Fallback.Constants (maxPartyLevel, experienceForLevel)
import Fallback.Data.Couple (Couple)
import Fallback.Data.Point (Position)
import qualified Fallback.Data.SparseMap as SM
import qualified Fallback.Data.TotalMap as TM
import Fallback.State.Item
import Fallback.State.Simple
import Fallback.State.Status
  (StatusEffects, seAttackDamageMultiplier, seMentalEffect, seSpeedMultiplier)
import Fallback.State.Tags
import Fallback.State.Terrain
  (ExploredMap, Terrain, setExplored, unexploredMap)
import Fallback.State.Progress (HasProgress(..), Progress)
import Fallback.Utility (squash)

-------------------------------------------------------------------------------

data Party = Party
  { partyCharacters :: TM.TotalMap CharacterNumber Character,
    partyClearedAreas :: Set.Set AreaTag,
    partyCoins :: Integer,
    partyCurrentArea :: AreaTag,
    partyDifficulty :: Difficulty,
    partyExperience :: Int,
    partyExploredMaps :: Map.Map AreaTag ExploredMap,
    partyFoundAreaLinks :: Set.Set (Couple AreaTag),
    partyIngredients :: Ingredients,
    partyItems :: IntMap.IntMap ItemTag,
    partyLevel :: Int,
    partyLevelCap :: Int,
    partyProgress :: Progress,
    partyQuests :: SM.SparseMap QuestTag QuestStatus }
  deriving (Read, Show)

instance HasProgress Party where
  getProgress = partyProgress

partyClearedArea :: Party -> AreaTag -> Bool
partyClearedArea party node = Set.member node $ partyClearedAreas party

partyGetCharacter :: Party -> CharacterNumber -> Character
partyGetCharacter party charNum = TM.get charNum $ partyCharacters party

partyAlterCharacter :: CharacterNumber -> (Character -> Character)
                    -> Party -> Party
partyAlterCharacter charNum fn party =
  party { partyCharacters = TM.adjust charNum fn $ partyCharacters party }

partyExploredMap :: Terrain -> Party -> ExploredMap
partyExploredMap terrain party =
  fromMaybe (unexploredMap terrain) $ Map.lookup (partyCurrentArea party) $
  partyExploredMaps party

partyUpdateExploredMap :: Terrain -> Set.Set Position -> Party -> Party
partyUpdateExploredMap terrain visible party =
  let tag = partyCurrentArea party
      exploredMaps = partyExploredMaps party
      explored = fromMaybe (unexploredMap terrain) $
                 Map.lookup tag exploredMaps
      explored' = setExplored (Set.toList visible) explored
  in party { partyExploredMaps = Map.insert tag explored' exploredMaps }

partyCanAffordCastingCost :: CharacterNumber -> CastingCost -> Party -> Bool
partyCanAffordCastingCost charNum cost party =
  case cost of
    AdrenalineCost adren -> adren <= chrAdrenaline char
    FocusCost focus -> focus <= chrMojo char
    IngredientCost ing -> Fold.and $ (<=) <$> ing <*> partyIngredients party
    ManaCost mana -> mana <= chrMojo char
    NoCost -> True
  where char = partyGetCharacter party charNum

partyDeductCastingCost :: CharacterNumber -> CastingCost -> Party -> Party
partyDeductCastingCost charNum cost party =
  case cost of
    AdrenalineCost adren ->
      let fn char = char { chrAdrenaline = max 0 $ chrAdrenaline char - adren }
      in partyAlterCharacter charNum fn party
    FocusCost focus ->
      let fn char = char { chrMojo = max 0 $ chrMojo char - focus }
      in partyAlterCharacter charNum fn party
    IngredientCost ing ->
      party { partyIngredients = (max 0 .) . subtract <$> ing <*>
                                 partyIngredients party }
    ManaCost mana ->
      let fn char = char { chrMojo = max 0 $ chrMojo char - mana }
      in partyAlterCharacter charNum fn party
    NoCost -> party

numIngredientUsers :: TM.TotalMap CharacterNumber Character -> Int
numIngredientUsers = Fold.sum . fmap (usesIngredients . chrClass) where
  usesIngredients HunterClass = 1
  usesIngredients AlchemistClass = 1
  usesIngredients _ = 0

partyMaxIngredientCount :: Party -> Int
partyMaxIngredientCount =
  (100 *) . max 1 . numIngredientUsers . partyCharacters

partyRemoveItem :: ItemSlot -> Party -> Party
partyRemoveItem (CharWeaponSlot charNum) party =
  let fn c = c { chrEquipment = (chrEquipment c) { eqpWeapon = Nothing } }
  in partyAlterCharacter charNum fn party
partyRemoveItem (CharArmorSlot charNum) party =
  let fn c = c { chrEquipment = (chrEquipment c) { eqpArmor = Nothing } }
  in partyAlterCharacter charNum fn party
partyRemoveItem (CharAccessorySlot charNum) party =
  let fn c = c { chrEquipment = (chrEquipment c) { eqpAccessory = Nothing } }
  in partyAlterCharacter charNum fn party
partyRemoveItem (PartyItemSlot idx) party =
  party { partyItems = IntMap.delete idx (partyItems party) }

partySpendUpgrades :: (SM.SparseMap (CharacterNumber, Stat) Int)
                   -> (SM.SparseMap (CharacterNumber, AbilityNumber) Int)
                   -> Party -> Party
partySpendUpgrades st sk party = party' where
  party' = party { partyCharacters = TM.mapWithKey upgradeChar $
                                     partyCharacters party }
  upgradeChar charNum char = char' where
    char' = char { chrAbilities = abilities',
                   chrBaseStats = baseStats',
                   chrSkillPoints = assert (skillPoints' >= 0) $ skillPoints',
                   chrStatPoints = assert (statPoints' >= 0) $ statPoints' }
    skillDelta abilNum = SM.get (charNum, abilNum) sk
    statDelta stat = SM.get (charNum, stat) st
    abilities' = abilityRankPlus <$> chrAbilities char <*>
                 TM.make skillDelta
    baseStats' = (+) <$> chrBaseStats char <*> TM.make statDelta
    skillPoints' = chrSkillPoints char -
                   (sum $ map skillDelta [minBound .. maxBound])
    statPoints' = chrStatPoints char -
                  (sum $ map statDelta [minBound .. maxBound])

partyTryExchangeItem :: ItemSlot -> Maybe ItemTag -> Party
                     -> Maybe (Maybe ItemTag, Party)
partyTryExchangeItem slot mbTag party =
  case slot of
    CharWeaponSlot charNum ->
      tryEquip charNum (wdUsableBy . getWeaponData)
               (fmap WeaponItemTag . eqpWeapon) (\e t -> e { eqpWeapon = t }) $
      \tag -> case tag of { WeaponItemTag t -> Just t; _ -> Nothing }
    CharArmorSlot charNum ->
      tryEquip charNum (adUsableBy . getArmorData)
               (fmap ArmorItemTag . eqpArmor) (\e t -> e { eqpArmor = t }) $
      \tag -> case tag of { ArmorItemTag t -> Just t; _ -> Nothing }
    CharAccessorySlot charNum ->
      tryEquip charNum (adUsableBy . getAccessoryData)
               (fmap AccessoryItemTag . eqpAccessory)
               (\e t -> e { eqpAccessory = t }) $
      \tag -> case tag of { AccessoryItemTag t -> Just t; _ -> Nothing }
    PartyItemSlot index ->
      let items = partyItems party
          items' = case mbTag of Just tag -> IntMap.insert index tag items
                                 Nothing -> IntMap.delete index items
      in Just (IntMap.lookup index items, party { partyItems = items' })
  where
    tryEquip :: CharacterNumber -> (a -> TM.TotalMap CharacterClass Bool)
             -> (Equipment -> Maybe ItemTag)
             -> (Equipment -> Maybe a -> Equipment)
             -> (ItemTag -> Maybe a) -> Maybe (Maybe ItemTag, Party)
    tryEquip charNum usableFn getFn setFn fromItemTag = do
      let char = partyGetCharacter party charNum
      mbEquipTag <- case mbTag of
                      Nothing -> Just Nothing
                      Just tag -> do
                        tag' <- fromItemTag tag
                        guard $ TM.get (chrClass char) $ usableFn tag'
                        Just (Just tag')
      let eqp = chrEquipment char
      Just (getFn eqp, partyAlterCharacter charNum
              (\c -> c {chrEquipment = setFn eqp mbEquipTag}) party)

-- | Add experience to the party; if necessary, increase level and stats/skill
-- points of characters.
partyGrantExperience :: Int -> Party -> Party
partyGrantExperience xp party =
  let cap = min maxPartyLevel (partyLevelCap party)
      xp' = min (experienceForLevel $ min maxPartyLevel (cap + 1))
                (partyExperience party + xp)
      stop level = level >= cap || experienceForLevel (level + 1) > xp'
      level' = until stop (+1) (partyLevel party)
      delta = level' - partyLevel party
  in if delta <= 0 then party { partyExperience = xp' } else
       party { partyCharacters = levelUp delta <$> partyCharacters party,
               partyExperience = xp', partyLevel = level' }
  where
    levelUp levels char =
      char { chrBaseStats = (+) <$> chrStatDeltas char levels <*>
                            chrBaseStats char,
             chrSkillPoints = chrSkillPoints char + levels,
             chrStatPoints = chrStatPoints char + 2 * levels }
    chrStatDeltas char levels = fmap (levels *) $ TM.make $ statOf $
      case chrClass char of
        WarriorClass -> (3, 2, 1)
        RogueClass -> (1, 3, 2)
        HunterClass -> (2, 3, 1)
        AlchemistClass -> (3, 1, 2)
        ClericClass -> (2, 1, 3)
        MagusClass -> (1, 2, 3)
    statOf (s, _, _) Strength = s
    statOf (_, a, _) Agility = a
    statOf (_, _, i) Intellect = i

partyAlterIngredients :: (Ingredients -> Ingredients) -> Party -> Party
partyAlterIngredients fn party = party { partyIngredients = ings' } where
  ings' = fmap (max 0 . min (partyMaxIngredientCount party)) $
          fn $ partyIngredients party

-- TODO: deprecated
partyGrantIngredient :: Int -> Ingredient -> Party -> Party
partyGrantIngredient n ingredient party =
  party { partyIngredients = TM.adjust ingredient fn (partyIngredients party) }
  where fn = max 0 . min (partyMaxIngredientCount party) . (+ n)

-- | Add an item to the first open slot in the party inventory.
partyGrantItem :: ItemTag -> Party -> Party
partyGrantItem tag party =
  let items = partyItems party
      index = until (flip IntMap.notMember items) (+1) 0
  in party { partyItems = IntMap.insert index tag items }

-- | Determine if the party possesses at least one copy of the item, either in
-- the party inventory or worn as equipment by one of the characters.
partyHasItem :: ItemTag -> Party -> Bool
partyHasItem tag party =
  Fold.any (== tag) (partyItems party) ||
  Fold.any (chrHasItemEquipped tag) (partyCharacters party)

-- | Find an item matching the given predicate, and the slot that that item is
-- in.  The shared party inventory is searched first, followed by each
-- character's equipment.
partyFindItem :: (ItemTag -> Bool) -> Party -> Maybe (ItemTag, ItemSlot)
partyFindItem fn party = find (fn . fst) $
  map (\(idx, tag) -> (tag, PartyItemSlot idx))
      (IntMap.assocs $ partyItems party) ++
  mapMaybe (\slot -> flip (,) slot <$> partyItemInSlot slot party)
           allEquipmentSlots

partyItemInSlot :: ItemSlot -> Party -> Maybe ItemTag
partyItemInSlot (PartyItemSlot index) party =
  IntMap.lookup index $ partyItems party
partyItemInSlot (CharWeaponSlot charNum) party =
  fmap WeaponItemTag $ eqpWeapon $ chrEquipment $
  partyGetCharacter party charNum
partyItemInSlot (CharArmorSlot charNum) party =
  fmap ArmorItemTag $ eqpArmor $ chrEquipment $
  partyGetCharacter party charNum
partyItemInSlot (CharAccessorySlot charNum) party =
  fmap AccessoryItemTag $ eqpAccessory $ chrEquipment $
  partyGetCharacter party charNum

-- | Remove all copies of the item from the party.
partyPurgeItem :: ItemTag -> Party -> Party
partyPurgeItem tag party =
  party { partyCharacters = fmap (chrPurgeItem tag) (partyCharacters party),
          partyItems = IntMap.filter (tag /=) (partyItems party) }

-------------------------------------------------------------------------------

data Character = Character
  { chrAbilities :: TM.TotalMap AbilityNumber (Maybe AbilityRank),
    chrAdrenaline :: Int,
    chrAppearance :: CharacterAppearance,
    chrBaseStats :: Stats,
    chrClass :: CharacterClass,
    chrEquipment :: Equipment,
    chrHealth :: Int,
    chrMojo :: Int,
    chrName :: String,
    chrSkillPoints :: Int,
    chrStatPoints :: Int,
    chrStatus :: StatusEffects }
  deriving (Read, Show)

chrAbilityRank :: AbilityTag -> Character -> Maybe AbilityRank
chrAbilityRank tag char =
  let (cls, num) = abilityClassAndNumber tag
  in if cls /= chrClass char then Nothing else TM.get num $ chrAbilities char

chrAbilityMultiplier :: AbilityTag -> Double -> Double -> Double -> Character
                     -> Double
chrAbilityMultiplier tag m1 m2 m3 char =
  case chrAbilityRank tag char of
    Nothing -> 1
    Just Rank1 -> m1
    Just Rank2 -> m2
    Just Rank3 -> m3

chrAdrenalineMultiplier :: Character -> Double
chrAdrenalineMultiplier char =
  chrAbilityMultiplier Valiance 1.1 1.2 1.3 char *
  (product $ map bonusAdrenalineMultiplier $ chrBonusesList char)

chrAlterStatus :: (StatusEffects -> StatusEffects) -> Character -> Character
chrAlterStatus fn char = char { chrStatus = fn (chrStatus char) }

-- | Get the total bonuses for a character's equipped items.
chrBonuses :: Character -> Bonuses
chrBonuses = sumBonuses . chrBonusesList

chrBonusesList :: Character -> [Bonuses]
chrBonusesList char =
  catMaybes [wdBonuses . getWeaponData <$> eqpWeapon eqp,
             adBonuses . getArmorData <$> eqpArmor eqp,
             adBonuses . getAccessoryData <$> eqpAccessory eqp]
  where eqp = chrEquipment char

chrEquippedWeaponData :: Character -> WeaponData
chrEquippedWeaponData char =
  case wdRange wd of
    Melee -> wd
    Ranged n ->
      if chrAbilityRank EagleEye char < Just Rank3 then wd
      else wd { wdRange = Ranged (n + 1) }
  where wd = maybe unarmedWeaponData getWeaponData $
             eqpWeapon $ chrEquipment char

-- | Determine if the character has a copy of the item equipped.  For
-- non-equippable items, this always returns 'False'.
chrHasItemEquipped :: ItemTag -> Character -> Bool
chrHasItemEquipped (WeaponItemTag tag) char =
  Just tag == eqpWeapon (chrEquipment char)
chrHasItemEquipped (ArmorItemTag tag) char =
  Just tag == eqpArmor (chrEquipment char)
chrHasItemEquipped (AccessoryItemTag tag) char =
  Just tag == eqpAccessory (chrEquipment char)
chrHasItemEquipped _ _ = False

chrIsConscious :: Character -> Bool
chrIsConscious char = chrHealth char > 0

chrCanTakeTurn :: Character -> Bool
chrCanTakeTurn char =
  chrIsConscious char && (isNothing $ seMentalEffect $ chrStatus char)

chrMaxHealth :: Party -> Character -> Int
chrMaxHealth party char =
  100 + ((15 + partyLevel party) * chrGetStat Strength char) `div` 10

-- | Determine a character's maximum mojo (that is, mana or focus).
chrMaxMojo :: Party -> Character -> Int
chrMaxMojo party char =
  case chrClass char of
    WarriorClass -> maxFocus
    RogueClass -> maxFocus
    HunterClass -> 0
    AlchemistClass -> 0
    ClericClass -> maxMana
    MagusClass -> maxMana
  where
    maxFocus = floor $ squash 16.5 $
      fromIntegral ((30 + partyLevel party) * chrGetStat Intellect char) / 200
    maxMana = ((15 + partyLevel party) * chrGetStat Intellect char) `div` 10

chrPurgeItem :: ItemTag -> Character -> Character
chrPurgeItem (WeaponItemTag tag) char =
  if Just tag /= eqpWeapon (chrEquipment char) then char
  else char { chrEquipment = (chrEquipment char) { eqpWeapon = Nothing } }
chrPurgeItem (ArmorItemTag tag) char =
  if Just tag /= eqpArmor (chrEquipment char) then char
  else char { chrEquipment = (chrEquipment char) { eqpArmor = Nothing } }
chrPurgeItem (AccessoryItemTag tag) char =
  if Just tag /= eqpAccessory (chrEquipment char) then char
  else char { chrEquipment = (chrEquipment char) { eqpAccessory = Nothing } }
chrPurgeItem _ char = char

-- | Get all resistance values for a character (including item bonuses).
chrResistances :: Character -> Resistances
chrResistances char =
  let strength = chrGetStat Strength char
      agility = chrGetStat Agility char
      intellect = chrGetStat Intellect char
      baseResist Armor = chrAbilityMultiplier Hardiness 0.97 0.94 0.90 char
      baseResist ResistFire = 0.9975 ^^ strength
      baseResist ResistCold = 0.9975 ^^ agility
      baseResist ResistEnergy = 0.9975 ^^ intellect
      baseResist ResistChemical =
        0.9966 ^^ strength * chrAbilityMultiplier Immunity 0.9 0.8 0.6 char
      baseResist ResistMental =
        0.9966 ^^ intellect * chrAbilityMultiplier Clarity 0.8 0.6 0.3 char
      baseResist ResistStun = 0.9966 ^^ agility
  in (*) <$> TM.make baseResist <*> bonusResistances (chrBonuses char)

-- | Get the specified resistance value for a character (including item
-- bonuses).
chrGetResistance :: Resistance -> Character -> Double
chrGetResistance resist char = TM.get resist $ chrResistances char

-- | Get the total value of a stat for a character (including item bonuses).
chrGetStat :: Stat -> Character -> Int
chrGetStat stat char = max 1 $
  TM.get stat (chrBaseStats char) +
  (sum $ map (TM.get stat . bonusStats) $ chrBonusesList char)

-- | Determine the character's power modifier for special abilities (counting
-- everything except bonuses from meta-abilities).
chrPowerModifier :: Character -> PowerModifier
chrPowerModifier char =
  (1 + squash 1 (0.01 * fromIntegral (chrGetStat Intellect char))) *
  (product $ map bonusPowerModifier $ chrBonusesList char)

chrRecuperation :: Character -> Double
chrRecuperation = chrAbilityMultiplier Recuperation 1.1 1.2 1.4

-- | Get the current speed multiplier for a character, taking skills, item
-- bonuses, and status effects into account.
chrSpeed :: Character -> Double
chrSpeed char =
  (1 + squash 2 (0.01 * fromIntegral (chrGetStat Agility char))) *
  (product $ map bonusSpeedMultiplier $ chrBonusesList char) *
  chrAbilityMultiplier Alacrity 1.05 1.10 1.20 char *
  seSpeedMultiplier (chrStatus char)

chrWeaponDamageMultiplier :: Character -> Double
chrWeaponDamageMultiplier char =
  seAttackDamageMultiplier (chrStatus char) *
  case eqpWeapon $ chrEquipment char of
    Just tag ->
      case wdRange $ getWeaponData tag of
        Melee -> bonus bonusMeleeWeaponDamageMultiplier
        Ranged _ ->
          chrAbilityMultiplier EagleEye 1 1.15 1.15 char *
          bonus bonusRangedWeaponDamageMultiplier
    Nothing -> bonus bonusFistsDamageMultiplier
  where bonus fn = product $ map fn $ chrBonusesList char

-------------------------------------------------------------------------------

data Equipment = Equipment
  { eqpWeapon :: Maybe WeaponItemTag,
    eqpArmor :: Maybe ArmorItemTag,
    eqpAccessory :: Maybe AccessoryItemTag }
  deriving (Read, Show)

-------------------------------------------------------------------------------
