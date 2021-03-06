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

module Fallback.Scenario.Script.Damage
  (DamageSeverity(..), dealDamage, dealDamageTotal, dealDamageGeneral,
   killTarget, healDamage, reviveTarget, inflictAllPeriodicDamage,
   expectedDamage)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (right)
import Control.Exception (assert)
import Control.Monad (foldM, forM_, unless, void, when)
import qualified Data.Foldable as Fold (any)
import Data.List (find, foldl1')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)

import Fallback.Constants (maxAdrenaline, momentsPerActionPoint)
import Fallback.Control.Script
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM (get)
import Fallback.Scenario.Script.Base
import Fallback.Scenario.Script.Doodad
import Fallback.Scenario.Script.Other
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Party
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.Utility
  (ceilDiv, flip3, forMaybeM, groupKey, maybeM, sortKey, square, sumM)

-------------------------------------------------------------------------------
-- Damage:

-- | Determines the side effects of damage dealt to a creature.  Most damage is
-- 'HarshDamage'.  Certains attacks (such as ripostes and attacks of
-- opportunity) cause 'LightDamage', which (unlike 'HarshDamage') doesn't stun
-- or increase adrenaline.  'GentleDamage' is like 'LightDamage', but doesn't
-- even cause a hurt animation or show a number if the amount of damage (after
-- resistances, etc.) turns out to be zero.
data DamageSeverity = GentleDamage | LightDamage | HarshDamage
  deriving (Eq, Ord)

dealDamage :: (FromAreaEffect f) => [(HitTarget, DamageType, Double)]
           -> Script f ()
dealDamage = void . dealDamageTotal

-- | Like 'dealDamage', but return the total damage done to all targets (after
-- resistances).
dealDamageTotal :: (FromAreaEffect f) => [(HitTarget, DamageType, Double)]
                -> Script f Int
dealDamageTotal = dealDamageGeneral HarshDamage Nothing

dealDamageGeneral :: (FromAreaEffect f) => DamageSeverity -> Maybe Double
                  -> [(HitTarget, DamageType, Double)] -> Script f Int
dealDamageGeneral severity finalBlow hits = do
  let convert (hitTarget, dmgType, damage) = do
        mbOccupant <- getHitTargetOccupant hitTarget
        return $ case mbOccupant of
                   Just occupant -> Just (occupant, dmgType, damage)
                   Nothing -> Nothing
  let resist (occupant, dmgType, damage) = do
        -- Stun is measured in action points.  We do 1 AP stun per 100 damage.
        let stun = if severity == HarshDamage then damage * 0.01 else 0
        (damage', stun') <- do
          case occupant of
            Left charNum -> do
              char <- areaGet (arsGetCharacter charNum)
              return $ applyCharResistances char dmgType damage stun
            Right monstEntry -> do
              return $ applyMonsterResistances (Grid.geValue monstEntry)
                                               dmgType damage stun
        return (occupant, damage', stun')
  let keyFn (k, _, _) = right Grid.geKey k
  let combine (_, dmg1, s1) (k, dmg2, s2) = (k, dmg1 + dmg2, s1 + s2)
  let inflict (occupant, damage, stun) = do
        case occupant of
          -- Characters are not subject to Final Blows.
          Left charNum -> dealRawDamageToCharacter severity charNum damage stun
          Right monstEntry ->
            dealRawDamageToMonster severity finalBlow (Grid.geKey monstEntry)
                                   damage stun
  sumM inflict . map (foldl1' combine) . groupKey keyFn . sortKey keyFn =<<
    mapM resist =<< forMaybeM hits convert

-- | Inflict damage and stun on a character, ignoring armor and resistances.
-- Stun is measured in action points.
dealRawDamageToCharacter :: (FromAreaEffect f) => DamageSeverity
                         -> CharacterNumber -> Int -> Double -> Script f Int
dealRawDamageToCharacter severity charNum damage stun = do
  -- If the damage is zero, and it's gentle damage, then don't do anything
  -- (don't even set the hurt anim or show a number doodad).
  if severity <= GentleDamage && damage == 0 then return 0 else do
  char <- areaGet (arsGetCharacter charNum)
  -- If this is harsh damage (the normal kind of damage), wake us up from being
  -- dazed and add adrenaline.
  adrenaline <- if severity < HarshDamage then return 0 else do
    alterCharacterStatus charNum seWakeFromDaze
    party <- areaGet arsParty
    let adrenaline = adrenalineForDamage (chrAdrenalineMultiplier char)
                                         damage (chrMaxHealth party char)
    alterAdrenaline charNum (+ adrenaline)
    return adrenaline
  -- Do stun (if in combat):
  when (stun > 0) $ whenCombat $ do
    moments <- emitEffect $ EffGetCharMoments charNum
    emitEffect $ EffSetCharMoments charNum $
      max 0 (moments - round (stun * fromIntegral momentsPerActionPoint))
  -- Do damage:
  minHealth <- emitAreaEffect $ EffIfCombat (return 0) (return 1)
  let health' = max minHealth (chrHealth char - damage)
  addFloatingNumberOnTarget hurtTint damage (HitCharacter charNum)
  emitAreaEffect $ EffAlterCharacter charNum $ \c -> c { chrHealth = health' }
  emitAreaEffect $ EffIfCombat (setCharacterAnim charNum $ HurtAnim 12)
                               (setPartyAnim $ HurtAnim 12)
  -- If we're in combat, the character can die:
  whenCombat $ when (health' <= 0) $ do
    onCharacterDead charNum
    -- When a character is killed by normal damage, all other conscious
    -- characters get the amount of adrenaline that that character would have
    -- gotten.  (This doesn't apply when the character is insta-killed.)
    mapM_ (flip alterAdrenaline (+ adrenaline)) =<< getAllConsciousCharacters
  return damage

-- | Inflict damage and stun on a monster, ignoring armor and resistances.
-- Stun is measured in action points.  The monster must exist.
dealRawDamageToMonster :: (FromAreaEffect f) => DamageSeverity -> Maybe Double
                       -> Grid.Key Monster -> Int -> Double -> Script f Int
dealRawDamageToMonster severity finalBlow key damage stun = do
  -- If the damage is zero, and it's gentle damage, then don't do anything
  -- (don't even set the monster hurt anim or show a number doodad).
  if severity <= GentleDamage && damage == 0 then return 0 else do
  -- If this is harsh damage (the normal kind of damage), wake us up from being
  -- dazed.
  when (severity >= HarshDamage) $ alterMonsterStatus key seWakeFromDaze
  entry <- demandMonsterEntry key
  let monst = Grid.geValue entry
  -- Do damage and stun:
  (health', damage') <- do
    let health' = max 0 (monstHealth monst - damage)
    case finalBlow of
      Just mult | health' > 0 &&
                  fromIntegral health' <= fromIntegral damage * mult -> do
        addFloatingWordOnTarget WordFinalBlow (HitMonster key)
        return (0, monstHealth monst)
      _ -> return (health', damage)
  let moments' = max 0 (monstMoments monst -
                        round (stun * fromIntegral momentsPerActionPoint))
  let mbMonst' = if health' <= 0 then Nothing else Just monst
                   { monstPose = (monstPose monst) { cpAnim = HurtAnim 12 },
                     monstHealth = health', monstMoments = moments' }
  addFloatingNumberOnTarget hurtTint damage' (HitMonster key)
  emitAreaEffect $ EffReplaceMonster key mbMonst'
  -- If the monster is now dead, we need do to several things.
  when (isNothing mbMonst') $ onMonsterDead entry
  return damage'

adrenalineForDamage :: Double -> Int -> Int -> Int
adrenalineForDamage multiplier damage maxHealth =
  round $ (fromIntegral maxAdrenaline * multiplier *) $ square $ min 1 $
  2 * fromIntegral damage / (fromIntegral maxHealth :: Double)

killTarget :: (FromAreaEffect f) => HitTarget -> Script f ()
killTarget hitTarget = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      minHealth <- emitAreaEffect $ EffIfCombat (return 0) (return 1)
      emitAreaEffect $ EffAlterCharacter charNum $ \c ->
        c { chrHealth = minHealth }
      whenCombat $ onCharacterDead charNum
    Just (Right entry) -> do
      emitAreaEffect $ EffReplaceMonster (Grid.geKey entry) Nothing
      onMonsterDead entry
    Nothing -> return ()

onCharacterDead :: CharacterNumber -> Script CombatEffect ()
onCharacterDead charNum = do
  alterCharacterMojo charNum (const 0)
  alterAdrenaline charNum (const 0)
  alterCharacterStatus charNum (const initStatusEffects)
  resources <- areaGet arsResources
  char <- areaGet (arsGetCharacter charNum)
  let images = rsrcCharacterImages resources (chrClass char)
                                   (chrAppearance char)
  faceDir <- emitEffect $ EffGetCharFaceDir charNum
  playSound (characterScreamSound char)
  playSound SndDie1
  pos <- areaGet (arsCharacterPosition charNum)
  addDeathDoodad images faceDir (makeRect pos (1, 1))
  -- Unsummon monsters as necessary.
  unsummonDependentsOf (Left charNum)
  -- If all characters are now dead, it's game over:
  alive <- areaGet (Fold.any chrIsConscious . partyCharacters . arsParty)
  unless alive $ emitAreaEffect EffGameOver

onMonsterDead :: (FromAreaEffect f) => Grid.Entry Monster -> Script f ()
onMonsterDead entry = do
  let monst = Grid.geValue entry
  -- Add a death doodad.
  resources <- areaGet arsResources
  addDeathDoodad (rsrcMonsterImages resources $ monstType monst)
                 (cpFaceDir $ monstPose monst) (Grid.geRect entry)
  -- Add the monster's remains (if any).
  maybeM (mtRemains $ monstType monst) $ \remains -> do
    -- TODO Do something prettier for non-small monsters.  For example, maybe
    --   do a random collection of large and small bloods.
    mapM_ (addRemains remains) $ prectPositions $ Grid.geRect entry
  -- If the monster has a "dead" var, set it to True.
  maybeM (monstDeadVar monst) (emitAreaEffect . flip EffSetVar True)
  -- If this was an enemy monster, grant experience and coins for killing it,
  -- and give each Warrior/Rogue character one focus point.
  unless (monstIsAlly monst) $ do
    let mtype = monstType monst
    grantExperience $ mtExperienceValue mtype
    alterPartyCoins . (+) =<< uncurry getRandomR (mtCoins mtype)
    charNums <- getAllConsciousCharacters
    forM_ charNums $ \charNum -> alterFocus (HitCharacter charNum) (+1)
  -- Unsummon other monsters as necessary.
  unsummonDependentsOf $ Right $ Grid.geKey entry

-------------------------------------------------------------------------------
-- Healing:

healDamage :: (FromAreaEffect f) => [(HitTarget, Double)] -> Script f ()
healDamage hits = do
  heals <- flip3 foldM Map.empty hits $ \totals (hitTarget, amount) -> do
    mbOccupant <- getHitTargetOccupant hitTarget
    return $ case mbOccupant of
               Nothing -> totals
               Just occupant ->
                 Map.alter (Just . (amount +) . fromMaybe 0)
                           (right Grid.geKey occupant) totals
  forM_ (Map.assocs heals) $ \(occupant, amount) -> do
    either healCharacter healMonster occupant amount

healCharacter :: (FromAreaEffect f) => CharacterNumber -> Double -> Script f ()
healCharacter charNum baseAmount = do
  conscious <- areaGet (chrIsConscious . arsGetCharacter charNum)
  when conscious $ do
  amount <- areaGet (max 0 . round . (baseAmount *) .
                     chrRecuperation . arsGetCharacter charNum)
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrHealth = min (chrMaxHealth party char)
                           (chrHealth char + amount) }
  pos <- areaGet (arsCharacterPosition charNum)
  addBoomDoodadAtPosition HealBoom 4 pos
  addFloatingNumberOnTarget healTint amount (HitCharacter charNum)

healMonster :: (FromAreaEffect f) => Grid.Key Monster -> Double -> Script f ()
healMonster key baseAmount = withMonsterEntry key $ \entry -> do
  let monst = Grid.geValue entry
  let amount = max 0 $ round baseAmount
  let health' = min (monstHealth monst + amount)
                    (mtMaxHealth $ monstType monst)
  let monst' = monst { monstHealth = health' }
  emitAreaEffect $ EffReplaceMonster key (Just monst')
  let prect = Grid.geRect entry
  forM_ (prectPositions prect) $ addBoomDoodadAtPosition HealBoom 4
  addFloatingNumberOnTarget healTint amount (HitMonster key)

reviveTarget :: HitTarget -> Double -> Script CombatEffect ()
reviveTarget hitTarget baseAmount = do
  let simpleHeal = healDamage [(hitTarget, baseAmount)]
  case hitTarget of
    HitCharacter charNum -> do
      conscious <- areaGet (chrIsConscious . arsGetCharacter charNum)
      if conscious then simpleHeal else do
      lastPos <- areaGet (arsCharacterPosition charNum)
      mbPos <- do
        blocked <- areaGet arsIsBlockedForParty
        directions <- randomPermutation allDirections
        areaGet (find (not . blocked) . (lastPos :) .
                 arsAccessiblePositions directions lastPos)
      maybeM mbPos (emitEffect . EffSetCharPosition charNum)
      emitEffect $ EffSetCharAnim charNum NoAnim
      health <- do
        party <- areaGet arsParty
        let char = partyGetCharacter party charNum
        return (max 1 $ min (chrMaxHealth party char) $ round $
                (baseAmount *) $ chrRecuperation char)
      emitAreaEffect $ EffAlterCharacter charNum $ \char -> char
        { chrHealth = health }
      addBoomDoodadAtPosition HealBoom 4 =<<
        areaGet (arsCharacterPosition charNum)
      addFloatingNumberOnTarget healTint health (HitCharacter charNum)
    _ -> simpleHeal

-------------------------------------------------------------------------------
-- Periodic damage:

inflictAllPeriodicDamage :: (FromAreaEffect f) => Script f ()
inflictAllPeriodicDamage = do
  fields <- areaGet (Map.assocs . arsFields)
  fieldDamages <- forMaybeM fields $ \(pos, field) -> do
    case field of
      BarrierWall _ -> return Nothing
      FireWall baseDamage -> do
        damage <- (baseDamage *) <$> getRandomR 0.8 1.2
        return $ Just (HitPosition pos, FireDamage, damage)
      IceWall baseDamage -> do
        damage <- (baseDamage *) <$> getRandomR 0.8 1.2
        return $ Just (HitPosition pos, ColdDamage, damage)
      PoisonCloud basePoison -> do
        poison <- (basePoison *) <$> getRandomR 0.8 1.2
        Nothing <$ inflictPoison (HitPosition pos) poison
      SmokeScreen _ -> return Nothing
      Webbing rounds -> do
        occupied <- areaGet (arsOccupied pos)
        when occupied $ do
          removeFields [pos]
          alterStatus (HitPosition pos) (seApplyEntanglement rounds)
        return Nothing
  charNums <- getAllConsciousCharacters
  party <- areaGet arsParty
  charPoisonDamages <- forMaybeM charNums $ \charNum -> do
    let delta = seRegenPoison $ chrStatus $ partyGetCharacter party charNum
    when (delta > 0) $ do
      let amount = delta `ceilDiv` 5
      alterCharacterStatus charNum $ seAlterRegenPoison (subtract amount)
      healDamage [(HitCharacter charNum, fromIntegral amount)]
    if delta >= 0 then return Nothing else do
      let damage = negate delta `ceilDiv` 5
      alterCharacterStatus charNum $ seAlterRegenPoison (+ damage)
      return $ Just (HitCharacter charNum, RawDamage, fromIntegral damage)
  monsters <- areaGet (Grid.entries . arsMonsters)
  monstPoisonDamages <- forMaybeM monsters $ \monstEntry -> do
    let key = Grid.geKey monstEntry
    let delta = seRegenPoison $ monstStatus $ Grid.geValue monstEntry
    when (delta > 0) $ do
      let amount = delta `ceilDiv` 5
      alterMonsterStatus key $ seAlterRegenPoison (subtract amount)
      healDamage [(HitMonster key, fromIntegral amount)]
    if delta >= 0 then return Nothing else do
      let damage = negate delta `ceilDiv` 5
      alterMonsterStatus key $ seAlterRegenPoison (+ damage)
      return $ Just (HitMonster key, RawDamage, fromIntegral damage)
  void $ dealDamageGeneral GentleDamage Nothing $
    fieldDamages ++ charPoisonDamages ++ monstPoisonDamages

-------------------------------------------------------------------------------

expectedDamage :: Monster -> DamageType -> Double -> Double
expectedDamage monst dmgType damage =
  fromIntegral $ fst $ applyMonsterResistances monst dmgType damage 0

-------------------------------------------------------------------------------
-- Private:

applyResistances :: StatusEffects -> Resistances -> DamageType -> Double
                 -> Double -> (Int, Double)
applyResistances status resist dmgType damage stun = (damage', stun')
  where
    armor = TM.get Armor resist * seArmorMultiplier status
    magicArmor = assert (armor >= 0) (sqrt armor) *
                 seMagicShieldMultiplier status
    damage' = round $ (damage *) $
              case dmgType of
                AcidDamage -> TM.get ResistChemical resist * magicArmor
                ColdDamage -> TM.get ResistCold resist * magicArmor
                EnergyDamage -> TM.get ResistEnergy resist * magicArmor
                FireDamage -> TM.get ResistFire resist * magicArmor
                MagicDamage -> magicArmor
                PhysicalDamage -> armor
                RawDamage -> 1
    stun' = stun * TM.get ResistStun resist

applyCharResistances :: Character -> DamageType -> Double -> Double
                     -> (Int, Double)
applyCharResistances char dmgType damage stun =
  applyResistances (chrStatus char) (chrResistances char) dmgType damage stun

applyMonsterResistances :: Monster -> DamageType -> Double -> Double
                        -> (Int, Double)
applyMonsterResistances monst dmgType damage stun =
  applyResistances (monstStatus monst) (mtResistances mt) dmgType damage stun
  where mt = monstType monst

characterScreamSound :: Character -> SoundTag
characterScreamSound char =
  case (chrClass char, chrAppearance char) of
    (WarriorClass, Appearance2) -> SndHurtMale
    (WarriorClass, Appearance3) -> SndHurtMale
    (RogueClass, Appearance1) -> SndHurtMale
    (RogueClass, Appearance2) -> SndHurtMale
    (HunterClass, Appearance0) -> SndHurtMale
    (HunterClass, Appearance3) -> SndHurtMale
    (AlchemistClass, Appearance0) -> SndHurtMale
    (AlchemistClass, Appearance2) -> SndHurtMale
    (ClericClass, Appearance1) -> SndHurtMale
    (ClericClass, Appearance3) -> SndHurtMale
    (MagusClass, Appearance0) -> SndHurtMale
    (MagusClass, Appearance1) -> SndHurtMale
    _ -> SndHurtFemale

hurtTint, healTint :: Tint
hurtTint = Tint 255 192 192 255
healTint = Tint 140 255 192 255

-------------------------------------------------------------------------------
