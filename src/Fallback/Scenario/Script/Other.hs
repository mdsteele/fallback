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

module Fallback.Scenario.Script.Other
  (-- * Actions
   -- ** Movement
   getPartyPosition, setPartyPosition, partyWalkTo, charWalkTo, teleport,
   exitTo, walkMonster, setMonsterTownAI,
   -- ** Attacks
   characterBeginOffensiveAction, characterWeaponAttack,
   characterWeaponInitialAnimation, characterWeaponBaseDamage,
   characterWeaponChooseCritical, characterWeaponHit,
   monsterBeginOffensiveAction, monsterPerformAttack,

   -- * Effects
   -- ** Damage
   dealDamage, dealDamageTotal, healDamage,
   -- ** Mojo/adrenaline
   alterMana, alterCharacterMojo, restoreMojoToFull, alterAdrenaline,
   -- ** Status effects
   alterStatus, grantInvisibility, inflictPoison, inflictStun,
   -- ** Other
   grantExperience, removeFields, setFields,
   getTerrainTile, resetTerrain, setTerrain,

   -- * Animation
   -- ** Camera motion
   shakeCamera,
   -- ** Creature animation
   faceCharacterToward, faceMonsterToward, facePartyToward,
   setCharacterAnim, setMonsterAnim, setPartyAnim,
   getMonsterHeadPos,

   -- * UI
   setMessage, narrate,
   forcedChoice, maybeChoice, multiChoice,
   ConvChoice(..), forcedConversationLoop,

   -- * Objects
   -- ** Devices
   addDevice_, removeDevice, replaceDevice,
   -- ** Monsters
   addBasicEnemyMonster, summonAllyMonster, tryAddMonster,
   -- ** Items
   grantAndEquipWeapon,

   -- * Other
   inflictAllPeriodicDamage,

   -- * Targeting
   aoeTarget, beamTarget, splashTarget, wallTarget)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (right, second)
import Control.Exception (assert)
import Control.Monad (foldM, forM, forM_, replicateM, unless, when)
import Data.Array (range)
import qualified Data.Foldable as Fold
import Data.List (foldl1')
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Set as Set (empty)

import Fallback.Constants
  (maxAdrenaline, momentsPerActionPoint, sightRangeSquared)
import Fallback.Control.Script
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Data.TotalMap (tmGet)
import Fallback.Scenario.Monsters (getMonsterType)
import Fallback.Scenario.Script.Base
import Fallback.Scenario.Script.Doodad
import Fallback.State.Action (TargetKind(..))
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Item (WeaponData(..))
import Fallback.State.Party
import Fallback.State.Progress (Var)
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags
  (AbilityTag(..), AreaTag, ItemTag(..), MonsterTag, WeaponItemTag)
import Fallback.State.Terrain (TerrainTile, prectRect, terrainMap, tmapGet)
import Fallback.State.Tileset (TileTag, tilesetGet)
import Fallback.Utility
  (ceilDiv, flip3, flip4, groupKey, maybeM, sortKey, square, sumM)

-------------------------------------------------------------------------------
-- Query:

demandMonsterEntry :: (FromAreaEffect f) => Grid.Key Monster
                   -> Script f (Grid.Entry Monster)
demandMonsterEntry key =
  maybe (fail "demandMonsterEntry") return =<< lookupMonsterEntry key

-------------------------------------------------------------------------------
-- Movement:

getPartyPosition :: Script TownEffect Position
getPartyPosition = emitEffect EffGetPartyPosition

setPartyPosition :: Position -> Script TownEffect ()
setPartyPosition = emitEffect . EffSetPartyPosition

partyWalkTo :: Position -> Script TownEffect ()
partyWalkTo pos = do
  oldPos <- getPartyPosition
  facePartyToward pos
  setPartyPosition pos
  emitEffect $ EffSetPartyAnim $ WalkAnim 4 4 oldPos
  wait 4

charWalkTo :: CharacterNumber -> Position -> Script CombatEffect Int
charWalkTo charNum pos = do
  let frames = 4
  oldPos <- areaGet (arsCharacterPosition charNum)
  faceCharacterToward charNum pos
  emitEffect $ EffSetCharPosition charNum pos
  emitEffect $ EffSetCharAnim charNum $ WalkAnim frames frames oldPos
  return frames

teleport :: AreaTag -> Position -> Script TownEffect ()
teleport tag pos = emitEffect $ EffTeleportToArea tag pos

exitTo :: AreaTag -> Script TownEffect ()
exitTo tag = do
  fadeOutMusic 0.8
  emitEffect $ EffExitTowardArea tag

walkMonster :: (FromAreaEffect f) => Int -> Grid.Key Monster -> Position
            -> Script f ()
walkMonster frames gkey pos' = do
  withMonsterEntry gkey $ \entry -> do
    let rect' = makeRect pos' $ rectSize $ Grid.geRect entry
    ok <- emitAreaEffect $ EffTryMoveMonster (Grid.geKey entry) rect'
    when ok $ do
      let deltaX = pointX $ pos' `pSub` rectTopleft (Grid.geRect entry)
      let dir = if deltaX < 0 then FaceLeft else FaceRight
      let anim' = WalkAnim frames frames $ rectTopleft $ Grid.geRect entry
      emitAreaEffect $ EffReplaceMonster (Grid.geKey entry) $
        Just (Grid.geValue entry) { monstAnim = anim', monstFaceDir = dir }
      wait frames

setMonsterTownAI :: (FromAreaEffect f) => Grid.Key Monster -> MonsterTownAI
                 -> Script f ()
setMonsterTownAI key townAI = withMonsterEntry key $ \entry -> do
  emitAreaEffect (EffReplaceMonster key $
                  Just (Grid.geValue entry) { monstTownAI = townAI })

-------------------------------------------------------------------------------
-- Attacks:

determineIfAttackMisses :: Either CharacterNumber (Grid.Key Monster)
                        -> Position -> Bool -> Script CombatEffect Bool
determineIfAttackMisses attacker target isRanged = do
  attackAgil <- do
    case attacker of
      Left charNum -> chrGetStat Agility <$> areaGet (arsGetCharacter charNum)
      Right monstKey -> mtAgility . monstType . Grid.geValue <$>
                        demandMonsterEntry monstKey
  let doMiss = True <$ (playSound =<< getRandomElem [SndMiss1, SndMiss2])
  let missWhen x = x >>= \b -> if b then doMiss else return False
  let doTryAvoid defendAgil = do
        -- TODO take bless/curse into account
        -- TODO take EagleEye rank 1 into account
        let input = fromIntegral (attackAgil - defendAgil) / 15 + 4
        let probMiss = 1 - 0.5 * (1 + input / (1 + abs input))
        missWhen $ randomBool probMiss
  mbOccupant <- areaGet (arsOccupant target)
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      dodge <- do
        if not isRanged then return False else do
        let probMiss = 1 - chrAbilityMultiplier Dodge 0.95 0.9 0.8 char
        -- TODO add doodad saying "Dodge"
        missWhen $ randomBool probMiss
      if dodge then return True else do
      parry <- do
        if isRanged then return False else do
        let probMiss = 1 - chrAbilityMultiplier Parry 0.97 0.94 0.9 char
        -- TODO add doodad saying "Parry"
        -- TODO do we want a different sound?  e.g. clang instead of woosh?
        missWhen $ randomBool probMiss
      if parry then return True else do
      doTryAvoid (chrGetStat Agility char)
    Just (Right monstEntry) -> do
      doTryAvoid $ mtAgility $ monstType $ Grid.geValue monstEntry
    Nothing -> doMiss

characterBeginOffensiveAction :: CharacterNumber -> Position
                              -> Script CombatEffect ()
characterBeginOffensiveAction charNum target = do
  faceCharacterToward charNum target
  alterCharacterStatus charNum $ seSetInvisibility Nothing
  setCharacterAnim charNum (AttackAnim 8)

characterWeaponAttack :: CharacterNumber -> Position -> Script CombatEffect ()
characterWeaponAttack charNum target = do
  char <- areaGet (arsGetCharacter charNum)
  let wd = chrEquippedWeaponData char
  characterWeaponInitialAnimation charNum target wd
  let isRanged = wdRange wd /= Melee
  miss <- determineIfAttackMisses (Left charNum) target isRanged
  unless miss $ do
  (critical, damage) <- characterWeaponChooseCritical char =<<
                        characterWeaponBaseDamage char wd
  -- TODO take EagleEye rank 2 into account for damage
  -- TODO take Backstab into account for damage
  -- TODO take FinalBlow into account somehow
  characterWeaponHit wd target critical damage

characterWeaponInitialAnimation  :: CharacterNumber -> Position
                                 -> WeaponData -> Script CombatEffect ()
characterWeaponInitialAnimation charNum target wd = do
  characterBeginOffensiveAction charNum target
  origin <- areaGet (arsCharacterPosition charNum)
  attackInitialAnimation (wdAppearance wd) (wdElement wd) origin target

characterWeaponBaseDamage :: (FromAreaEffect f) => Character -> WeaponData
                          -> Script f Double
characterWeaponBaseDamage char wd = do
  let dieRoll = uncurry getRandomR (wdDamageRange wd)
  let rollDice n = sum <$> replicateM n dieRoll
  let strength = chrGetStat Strength char
  extraDie <- (strength `mod` 5 >) <$> getRandomR 0 4
  fromIntegral <$> rollDice (wdDamageBonus wd + strength `div` 5 +
                             if extraDie then 1 else 0)

characterWeaponChooseCritical :: (FromAreaEffect f) => Character -> Double
                              -> Script f (Bool, Double)
characterWeaponChooseCritical char damage = do
  critical <- randomBool (1 - 0.998 ^^ chrGetStat Intellect char)
  return (critical, if critical then damage * 1.5 else damage)

characterWeaponHit :: WeaponData -> Position -> Bool -> Double
                   -> Script CombatEffect ()
characterWeaponHit wd target critical damage = do
  attackHit (wdAppearance wd) (wdElement wd) (wdEffects wd)
            target critical damage

monsterBeginOffensiveAction :: Grid.Key Monster -> Position
                            -> Script CombatEffect ()
monsterBeginOffensiveAction key target = do
  faceMonsterToward key target
  alterMonsterStatus key $ seSetInvisibility Nothing
  setMonsterAnim key (AttackAnim 8)

monsterPerformAttack :: Grid.Key Monster -> MonsterAttack -> Position
                     -> Script CombatEffect ()
monsterPerformAttack key attack target = do
  monsterAttackInitialAnimation key attack target
  miss <- determineIfAttackMisses (Right key) target (maRange attack /= Melee)
  unless miss $ do
  (critical, damage) <- monsterAttackChooseCritical attack =<<
                        monsterAttackBaseDamage attack
  monsterAttackHit attack target critical damage

monsterAttackInitialAnimation :: Grid.Key Monster -> MonsterAttack -> Position
                              -> Script CombatEffect ()
monsterAttackInitialAnimation key attack target = do
  monsterBeginOffensiveAction key target
  withMonsterEntry key $ \entry -> do
    attackInitialAnimation (maAppearance attack) (maElement attack)
                           (monsterHeadPos entry) target

monsterAttackBaseDamage :: (FromAreaEffect f) => MonsterAttack
                        -> Script f Double
monsterAttackBaseDamage attack = do
  let dieRoll = uncurry getRandomR (maDamageRange attack)
  let rollDice n = sum <$> replicateM n dieRoll
  fromIntegral <$> rollDice (maDamageCount attack)

monsterAttackChooseCritical :: (FromAreaEffect f) => MonsterAttack -> Double
                            -> Script f (Bool, Double)
monsterAttackChooseCritical attack damage = do
  critical <- randomBool (maCriticalChance attack)
  return (critical, if critical then damage * 1.5 else damage)

monsterAttackHit :: MonsterAttack -> Position -> Bool -> Double
                 -> Script CombatEffect ()
monsterAttackHit ma target critical damage = do
  attackHit (maAppearance ma) (maElement ma) (maEffects ma)
            target critical damage

attackInitialAnimation :: (FromAreaEffect f) => AttackAppearance
                       -> AttackElement -> Position -> Position -> Script f ()
attackInitialAnimation appearance element origin target = do
  case appearance of
    BiteAttack -> return ()
    BladeAttack -> return ()
    BluntAttack -> return ()
    BowAttack -> do
      playSound SndArrow
      addBallisticDoodad ArrowProj origin target 250 >>= wait
    BreathAttack -> do
      let proj = case element of
                   AcidAttack -> AcidProj
                   EnergyAttack -> IceProj -- FIXME
                   FireAttack -> FireProj
                   IceAttack -> IceProj
                   PhysicalAttack -> StarProj
      playSound SndBreath
      addBallisticDoodad proj origin target 220 >>= wait
    ClawAttack -> return ()
    ThrownAttack -> do
      playSound SndThrow
      addBallisticDoodad StarProj origin target 200 >>= wait
    WandAttack -> do
      let tint = case element of
                   AcidAttack -> Tint 0 255 0 192
                   EnergyAttack -> Tint 0 0 255 192
                   FireAttack -> Tint 255 0 0 192
                   IceAttack -> Tint 0 255 255 192
                   PhysicalAttack -> Tint 255 255 255 192
      addLightningDoodad tint origin target

attackHit :: AttackAppearance -> AttackElement -> [AttackEffect] -> Position
          -> Bool -> Double -> Script CombatEffect ()
attackHit appearance element effects target critical damage = do
  -- TODO take attack effects into account (in addition to main attack element)
  -- when choosing sound/doodad
  let elementSnd AcidAttack = SndChemicalDamage
      elementSnd FireAttack = if critical then SndBoomSmall else SndFireDamage
      elementSnd PhysicalAttack = if critical then SndHit3 else SndHit4
      elementSnd _ = error "FIXME attackHit"
  playSound =<<
    case appearance of
      BiteAttack -> return SndBite
      BowAttack -> if critical then return SndHit2 else return SndHit1
      BladeAttack -> if critical then return SndHit4
                     else getRandomElem [SndHit1, SndHit2, SndHit3]
      BluntAttack -> if critical then return SndHit2 else return SndHit1
      BreathAttack -> return $ elementSnd element
      ClawAttack -> return SndClaw
      ThrownAttack -> if critical then return SndHit2 else return SndHit1
      WandAttack -> return $ elementSnd element
  let elementBoom = do
        let boom = case element of
                     AcidAttack -> AcidBoom
                     EnergyAttack -> EnergyBoom
                     FireAttack -> FireBoom
                     IceAttack -> IceBoom
                     PhysicalAttack -> SlashRight
        addBoomDoodadAtPosition boom (if critical then 3 else 2) target
  case appearance of
    BiteAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    BowAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    BladeAttack -> do
      addBoomDoodadAtPosition SlashRight 2 target
      when critical $ addBoomDoodadAtPosition SlashLeft 3 target
    BluntAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    BreathAttack -> elementBoom
    ClawAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    ThrownAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    WandAttack -> elementBoom
  let hitTarget = HitPosition target
  extraHits <- flip3 foldM [] effects $ \hits effect -> do
    case effect of
      DrainMana mult ->
        hits <$ alterMana hitTarget (subtract $ round $ mult * damage)
      ExtraAcidDamage mult -> do
        return ((hitTarget, AcidDamage, mult * damage) : hits)
      ExtraEnergyDamage mult -> do
        return ((hitTarget, EnergyDamage, mult * damage) : hits)
      ExtraFireDamage mult -> do
        return ((hitTarget, FireDamage, mult * damage) : hits)
      ExtraIceDamage mult -> do
        return ((hitTarget, ColdDamage, mult * damage) : hits)
      InflictPoison mult -> hits <$ inflictPoison hitTarget (mult * damage)
      InflictStun mult -> hits <$ inflictStun hitTarget (mult * damage)
      _ -> return hits -- FIXME
  let damageElement =
        case element of
          AcidAttack -> AcidDamage
          EnergyAttack -> EnergyDamage
          FireAttack -> FireDamage
          IceAttack -> ColdDamage
          PhysicalAttack -> PhysicalDamage
  dealDamage ((hitTarget, damageElement, damage) : extraHits)
  wait 16

-------------------------------------------------------------------------------
-- Damage:

dealDamage :: (FromAreaEffect f) => [(HitTarget, DamageType, Double)]
           -> Script f ()
dealDamage hits = dealDamageTotal hits >> return ()

-- | Like 'dealDamage', but return the total damage done to all targets (after
-- resistances).
dealDamageTotal :: (FromAreaEffect f) => [(HitTarget, DamageType, Double)]
                -> Script f Int
dealDamageTotal = dealDamageGeneral False

dealDamageGeneral :: (FromAreaEffect f) => Bool
                  -> [(HitTarget, DamageType, Double)] -> Script f Int
dealDamageGeneral gentle hits = do
  let convert (hitTarget, dmgType, damage) = do
        mbOccupant <- getHitTargetOccupant hitTarget
        return $ case mbOccupant of
                   Just occupant -> Just (occupant, dmgType, damage)
                   Nothing -> Nothing
  let resist (occupant, dmgType, damage) = do
        -- Stun is measured in action points.  We do 1 AP stun per 100 damage.
        let stun = if gentle then 0 else damage * 0.01
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
          Left charNum -> dealRawDamageToCharacter gentle charNum damage stun
          Right monstEntry ->
            dealRawDamageToMonster gentle (Grid.geKey monstEntry) damage stun
  sumM inflict . map (foldl1' combine) . groupKey keyFn . sortKey keyFn =<<
    (mapM resist . catMaybes =<< mapM convert hits)

-- | Inflict damage and stun on a character, ignoring armor and resistances.
-- Stun is measured in action points.
dealRawDamageToCharacter :: (FromAreaEffect f) => Bool -> CharacterNumber
                         -> Int -> Double -> Script f Int
dealRawDamageToCharacter gentle charNum damage stun = do
  pos <- areaGet (arsCharacterPosition charNum)
  char <- areaGet (arsGetCharacter charNum)
  -- If this is non-gentle damage, wake us up from being dazed, and add
  -- adrenaline.
  unless gentle $ do
    alterCharacterStatus charNum seWakeFromDaze
    party <- areaGet arsParty
    let maxHealth = chrMaxHealth party $ partyGetCharacter party charNum
    -- TODO take Valiance into account
    alterAdrenaline charNum (+ adrenalineForDamage damage maxHealth)
  -- Do stun (if in combat):
  when (stun > 0) $ whenCombat $ do
    moments <- emitEffect $ EffGetCharMoments charNum
    emitEffect $ EffSetCharMoments charNum $
      max 0 (moments - round (stun * fromIntegral momentsPerActionPoint))
  -- Do damage:
  minHealth <- emitAreaEffect $ EffIfCombat (return 0) (return 1)
  let health' = max minHealth (chrHealth char - damage)
  emitAreaEffect $ EffAlterCharacter charNum $ \c -> c { chrHealth = health' }
  emitAreaEffect $ EffIfCombat (setCharacterAnim charNum $ HurtAnim 12)
                               (setPartyAnim $ HurtAnim 12)
  unless (gentle && damage == 0) $ do
    addNumberDoodadAtPosition damage pos
  -- If we're in combat, the character can die:
  whenCombat $ when (health' <= 0) $ do
    alterCharacterMojo charNum (const 0)
    alterAdrenaline charNum (const 0)
    alterCharacterStatus charNum (const initStatusEffects)
    resources <- areaGet arsResources
    let images = rsrcCharacterImages resources (chrClass char)
                                     (chrAppearance char)
    faceDir <- emitEffect $ EffGetCharFaceDir charNum
    playSound (characterScreamSound char)
    playSound SndDie1
    addDeathDoodad images faceDir (makeRect pos (1, 1))
    -- If all characters are now dead, it's game over:
    alive <- areaGet (Fold.any chrIsConscious . partyCharacters . arsParty)
    unless alive $ emitAreaEffect EffGameOver
  return damage

-- | Inflict damage and stun on a monster, ignoring armor and resistances.
-- Stun is measured in action points.  The monster must exist.
dealRawDamageToMonster :: (FromAreaEffect f) => Bool -> Grid.Key Monster
                       -> Int -> Double -> Script f Int
dealRawDamageToMonster gentle key damage stun = do
  entry <- demandMonsterEntry key
  let monst = Grid.geValue entry
  -- If this is non-gentle damage, wake us up from being dazed and add
  -- adrenaline.
  unless gentle $ alterMonsterStatus key seWakeFromDaze
  let adrenaline' = monstAdrenaline monst +
        if gentle then 0 else
          adrenalineForDamage damage $ mtMaxHealth $ monstType monst
  -- Do damage and stun:
  let health' = monstHealth monst - damage
  let moments' = max 0 (monstMoments monst -
                        round (stun * fromIntegral momentsPerActionPoint))
  let mbMonst' = if health' <= 0 then Nothing else
                   Just monst { monstAdrenaline = adrenaline',
                                monstAnim = HurtAnim 12, monstHealth = health',
                                monstMoments = moments' }
  emitAreaEffect $ EffReplaceMonster key mbMonst'
  unless (gentle && damage == 0) $ do
    addNumberDoodadAtPoint damage $ rectCenter $ prectRect $ Grid.geRect entry
  -- If the monster is now dead, add a death doodad, set the monster's
  -- "dead" var (if any) to True, and grant experience if it's an enemy.
  when (isNothing mbMonst') $ do
    resources <- areaGet arsResources
    let mtype = monstType monst
    addDeathDoodad (rsrcMonsterImages resources (mtSize mtype)
                                      (mtImageRow mtype))
                   (monstFaceDir monst) (Grid.geRect entry)
    maybeM (monstDeadVar monst) (emitAreaEffect . flip EffSetVar True)
    unless (monstIsAlly monst) $ do
      grantExperience $ mtExperienceValue $ monstType monst
  return damage

adrenalineForDamage :: Int -> Int -> Int
adrenalineForDamage damage maxHealth =
  round $ (fromIntegral maxAdrenaline *) $ square $ min 1 $
  2 * fromIntegral damage / (fromIntegral maxHealth :: Double)

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
  multiplier <- chrAbilityMultiplier Recuperation 1.1 1.2 1.4 <$>
                areaGet (arsGetCharacter charNum)
  let amount = max 0 $ round (multiplier * baseAmount)
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrHealth = min (chrMaxHealth party char)
                           (chrHealth char + amount) }
  pos <- areaGet (arsCharacterPosition charNum)
  addBoomDoodadAtPosition HealBoom 4 pos
  addNumberDoodadAtPosition amount pos

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
  addNumberDoodadAtPoint amount $ rectCenter $ prectRect prect

-------------------------------------------------------------------------------
-- Mojo/adrenaline:

alterMana :: (FromAreaEffect f) => HitTarget -> (Int -> Int) -> Script f ()
alterMana hitTarget fn = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      case chrClass char of
        ClericClass -> alterCharacterMojo charNum fn
        MagusClass -> alterCharacterMojo charNum fn
        _ -> return ()
    _ -> return ()

alterCharacterMojo :: (FromAreaEffect f) => CharacterNumber -> (Int -> Int)
                   -> Script f ()
alterCharacterMojo charNum fn = do
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrMojo = max 0 $ min (chrMaxMojo party char) $ fn (chrMojo char) }

restoreMojoToFull :: (FromAreaEffect f) => CharacterNumber -> Script f ()
restoreMojoToFull charNum = do
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrMojo = chrMaxMojo party char }

alterAdrenaline :: (FromAreaEffect f) => CharacterNumber -> (Int -> Int)
                -> Script f ()
alterAdrenaline charNum fn =
  emitAreaEffect $ EffAlterCharacter charNum $ \c ->
    c { chrAdrenaline = max 0 $ min maxAdrenaline $ fn (chrAdrenaline c) }

-------------------------------------------------------------------------------
-- Status effects:

-- | Directly alters the status effects of the target.
alterStatus :: (FromAreaEffect f) => HitTarget
            -> (StatusEffects -> StatusEffects) -> Script f ()
alterStatus hitTarget fn = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> alterCharacterStatus charNum fn
    Just (Right monstEntry) -> alterMonsterStatus (Grid.geKey monstEntry) fn
    Nothing -> return ()

alterCharacterStatus :: (FromAreaEffect f) => CharacterNumber
                     -> (StatusEffects -> StatusEffects) -> Script f ()
alterCharacterStatus charNum fn = do
  fn' <- emitAreaEffect $ EffIfCombat (return fn) (return (townifyStatus . fn))
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrStatus = fn' (chrStatus char) }

alterMonsterStatus :: (FromAreaEffect f) => Grid.Key Monster
                   -> (StatusEffects -> StatusEffects) -> Script f ()
alterMonsterStatus key fn = do
  fn' <- emitAreaEffect $ EffIfCombat (return fn) (return (townifyStatus . fn))
  mbMonst <- fmap Grid.geValue <$> lookupMonsterEntry key
  let mbMonst' = (\m -> m { monstStatus = fn' (monstStatus m) }) <$> mbMonst
  emitAreaEffect $ EffReplaceMonster key mbMonst'

-- | Grants the given level of invisibility to the target, unless the target is
-- already more invisible.
grantInvisibility :: (FromAreaEffect f) => HitTarget -> Invisibility
                  -> Script f ()
grantInvisibility hitTarget invis = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      status <- chrStatus <$> areaGet (arsGetCharacter charNum)
      when (seInvisibility status < Just invis) $ do
        alterCharacterStatus charNum $ seSetInvisibility $ Just invis
    Just (Right monstEntry) -> do
      let status = monstStatus $ Grid.geValue monstEntry
      when (seInvisibility status < Just invis) $ do
        alterMonsterStatus (Grid.geKey monstEntry) $ seSetInvisibility $
          Just invis
    Nothing -> return ()

-- | Inflicts poison damage (as opposed to direct damage) onto the target,
-- taking resistances into account.
inflictPoison :: (FromAreaEffect f) => HitTarget -> Double -> Script f ()
inflictPoison hitTarget basePoison = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      let poison = round (basePoison * chrGetResistance ResistChemical char)
      alterCharacterStatus charNum $ seAlterPoison (poison +)
    Just (Right monstEntry) -> do
      let poison = round $ (basePoison *) $ tmGet ResistChemical $
                   mtResistances $ monstType $ Grid.geValue monstEntry
      alterMonsterStatus (Grid.geKey monstEntry) $ seAlterPoison (poison +)
    Nothing -> return ()

-- | Inflicts stun (measured in action points) onto the target, taking
-- resistances into account.
inflictStun :: HitTarget -> Double -> Script CombatEffect ()
inflictStun hitTarget stun = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      let stun' = max 0 (stun * chrGetResistance ResistStun char)
      moments <- emitEffect $ EffGetCharMoments charNum
      emitEffect $ EffSetCharMoments charNum $
        max 0 (moments - round (stun' * fromIntegral momentsPerActionPoint))
    Just (Right monstEntry) -> do
      let monst = Grid.geValue monstEntry
      let stun' = max 0 $ (stun *) $ tmGet ResistStun $ mtResistances $
                  monstType monst
      emitAreaEffect $ EffReplaceMonster (Grid.geKey monstEntry) $ Just monst
        { monstMoments = monstMoments monst -
                         round (stun' * fromIntegral momentsPerActionPoint) }
    Nothing -> return ()

-------------------------------------------------------------------------------
-- Camera motion:

shakeCamera :: (FromAreaEffect f) => Double -> Int -> Script f ()
shakeCamera amplitude duration = do
  emitAreaEffect $ EffShakeCamera amplitude duration

-------------------------------------------------------------------------------
-- Creature animation:

faceCharacterToward :: CharacterNumber -> Position -> Script CombatEffect ()
faceCharacterToward charNum pos = do
  deltaX <- pointX . (pos `pSub`) <$> areaGet (arsCharacterPosition charNum)
  when (deltaX /= 0) $ do
    let dir = if deltaX < 0 then FaceLeft else FaceRight
    emitEffect $ EffSetCharFaceDir charNum dir

faceMonsterToward :: (FromAreaEffect f) => Grid.Key Monster -> Position
                  -> Script f ()
faceMonsterToward key pos = do
  withMonsterEntry key $ \entry -> do
    let deltaX = pointX $ (pos `pSub`) $ monsterHeadPos entry
    when (deltaX /= 0) $ do
      let dir = if deltaX < 0 then FaceLeft else FaceRight
      emitAreaEffect $ EffReplaceMonster key $
        Just (Grid.geValue entry) { monstFaceDir = dir }

facePartyToward :: Position -> Script TownEffect ()
facePartyToward pos = do
  deltaX <- pointX . (pos `pSub`) <$> getPartyPosition
  when (deltaX /= 0) $ do
    let dir = if deltaX < 0 then FaceLeft else FaceRight
    emitEffect $ EffSetPartyFaceDir dir

setCharacterAnim :: CharacterNumber -> CreatureAnim -> Script CombatEffect ()
setCharacterAnim charNum anim = emitEffect $ EffSetCharAnim charNum anim

setMonsterAnim :: (FromAreaEffect f) => Grid.Key Monster -> CreatureAnim
               -> Script f ()
setMonsterAnim key anim = do
  withMonsterEntry key $ \entry -> do
    let monst = Grid.geValue entry
    emitAreaEffect $ EffReplaceMonster key $ Just monst { monstAnim = anim }

setPartyAnim :: CreatureAnim -> Script TownEffect ()
setPartyAnim = emitEffect . EffSetPartyAnim

getMonsterHeadPos :: (FromAreaEffect f) => Grid.Key Monster
                  -> Script f Position
getMonsterHeadPos key = do
  entry <- demandMonsterEntry key
  return $ monsterHeadPos entry

-------------------------------------------------------------------------------
-- Messages and conversation:

setMessage :: (FromAreaEffect f) => String -> Script f ()
setMessage = emitAreaEffect . EffMessage

narrate :: (FromAreaEffect f) => String -> Script f ()
narrate = emitAreaEffect . EffNarrate

-- | Put up a conversation dialog, with no option for the player to not answer.
forcedChoice :: (FromAreaEffect f) => String -> [(String, a)] -> Script f a
forcedChoice text choices =
  emitAreaEffect $ EffMultiChoice text choices Nothing

-- | Put up a conversation dialog, with an option for the player to not answer
-- (in which case the result is 'Nothing').
maybeChoice :: (FromAreaEffect f) => String -> [(String, a)]
            -> Script f (Maybe a)
maybeChoice text choices =
  emitAreaEffect $ EffMultiChoice text (map (second Just) choices) Nothing

-- | Put up a conversation dialog, with an option for the player to not answer
-- (in which case the given default value is used).
multiChoice :: (FromAreaEffect f) => String -> [(String, a)] -> a -> Script f a
multiChoice text choices cancel =
  emitAreaEffect $ EffMultiChoice text choices $ Just cancel

data ConvChoice a = ContinueConv String [(String, ConvChoice a)]
                  | StopConv a

forcedConversationLoop :: (FromAreaEffect f) => String
                       -> [(String, ConvChoice a)] -> Script f a
forcedConversationLoop = (choice [] [] .) . ContinueConv where
  choice _ _ (StopConv a) = return a
  choice before after (ContinueConv text options) = do
    let options' = before ++ options ++ after
    idx <- forcedChoice text $ zip (map fst options') [0..]
    let (before', rest) = splitAt idx options'
    let after' = tail rest
    choice before' after' (snd $ head rest)

-------------------------------------------------------------------------------
-- Other script actions:

addBasicEnemyMonster :: (FromAreaEffect f) => Position -> MonsterTag
                     -> Maybe (Var Bool) -> MonsterTownAI -> Script f ()
addBasicEnemyMonster nearPos tag mbDeadVar townAi = do
  let mtype = getMonsterType tag
  within <- areaGet arsBoundaryRect
  -- TODO Allow for non-SizeSmall monsters
  spot <- areaGet $ \ars ->
    if not $ arsIsBlockedForParty ars nearPos then nearPos
    else arsFindOpenSpot ars nearPos within Set.empty
  _ <- tryAddMonster spot Monster
         { monstAnim = NoAnim,
           monstAdrenaline = 0,
           monstDeadVar = mbDeadVar,
           monstFaceDir = FaceLeft,
           monstHealth = mtMaxHealth mtype,
           monstIsAlly = False,
           monstMoments = 0,
           monstName = mtName mtype,
           monstScript = Nothing,
           monstStatus = initStatusEffects,
           monstTag = tag,
           monstTownAI = townAi,
           monstType = mtype }
  return ()

addDevice_ :: (FromAreaEffect f) => Device -> Position -> Script f ()
addDevice_ device pos = () <$ emitAreaEffect (EffTryAddDevice pos device)

getTerrainTile :: (FromAreaEffect f) => TileTag -> Script f TerrainTile
getTerrainTile tag = do
  resources <- areaGet arsResources
  return $ tilesetGet tag $ rsrcTileset resources

grantAndEquipWeapon :: (FromAreaEffect f) => WeaponItemTag -> CharacterNumber
                    -> Script f ()
grantAndEquipWeapon tag charNum = do
  party <- areaGet arsParty
  let char = partyGetCharacter party charNum
  let mbOldItem = fmap WeaponItemTag $ eqpWeapon $ chrEquipment char
  emitAreaEffect $ EffAlterCharacter charNum (\c -> c { chrEquipment =
    (chrEquipment c) { eqpWeapon = Just tag } })
  maybeM mbOldItem (emitAreaEffect . EffGrantItem)

grantExperience :: (FromAreaEffect f) => Int -> Script f ()
grantExperience xp = do
  oldLevel <- areaGet (partyLevel . arsParty)
  emitAreaEffect $ EffGrantExperience xp
  newLevel <- areaGet (partyLevel . arsParty)
  when (newLevel > oldLevel) $ do
    setMessage $ "Party is now level " ++ show newLevel ++ "!"
    mapM_ return =<< areaGet arsPartyPositions -- FIXME doodads
    playSound SndLevelUp

removeDevice :: (FromAreaEffect f) => Grid.Key Device -> Script f ()
removeDevice key = emitAreaEffect $ EffReplaceDevice key Nothing

removeFields :: (FromAreaEffect f) => [Position] -> Script f ()
removeFields = emitAreaEffect . EffAlterFields (const Nothing)

replaceDevice :: (FromAreaEffect f) => Grid.Entry Device -> Device
              -> Script f ()
replaceDevice entry device =
  emitAreaEffect $ EffReplaceDevice (Grid.geKey entry) (Just device)

resetTerrain :: (FromAreaEffect f) => [Position] -> Script f ()
resetTerrain positions = do
  tmap <- areaGet (terrainMap . arsTerrain)
  let update pos = (pos, tmapGet tmap pos)
  emitAreaEffect $ EffSetTerrain $ map update positions

setFields :: (FromAreaEffect f) => Field -> [Position] -> Script f ()
setFields field = emitAreaEffect . EffAlterFields fn where
  fn Nothing = Just field
  fn (Just field') = Just $
    case (field', field) of
      (BarrierWall a, BarrierWall b) -> BarrierWall (max a b)
      (BarrierWall a, _) -> BarrierWall a
      (FireWall a, FireWall b) -> FireWall (max a b)
      (IceWall a, IceWall b) -> IceWall (max a b)
      (PoisonCloud a, PoisonCloud b) -> PoisonCloud (max a b)
      (SmokeScreen a, SmokeScreen b) -> SmokeScreen (max a b)
      (Webbing a, Webbing b) -> Webbing (max a b)
      _ -> field

setTerrain :: (FromAreaEffect f) => [(Position, TerrainTile)] -> Script f ()
setTerrain = emitAreaEffect . EffSetTerrain

summonAllyMonster :: (FromAreaEffect f) => Position -> MonsterTag
                  -> Script f ()
summonAllyMonster startPos tag = do
  let mtype = getMonsterType tag
  arena <- areaGet arsBoundaryRect
  -- TODO Allow for non-SizeSmall monsters
  spot <- areaGet (flip4 arsFindOpenSpot startPos arena Set.empty)
  _ <- tryAddMonster spot Monster
         { monstAnim = NoAnim,
           monstAdrenaline = 0,
           monstDeadVar = Nothing,
           monstFaceDir = FaceLeft, -- TODO
           monstHealth = mtMaxHealth mtype,
           monstIsAlly = True,
           monstMoments = 0,
           monstName = "Summoned " ++ mtName mtype,
           monstScript = Nothing,
           monstStatus = initStatusEffects,
           monstTag = tag,
           monstTownAI = ChaseAI,
           monstType = mtype }
  return ()

tryAddMonster :: (FromAreaEffect f) => Position -> Monster
              -> Script f (Maybe (Grid.Entry Monster))
tryAddMonster position monster = do
  emitAreaEffect $ EffTryAddMonster position monster
{-
tryAddMonster :: (FromAreaEffect f) => MonsterTag -> MonsterType -> Position
              -> Maybe (Var Bool) -> MonsterTownAI -> Maybe MonsterScript
              -> Script f (Maybe (Entry Monster))
tryAddMonster tag mtype position mbDeadVar ai mbMscript = do
  emitAreaEffect $ EffTryAddMonster position $ Monster
    { monstAnim = NoAnim,
      monstAdrenaline = 0,
      monstDeadVar = mbDeadVar,
      monstFaceDir = FaceLeft,
      monstHealth = mtMaxHealth mtype,
      monstIsAlly = False,
      monstMoments = 0,
      monstName = mtName mtype,
      monstScript = mbMscript,
      monstStatus = initStatusEffects,
      monstTag = tag,
      monstTownAI = ai,
      monstType = mtype }
-}

-------------------------------------------------------------------------------

inflictAllPeriodicDamage :: (FromAreaEffect f) => Script f ()
inflictAllPeriodicDamage = do
  fields <- Map.assocs <$> areaGet arsFields
  fieldDamages <- fmap catMaybes $ forM fields $ \(pos, field) -> do
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
      Webbing ent -> do
        -- FIXME if something's there, remove the webbing
        Nothing <$ alterStatus (HitPosition pos) (seApplyEntanglement ent)
  charNums <- getAllConsciousCharacters
  party <- areaGet arsParty
  charPoisonDamages <- fmap catMaybes $ forM charNums $ \charNum -> do
    let totalPoison = sePoison $ chrStatus $ partyGetCharacter party charNum
    if totalPoison <= 0 then return Nothing else do
      let damage = totalPoison `ceilDiv` 5
      alterCharacterStatus charNum $ seAlterPoison $ subtract damage
      return $ Just (HitCharacter charNum, RawDamage, fromIntegral damage)
  monsters <- Grid.entries <$> areaGet arsMonsters
  monstPoisonDamages <- fmap catMaybes $ forM monsters $ \monstEntry -> do
    let totalPoison = sePoison $ monstStatus $ Grid.geValue monstEntry
    if totalPoison <= 0 then return Nothing else do
      let damage = totalPoison `ceilDiv` 5
      alterMonsterStatus (Grid.geKey monstEntry) $ seAlterPoison $
        subtract damage
      return $ Just (HitMonster (Grid.geKey monstEntry), RawDamage,
                     fromIntegral damage)
  dealDamageGeneral True (fieldDamages ++ charPoisonDamages ++
                          monstPoisonDamages) >> return ()

-------------------------------------------------------------------------------
-- Targeting:

circleArea :: SqDist -> Position -> [Position]
circleArea dist center =
  let limit = floor $ sqrt (fromIntegral dist :: Double)
      corner = Point limit limit
  in filter ((dist >=) . pSqDist center) $
     range (center `pSub` corner, center `pAdd` corner)

aoeTarget :: Int -> SqDist -> TargetKind (Position, [Position])
aoeTarget maxRange blastRadiusSquared = AreaTarget (ofRadius maxRange) fn
  where fn _ars _origin target = circleArea blastRadiusSquared target

beamTarget :: TargetKind (Position, [Position])
beamTarget = AreaTarget sightRangeSquared arsBeamPositions

splashTarget :: Int -> TargetKind (Position, [Position])
splashTarget maxRange = AreaTarget (ofRadius maxRange) fn where
  fn ars origin target =
    if origin == target || cannotSeeThrough (arsTerrainOpenness target ars)
    then [target] else
      let dir = ipointDir (target `pSub` origin)
      in [target, target `plusDir` pred dir, target `plusDir` dir,
          target `plusDir` succ dir]

wallTarget :: Int -> Int -> TargetKind (Position, [Position])
wallTarget maxRange radius = AreaTarget (ofRadius maxRange) fn where
  fn ars origin target =
    if origin == target || blocked target then [] else
      let (d1, d2, d3, d4) =
            if isCardinal dir
            then (pred $ pred dir, pred $ pred dir,
                  succ $ succ dir, succ $ succ dir)
            else (pred dir, pred $ pred $ pred dir,
                  succ dir, succ $ succ $ succ dir)
      in target : wing d1 d2 target radius ++ wing d3 d4 target radius
    where
      dir = ipointDir (target `pSub` origin)
      blocked pos =
        isJust (arsOccupant pos ars) ||
        case arsTerrainOpenness pos ars of
          TerrainOpen -> False
          TerrainHover -> False
          _ -> True
      wing dir1 dir2 start n =
        if n <= 0 then [] else
          let pos = start `plusDir` (if n `mod` 2 == 1 then dir1 else dir2)
          in if blocked pos then [] else pos : wing dir1 dir2 pos (n - 1)

-------------------------------------------------------------------------------
-- Private:

applyResistances :: StatusEffects -> Resistances -> DamageType -> Double
                 -> Double -> (Int, Double)
applyResistances status resist dmgType damage stun = (damage', stun')
  where
    armor = tmGet Armor resist * seArmorMultiplier status
    magicArmor = assert (armor >= 0) (sqrt armor) *
                 seMagicShieldMultiplier status
    damage' = round $ (damage *) $
              case dmgType of
                AcidDamage -> tmGet ResistChemical resist * magicArmor
                ColdDamage -> tmGet ResistCold resist * magicArmor
                EnergyDamage -> tmGet ResistEnergy resist * magicArmor
                FireDamage -> tmGet ResistFire resist * magicArmor
                MagicDamage -> magicArmor
                PhysicalDamage -> armor
                RawDamage -> 1
    stun' = stun * tmGet ResistStun resist

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

monsterHeadPos :: Grid.Entry Monster -> Position
monsterHeadPos entry =
  let Rect x y w _ = Grid.geRect entry
  in case monstFaceDir (Grid.geValue entry) of
       FaceLeft -> Point x y
       FaceRight -> Point (x + w - 1) y

-------------------------------------------------------------------------------
