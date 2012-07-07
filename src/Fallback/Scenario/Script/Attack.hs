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

module Fallback.Scenario.Script.Attack
  (characterOffensiveAction, characterOffensiveActionTowards,
   characterWeaponAttack, characterWeaponInitialAnimation,
   characterWeaponBaseDamage, characterWeaponChooseCritical,
   characterWeaponHit,
   monsterOffensiveAction, monsterOffensiveActionToward, monsterPerformAttack)
where

import Control.Applicative ((<$), (<$>))
import Control.Monad (foldM, replicateM, unless, when)

import Fallback.Control.Script
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script.Base
import Fallback.Scenario.Script.Damage (dealDamage, killTarget)
import Fallback.Scenario.Script.Doodad
import Fallback.Scenario.Script.Other
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Item (DamageModifier(..), WeaponData(..))
import Fallback.State.Party
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags (AbilityTag(..))
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

characterOffensiveAction :: (FromAreaEffect f) => CharacterNumber -> Int
                         -> Script f ()
characterOffensiveAction charNum frames = do
  alterStatus (HitCharacter charNum) (seSetInvisibility NoInvisibility)
  setCharacterAnim charNum (AttackAnim frames)

characterOffensiveActionTowards :: (FromAreaEffect f) => CharacterNumber
                                -> Int -> Position -> Script f ()
characterOffensiveActionTowards charNum frames target = do
  faceCharacterToward charNum target
  characterOffensiveAction charNum frames

characterWeaponAttack :: CharacterNumber -> Position -> Script CombatEffect ()
characterWeaponAttack charNum target = do
  characterOffensiveActionTowards charNum 8 target
  char <- areaGet (arsGetCharacter charNum)
  let wd = chrEquippedWeaponData char
  characterWeaponInitialAnimation charNum target wd
  let isRanged = wdRange wd /= Melee
  miss <- determineIfAttackMisses (Left charNum) target isRanged
  unless miss $ do
  (critical, damage) <- characterWeaponChooseCritical char =<<
                        characterWeaponBaseDamage char wd
  origin <- areaGet (arsCharacterPosition charNum)
  -- TODO take Backstab into account for damage
  -- TODO take FinalBlow into account somehow
  characterWeaponHit wd origin target critical damage

characterWeaponInitialAnimation  :: CharacterNumber -> Position
                                 -> WeaponData -> Script CombatEffect ()
characterWeaponInitialAnimation charNum target wd = do
  origin <- areaGet (arsCharacterPosition charNum)
  attackInitialAnimation (wdAppearance wd) (wdElement wd) origin target

characterWeaponBaseDamage :: (FromAreaEffect f) => Character -> WeaponData
                          -> Script f Double
characterWeaponBaseDamage char wd = do
  let dieRoll = uncurry getRandomR (wdDamageRange wd)
  let rollDice n = sum <$> replicateM n dieRoll
  let strength = chrGetStat Strength char
  extraDie <- (strength `mod` 5 >) <$> getRandomR 0 4
  damage <- fromIntegral <$>
    rollDice (wdDamageBonus wd + strength `div` 5 + if extraDie then 1 else 0)
  return (damage * chrWeaponDamageMultiplier char)

characterWeaponChooseCritical :: (FromAreaEffect f) => Character -> Double
                              -> Script f (Bool, Double)
characterWeaponChooseCritical char damage = do
  critical <- randomBool (1 - 0.998 ^^ chrGetStat Intellect char)
  return (critical, if critical then damage * 1.5 else damage)

characterWeaponHit :: WeaponData -> Position -> Position -> Bool -> Double
                   -> Script CombatEffect ()
characterWeaponHit wd origin target critical damage = do
  mbOccupant <- areaGet (arsOccupant target)
  let multOrKill ZeroDamage = Just 0
      multOrKill HalfDamage = Just 0.5
      multOrKill NormalDamage = Just 1
      multOrKill DoubleDamage = Just 2
      multOrKill InstantKill = Nothing
  let mbMult =
        case mbOccupant of
          Nothing -> Just 1
          Just (Left _) -> multOrKill (wdVsHuman wd)
          Just (Right entry) ->
            let mtype = monstType $ Grid.geValue entry
                dmgVs wdFn mtFn =
                  if mtFn mtype then multOrKill (wdFn wd) else Just 1
            in fmap product $ sequence $
               [dmgVs wdVsDaemonic mtIsDaemonic, dmgVs wdVsHuman mtIsHuman,
                dmgVs wdVsUndead mtIsUndead]
  case mbMult of
    Just mult -> attackHit True (wdAppearance wd) (wdElement wd) (wdEffects wd)
                           origin target critical (mult * damage)
    Nothing -> instantKill (wdAppearance wd) (wdElement wd) target critical

-------------------------------------------------------------------------------

monsterOffensiveAction :: Grid.Key Monster -> Int -> Script CombatEffect ()
monsterOffensiveAction key frames = do
  alterStatus (HitMonster key) (seSetInvisibility NoInvisibility)
  setMonsterAnim key (AttackAnim frames)

monsterOffensiveActionToward :: Grid.Key Monster -> Int -> Position
                             -> Script CombatEffect ()
monsterOffensiveActionToward key frames target = do
  faceMonsterToward key target
  monsterOffensiveAction key frames

monsterPerformAttack :: Grid.Key Monster -> MonsterAttack -> Position
                     -> Script CombatEffect ()
monsterPerformAttack key attack target = do
  monsterOffensiveActionToward key 8 target
  monsterAttackInitialAnimation key attack target
  miss <- determineIfAttackMisses (Right key) target (maRange attack /= Melee)
  unless miss $ do
  (critical, damage) <- monsterAttackChooseCritical attack =<<
                        monsterAttackBaseDamage key attack
  origin <- getMonsterHeadPos key
  monsterAttackHit key attack origin target critical damage

monsterAttackInitialAnimation :: Grid.Key Monster -> MonsterAttack -> Position
                              -> Script CombatEffect ()
monsterAttackInitialAnimation key attack target = do
  withMonsterEntry key $ \entry -> do
    attackInitialAnimation (maAppearance attack) (maElement attack)
                           (monstHeadPos entry) target

monsterAttackBaseDamage :: (FromAreaEffect f) => Grid.Key Monster
                        -> MonsterAttack -> Script f Double
monsterAttackBaseDamage key attack = do
  let dieRoll = uncurry getRandomR (maDamageRange attack)
  let rollDice n = sum <$> replicateM n dieRoll
  damage <- fromIntegral <$> rollDice (maDamageCount attack)
  status <- maybe initStatusEffects (monstStatus . Grid.geValue) <$>
            lookupMonsterEntry key
  return (damage * seAttackDamageMultiplier status)

monsterAttackChooseCritical :: (FromAreaEffect f) => MonsterAttack -> Double
                            -> Script f (Bool, Double)
monsterAttackChooseCritical attack damage = do
  critical <- randomBool (maCriticalChance attack)
  return (critical, if critical then damage * 1.5 else damage)

monsterAttackHit :: Grid.Key Monster -> MonsterAttack -> Position -> Position
                 -> Bool -> Double -> Script CombatEffect ()
monsterAttackHit key ma origin target critical damage = do
  isAlly <- maybe False (monstIsAlly . Grid.geValue) <$> lookupMonsterEntry key
  attackHit isAlly (maAppearance ma) (maElement ma) (maEffects ma) origin
            target critical damage

-------------------------------------------------------------------------------

attackInitialAnimation :: (FromAreaEffect f) => AttackAppearance
                       -> DamageType -> Position -> Position -> Script f ()
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
                   AcidDamage -> AcidProj
                   ColdDamage -> IceProj
                   EnergyDamage -> IceProj -- FIXME
                   FireDamage -> FireProj
                   MagicDamage -> IceProj -- FIXME
                   PhysicalDamage -> StarProj
                   RawDamage -> StarProj
      playSound SndBreath
      addBallisticDoodad proj origin target 220 >>= wait
    ClawAttack -> return ()
    ThrownAttack -> do
      playSound SndThrow
      addBallisticDoodad StarProj origin target 200 >>= wait
    WandAttack -> do
      let tint = case element of
                   AcidDamage -> Tint 0 255 0 192
                   ColdDamage -> Tint 0 255 255 192
                   EnergyDamage -> Tint 255 192 64 192
                   FireDamage -> Tint 255 0 0 192
                   MagicDamage -> Tint 192 64 255 192
                   PhysicalDamage -> Tint 255 255 255 192
                   RawDamage -> Tint 0 0 0 192
      addLightningDoodad tint origin target

determineIfAttackMisses :: Either CharacterNumber (Grid.Key Monster)
                        -> Position -> Bool -> Script CombatEffect Bool
determineIfAttackMisses attacker target isRanged = do
  attackAgil <- do
    case attacker of
      Left charNum -> areaGet (chrAttackAgility . arsGetCharacter charNum)
      Right monstKey -> do
        monstAttackAgility . Grid.geValue <$> demandMonsterEntry monstKey
  let playMissSound = playSound =<< getRandomElem [SndMiss1, SndMiss2]
  let missWithProb probMiss onMiss = do
        miss <- randomBool probMiss
        if not miss then return False else do True <$ onMiss
  let doTryAvoid defendAgil inv = do
        let defendAgil' = defendAgil + if inv == NoInvisibility then 0 else 20
        let input = fromIntegral (attackAgil - defendAgil') / 15 + 4
        let probMiss = 1 - 0.5 * (1 + input / (1 + abs input))
        missWithProb probMiss $ do
          playMissSound
          addFloatingWordOnTarget WordMiss (HitPosition target)
  mbOccupant <- areaGet (arsOccupant target)
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      dodge <- do
        if not isRanged then return False else do
        let probMiss = 1 - chrAbilityMultiplier Dodge 0.95 0.9 0.8 char
        missWithProb probMiss $ do
          playMissSound
          addFloatingWordOnTarget WordDodge (HitCharacter charNum)
      if dodge then return True else do
      parry <- do
        if isRanged then return False else do
        let probMiss = 1 - chrAbilityMultiplier Parry 0.97 0.94 0.9 char
        missWithProb probMiss $ do
          playMissSound -- TODO maybe different sound?  e.g. clang?
          addFloatingWordOnTarget WordParry (HitCharacter charNum)
      if parry then return True else do
      doTryAvoid (chrAttackAgility char) (seInvisibility $ chrStatus char)
    Just (Right monstEntry) -> do
      let monst = Grid.geValue monstEntry
      doTryAvoid (monstAttackAgility monst) (monstInvisibility monst)
    Nothing -> True <$ playMissSound

attackHitAnimation :: (FromAreaEffect f) => AttackAppearance -> DamageType
                   -> Position -> Bool -> Script f ()
attackHitAnimation appearance element target critical = do
  -- TODO take attack effects into account (in addition to main attack element)
  -- when choosing sound/doodad
  let elementSnd AcidDamage = SndChemicalDamage
      elementSnd ColdDamage = SndFreeze
      elementSnd FireDamage = if critical then SndBoomSmall else SndFireDamage
      elementSnd EnergyDamage = SndLightning
      elementSnd MagicDamage = SndLightning
      elementSnd PhysicalDamage = if critical then SndHit3 else SndHit4
      elementSnd RawDamage = SndHit1
  playSound $
    case appearance of
      BiteAttack -> SndBite
      BowAttack -> if critical then SndHit2 else SndHit1
      BladeAttack -> if critical then SndHit4 else SndHit3
      BluntAttack -> if critical then SndHit2 else SndHit1
      BreathAttack -> elementSnd element
      ClawAttack -> SndClaw
      ThrownAttack -> if critical then SndHit2 else SndHit1
      WandAttack -> elementSnd element
  let elementBoom = do
        let boom = case element of
                     AcidDamage -> AcidBoom
                     ColdDamage -> IceBoom
                     EnergyDamage -> EnergyBoom
                     FireDamage -> FireBoom
                     MagicDamage -> DarkBoom
                     PhysicalDamage -> SlashRight
                     RawDamage -> SlashRight
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

attackHit :: Bool -> AttackAppearance -> DamageType -> [AttackEffect]
          -> Position -> Position -> Bool -> Double -> Script CombatEffect ()
attackHit attackerIsAlly appearance element effects origin target critical
          damage = do
  attackHitAnimation appearance element target critical
  let hitTarget = HitPosition target
  extraHits <- flip3 foldM [] effects $ \hits effect -> do
    let affectStatus fn = hits <$ alterStatus hitTarget fn
    case effect of
      DrainMana mult ->
        hits <$ alterMana hitTarget (subtract $ round $ mult * damage)
      ExtraDamage dtype mult ->
        return ((hitTarget, dtype, mult * damage) : hits)
      InflictCurse mult ->
        affectStatus $ seApplyBlessing $ Harmful (mult * damage)
      InflictMental eff mult -> hits <$
        inflictMentalEffect attackerIsAlly hitTarget eff (mult * damage)
      InflictPoison mult -> hits <$ inflictPoison hitTarget (mult * damage)
      InflictSlow mult -> affectStatus $ seApplyHaste $ Harmful (mult * damage)
      InflictStun mult -> hits <$ inflictStun hitTarget (mult * damage)
      InflictWeakness mult ->
        affectStatus $ seApplyDefense $ Harmful (mult * damage)
      KnockBack -> return hits
      PurgeInvisibility -> affectStatus $ seSetInvisibility NoInvisibility
      ReduceBlessing mult -> affectStatus $ seReduceBlessing (mult * damage)
      ReduceDefense mult -> affectStatus $ seReduceDefense (mult * damage)
      ReduceHaste mult -> affectStatus $ seReduceHaste (mult * damage)
      ReduceMagicShield mult ->
        affectStatus $ seReduceMagicShield (mult * damage)
      SetField field -> hits <$ setFields field [target]
  when critical $ addFloatingWordOnTarget WordCritical hitTarget
  dealDamage ((hitTarget, element, damage) : extraHits)
  when (KnockBack `elem` effects) $ do
    _ <- tryKnockBack hitTarget $ ipointDir (target `pSub` origin)
    return ()
  wait 16

instantKill :: AttackAppearance -> DamageType -> Position -> Bool
            -> Script CombatEffect ()
instantKill appearance element target critical = do
  attackHitAnimation appearance element target critical
  mbOccupant <- areaGet (arsOccupant target)
  case mbOccupant of
    Just (Left charNum) -> do
      let hitTarget = HitCharacter charNum
      addFloatingWordOnTarget WordKO hitTarget
      killTarget hitTarget
    Just (Right entry) -> do
      let hitTarget = HitMonster $ Grid.geKey entry
      addFloatingWordOnTarget WordDeath hitTarget
      killTarget hitTarget
    Nothing -> return ()
  wait 16

-------------------------------------------------------------------------------

-- | Return the character's total agility as relevant to attacks, taking
-- skills, item bonuses, and status effects into account.
chrAttackAgility :: Character -> Int
chrAttackAgility char =
  chrGetStat Agility char +
  seAttackAgilityModifier (chrStatus char) +
  case wdRange $ chrEquippedWeaponData char of
    Melee -> 0
    Ranged _ -> if chrAbilityRank EagleEye char >= Just Rank1 then 20 else 0

-- | Get the monster's total agility as relevant to attacks, taking into
-- account status effects.
monstAttackAgility :: Monster -> Int
monstAttackAgility monst =
  mtAgility (monstType monst) + seAttackAgilityModifier (monstStatus monst)

-------------------------------------------------------------------------------
