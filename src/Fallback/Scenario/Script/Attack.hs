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
  (characterBeginOffensiveAction, characterWeaponAttack,
   characterWeaponInitialAnimation, characterWeaponBaseDamage,
   characterWeaponChooseCritical, characterWeaponHit,
   monsterBeginOffensiveAction, monsterPerformAttack)
where

import Control.Applicative ((<$), (<$>))
import Control.Monad (foldM, replicateM, unless, when)

import Fallback.Control.Script
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script.Base
import Fallback.Scenario.Script.Doodad
import Fallback.Scenario.Script.Other
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Item (WeaponData(..))
import Fallback.State.Party
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags (AbilityTag(..))
import Fallback.State.Terrain (positionCenter, prectRect)
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

characterBeginOffensiveAction :: CharacterNumber -> Position
                              -> Script CombatEffect ()
characterBeginOffensiveAction charNum target = do
  faceCharacterToward charNum target
  alterStatus (HitCharacter charNum) (seSetInvisibility Nothing)
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
  alterStatus (HitMonster key) (seSetInvisibility Nothing)
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
                           (monstHeadPos entry) target

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

-------------------------------------------------------------------------------

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

determineIfAttackMisses :: Either CharacterNumber (Grid.Key Monster)
                        -> Position -> Bool -> Script CombatEffect Bool
determineIfAttackMisses attacker target isRanged = do
  attackAgil <- do
    case attacker of
      -- TODO take EagleEye rank 1 into account
      Left charNum -> chrGetStat Agility <$> areaGet (arsGetCharacter charNum)
      Right monstKey -> mtAgility . monstType . Grid.geValue <$>
                        demandMonsterEntry monstKey
  let playMissSound = playSound =<< getRandomElem [SndMiss1, SndMiss2]
  let missWithProb probMiss onMiss = do
        miss <- randomBool probMiss
        if not miss then return False else do True <$ onMiss
  let doTryAvoid defendAgil pt = do
        -- TODO take bless/curse into account
        -- TODO take invisibility into account
        let input = fromIntegral (attackAgil - defendAgil) / 15 + 4
        let probMiss = 1 - 0.5 * (1 + input / (1 + abs input))
        missWithProb probMiss $ do
          playMissSound
          addWordDoodadAtPoint WordMiss pt
  mbOccupant <- areaGet (arsOccupant target)
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      pos <- areaGet (arsCharacterPosition charNum)
      dodge <- do
        if not isRanged then return False else do
        let probMiss = 1 - chrAbilityMultiplier Dodge 0.95 0.9 0.8 char
        missWithProb probMiss $ do
          playMissSound
          addWordDoodadAtPosition WordDodge pos
      if dodge then return True else do
      parry <- do
        if isRanged then return False else do
        let probMiss = 1 - chrAbilityMultiplier Parry 0.97 0.94 0.9 char
        missWithProb probMiss $ do
          playMissSound -- TODO maybe different sound?  e.g. clang?
          addWordDoodadAtPosition WordParry pos
      if parry then return True else do
      doTryAvoid (chrGetStat Agility char) (positionCenter pos)
    Just (Right monstEntry) -> do
      doTryAvoid (mtAgility $ monstType $ Grid.geValue monstEntry)
                 (rectCenter $ prectRect $ Grid.geRect monstEntry)
    Nothing -> True <$ playMissSound

attackHit :: AttackAppearance -> AttackElement -> [AttackEffect] -> Position
          -> Bool -> Double -> Script CombatEffect ()
attackHit appearance element effects target critical damage = do
  -- TODO take attack effects into account (in addition to main attack element)
  -- when choosing sound/doodad
  let elementSnd AcidAttack = SndChemicalDamage
      elementSnd FireAttack = if critical then SndBoomSmall else SndFireDamage
      elementSnd PhysicalAttack = if critical then SndHit3 else SndHit4
      elementSnd _ = error "FIXME attackHit"
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
  when critical $ addWordDoodadOnTarget WordCritical hitTarget
  dealDamage ((hitTarget, damageElement, damage) : extraHits)
  wait 16

-------------------------------------------------------------------------------
