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
  (-- * Attack modifiers
   AttackModifiers(..), baseAttackModifiers, Ever(..),
   -- * Character attacks
   characterOffensiveAction, characterOffensiveActionTowards,
   characterWeaponAttack,
   -- * Monster attacks
   monsterOffensiveAction, monsterOffensiveActionToward, monsterPerformAttack,
   -- * Miscellaneous
   characterCombatWalk)
where

import Control.Applicative ((<$), (<$>))
import Control.Monad (foldM, guard, replicateM, unless, void, when)
import Data.List (delete)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid (Product(Product), mconcat)

import Fallback.Control.Script
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script.Base
import Fallback.Scenario.Script.Damage
  (DamageSeverity(..), dealDamageGeneral, killTarget)
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
import Fallback.Utility (anyM, flip3, maybeM, whenM)

-------------------------------------------------------------------------------

data AttackModifiers = AttackModifiers
  { amCanBackstab :: Bool,
    amCanFinalBlow :: Bool,
    amCanMiss :: Bool,
    amCriticalHit :: Ever Double,
    amDamageMultiplier :: Double,
    amExtraEffects :: [AttackEffect],
    amHitSound :: Bool,
    amOffensive :: Bool,
    amSeverity :: DamageSeverity,
    amWeaponData :: Maybe WeaponData }

-- | The default set of attack modifiers.  Note that this has 'amCanMiss' set
-- to 'False' (for the convenience of all the special abilities that never
-- miss), so this must be overriden for normal attacks, which /can/ miss.
baseAttackModifiers :: AttackModifiers
baseAttackModifiers = AttackModifiers
  { amCanBackstab = True,
    amCanFinalBlow = True,
    amCanMiss = False,
    amCriticalHit = Sometimes,
    amDamageMultiplier = 1,
    amExtraEffects = [],
    amHitSound = True,
    amOffensive = True,
    amSeverity = HarshDamage,
    amWeaponData = Nothing }

data Ever a = Never | Sometimes | Always a

-------------------------------------------------------------------------------

-- | Purge invisibility from the character, and set the character's attack
-- animation for the given number of frames.
characterOffensiveAction :: (FromAreaEffect f) => CharacterNumber -> Int
                         -> Script f ()
characterOffensiveAction charNum frames = do
  alterStatus (HitCharacter charNum) (seSetInvisibility NoInvisibility)
  setCharacterAnim charNum (AttackAnim frames)

-- | Same as 'characterOffensiveAction', but also face the character towards
-- the given position.
characterOffensiveActionTowards :: (FromAreaEffect f) => CharacterNumber
                                -> Int -> Position -> Script f ()
characterOffensiveActionTowards charNum frames target = do
  faceCharacterToward charNum target
  characterOffensiveAction charNum frames

-- | Make the character attack the given position with their currently equipped
-- weapon.  This does /not/ check whether the given position is within the
-- weapon's usual range.
characterWeaponAttack :: CharacterNumber -> Position -> AttackModifiers
                      -> Script CombatEffect ()
characterWeaponAttack charNum target mods = do
  -- If this attack counts as an "offensive action", the character will lose
  -- invisibility.  However, we need to know later when calculating the
  -- backstab bonus whether the character was originally invisible, so we check
  -- that here first.
  wasInvisible <- areaGet ((NoInvisibility /=) . seInvisibility . chrStatus .
                           arsGetCharacter charNum)
  when (amOffensive mods) $ do
    characterOffensiveActionTowards charNum 8 target
  -- Get the character and weapon data, and do the initial weapon animation
  -- (e.g. arrow flying) before the hit.
  char <- areaGet (arsGetCharacter charNum)
  let wd = fromMaybe (chrEquippedWeaponData char) (amWeaponData mods)
  characterWeaponInitialAnimation charNum target wd
  -- Determine if the attack misses; if it does, we stop early.
  miss <- if not (amCanMiss mods) then return False
          else determineIfAttackMisses (Left charNum) target
                                       (wdRange wd /= Melee)
  unless miss $ do
  -- Get the base damage of the weapon, which includes a few of the possible
  -- damage multipliers (such as from being blessed/cursed, from the Eagle Eye
  -- skill, or from certain items).
  baseDamage <- characterWeaponBaseDamage char wd
  -- Determine if this is a critical hit.  If it is, the damage is increased,
  -- and the hit animation will be changed slightly.
  (critical, damage) <-
    case amCriticalHit mods of
      Never -> return (False, baseDamage)
      Sometimes -> characterWeaponChooseCritical char baseDamage
      Always mult -> return (True, baseDamage * mult)
  -- Put together all the modifiers for the attack hit.  If the hit turns out
  -- to be an instant kill, only some of these will be relevant.
  let hitMods = HitModifiers
        { hmAppearance = wdAppearance wd, hmAttackerIsAlly = True,
          hmCritical = critical, hmElement = wdElement wd,
          hmFinalBlow = if not (amCanFinalBlow mods) then Nothing else
            ranked 0.2 0.4 0.7 <$> chrAbilityRank FinalBlow char,
          hmSeverity = amSeverity mods, hmHitSound = amHitSound mods }
  origin <- areaGet (arsCharacterPosition charNum)
  -- Check the weapon's damage bonuses against certain types of monsters.  A
  -- Nothing value here indicates an instant kill, in which case we use
  -- instantKill instead of attackHit.
  mbWeaponMult <- weaponDamageMultiplier wd <$> areaGet (arsOccupant target)
  case mbWeaponMult of
    Nothing -> instantKill target hitMods
    Just weaponMult -> do
      -- If backstabbing is allowed for this attack, and if it's a melee
      -- attack, and if the character has the Backstab skill, calculate the
      -- backstab multiplier.
      backstabMult <- if not (amCanBackstab mods) then return 1 else do
        if wdRange wd /= Melee then return 1 else do
        flip (maybe $ return 1) (chrAbilityRank Backstab char) $ \rank -> do
        -- Rank 1 Backstab gives us a 15% damage bonus when we're attacking an
        -- enemy that is flanked by an ally.
        m1 <- do
          mbOccupant <- areaGet (arsOccupant target)
          case mbOccupant of
            Just (Right entry) -> do
              if (monstIsAlly $ Grid.geValue entry) then return Nothing else do
              let rect = expandPrect $ Grid.geRect entry
              charNums <- delete charNum <$> getAllConsciousCharacters
              charFlank <- flip anyM charNums $ \charNum' -> do
                areaGet (rectContains rect . arsCharacterPosition charNum')
              monstFlank <- areaGet (any (monstIsAlly . Grid.geValue) .
                                     Grid.searchRect rect . arsMonsters)
              return $ if charFlank || monstFlank then Just 1.15 else Nothing
            _ -> return Nothing
        -- Rank 2 Backstab gives us a 25% damage bonus if we were invisible.
        let m2 = if rank >= Rank2 && wasInvisible then Just 1.25 else Nothing
        -- Rank 3 Backstab gives us a 20% damage bonus for standing in smoke.
        m3 <- if rank < Rank3 then return Nothing else do
          mbField <- areaGet (Map.lookup origin . arsFields)
          return $ case mbField of Just (SmokeScreen _) -> Just 1.2
                                   _ -> Nothing
        -- We want to display a "Backstab" doodad only if at least one of the
        -- three backstab opportunities applied.
        case mconcat $ map (fmap Product) [m1, m2, m3] of
          Just (Product mult) -> do
            addFloatingWordOnTarget WordBackstab (HitPosition target)
            return mult
          Nothing -> return 1
      -- Combine all the multipliers together for the final damage amount.
      attackHit (amExtraEffects mods ++ wdEffects wd) origin target
                (weaponMult * backstabMult * amDamageMultiplier mods * damage)
                hitMods

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

weaponDamageMultiplier :: WeaponData
                       -> Maybe (Either CharacterNumber (Grid.Entry Monster))
                       -> Maybe Double
weaponDamageMultiplier wd mbOccupant =
  case mbOccupant of
    Nothing -> Just 1
    Just (Left _) -> multOrKill (wdVsHuman wd)
    Just (Right entry) ->
      let mtype = monstType $ Grid.geValue entry
          dmgVs wdFn mtFn = if mtFn mtype then multOrKill (wdFn wd) else Just 1
      in fmap product $ sequence $
           [dmgVs wdVsDaemonic mtIsDaemonic, dmgVs wdVsHuman mtIsHuman,
            dmgVs wdVsUndead mtIsUndead]
  where
    multOrKill ZeroDamage = Just 0
    multOrKill HalfDamage = Just 0.5
    multOrKill NormalDamage = Just 1
    multOrKill DoubleDamage = Just 2
    multOrKill InstantKill = Nothing

-------------------------------------------------------------------------------

-- | Purge invisibility from the monster, and set the monster's attack
-- animation for the given number of frames.
monsterOffensiveAction :: Grid.Key Monster -> Int -> Script CombatEffect ()
monsterOffensiveAction key frames = do
  alterStatus (HitMonster key) (seSetInvisibility NoInvisibility)
  setMonsterAnim key (AttackAnim frames)

-- | Same as 'monsterOffensiveAction', but also face the monster towards the
-- given position.
monsterOffensiveActionToward :: Grid.Key Monster -> Int -> Position
                             -> Script CombatEffect ()
monsterOffensiveActionToward key frames target = do
  faceMonsterToward key target
  monsterOffensiveAction key frames

-- | Make the monster use the given attack on the given position.  This does
-- /not/ check whether the given position is within the attack's usual range.
monsterPerformAttack :: Grid.Key Monster -> MonsterAttack -> Position
                     -> AttackModifiers -> Script CombatEffect ()
monsterPerformAttack key attack target mods = do
  when (amOffensive mods) $ do
    monsterOffensiveActionToward key 8 target
  -- Do the initial attack animation (e.g. arrow flying) before the hit.
  monsterAttackInitialAnimation key attack target
  -- Determine if the attack misses.  Even if it does, the target may still get
  -- a chance to riposte.
  miss <- if not (amCanMiss mods) then return False else do
    determineIfAttackMisses (Right key) target (maRange attack /= Melee)
  -- The monster gets to attack, but the target may get to riposte.  These
  -- happen concurrently.
  also_
    -- Try to perform the attack:
    (unless miss $ do
       -- Get the attack's base damage, which includes multipliers from
       -- e.g. being blessed/cursed.
       baseDamage <- monsterAttackBaseDamage key attack
       -- Determine if this is a critical hit.  If it is, the damage is
       -- increased, and the hit animation will be changed slightly.
       (critical, damage) <-
         case amCriticalHit mods of
           Never -> return (False, baseDamage)
           Sometimes -> monsterAttackChooseCritical attack baseDamage
           Always mult -> return (True, baseDamage * mult)
       -- Hit the target.
       origin <- getMonsterHeadPos key
       isAlly <- monstIsAlly . Grid.geValue <$> demandMonsterEntry key
       attackHit (amExtraEffects mods ++ maEffects attack) origin target
                 (amDamageMultiplier mods * damage) HitModifiers
         { hmAppearance = maAppearance attack, hmAttackerIsAlly = isAlly,
           hmCritical = critical, hmElement = maElement attack,
           hmFinalBlow = Nothing, hmHitSound = amHitSound mods,
           hmSeverity = amSeverity mods })
    -- If the monster made a melee attack, see if the target can riposte:
    (when (maRange attack == Melee) $ do
       monstEntry <- demandMonsterEntry key
       -- We don't riposte against allies (which might attack us if charmed).
       when (not $ monstIsAlly $ Grid.geValue monstEntry) $ do
       wait 4 -- Wait to see if the character survives the attack.
       -- Get the character at the target location.  If there is none, then
       -- either it's not a character that's getting attacked, or the character
       -- was killed by the attack; either way, no riposte happens.
       mbCharNum <- areaGet (arsCharacterAtPosition target)
       maybeM mbCharNum $ \charNum -> do
       char <- areaGet (arsGetCharacter charNum)
       -- In order to be able to riposte, the character must 1) have learned
       -- the Riposte skill, 2) not be charmed, 3) not be using a ranged
       -- weapon, and 4) succeed on a random chance (dependent on the
       -- character's Riposte rank).
       maybeM (chrAbilityRank Riposte char) $ \rank -> do
       when (seMentalEffect (chrStatus char) /= Just Charmed) $ do
       let wd = chrEquippedWeaponData char
       when (wdRange wd == Melee) $ do
       whenM (randomBool $ ranked 0.1 0.2 0.35 rank) $ do
       -- Now we can riposte.  We have to pick a particular position to attack,
       -- so we aim for the monster's head, or any other spot that we can
       -- reach.  Since we can only riposte against melee attacks, we should be
       -- able to reach something.
       let counterTarget =
             let headPos = monstHeadPos monstEntry
             in if adjacent target headPos then headPos else
                  fromMaybe headPos $ listToMaybe $ filter (adjacent target) $
                  prectPositions $ Grid.geRect monstEntry
       faceCharacterToward charNum counterTarget
       setCharacterAnim charNum (AttackAnim 6)
       addFloatingWordOnTarget WordRiposte (HitPosition counterTarget)
       characterWeaponAttack charNum counterTarget baseAttackModifiers
         { amCanBackstab = False, amCanMiss = False, amCriticalHit = Never,
           amDamageMultiplier = 0.75, amOffensive = False,
           amSeverity = LightDamage })

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

-------------------------------------------------------------------------------

data HitModifiers = HitModifiers
  { hmAppearance :: AttackAppearance,
    hmAttackerIsAlly :: Bool, -- n.b. ally, not friend; matters for mental effs
    hmCritical :: Bool, -- whether the should hit be animated as a critical hit
    hmElement :: DamageType,
    hmFinalBlow :: Maybe Double, -- percent extra damage
    hmHitSound :: Bool,
    hmSeverity :: DamageSeverity }

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

attackHitAnimation :: (FromAreaEffect f) => HitModifiers -> Position
                   -> Script f ()
attackHitAnimation mods target = do
  -- TODO take attack effects into account (in addition to main attack element)
  -- when choosing sound/doodad
  let critical = hmCritical mods
  let elementSnd AcidDamage = SndChemicalDamage
      elementSnd ColdDamage = SndFreeze
      elementSnd FireDamage = if critical then SndBoomSmall else SndFireDamage
      elementSnd EnergyDamage = SndLightning
      elementSnd MagicDamage = SndLightning
      elementSnd PhysicalDamage = if critical then SndHit3 else SndHit4
      elementSnd RawDamage = SndHit1
  when (hmHitSound mods) $ playSound $
    case hmAppearance mods of
      BiteAttack -> SndBite
      BowAttack -> if critical then SndHit2 else SndHit1
      BladeAttack -> if critical then SndHit4 else SndHit3
      BluntAttack -> if critical then SndHit2 else SndHit1
      BreathAttack -> elementSnd (hmElement mods)
      ClawAttack -> SndClaw
      ThrownAttack -> if critical then SndHit2 else SndHit1
      WandAttack -> elementSnd (hmElement mods)
  let elementBoom = do
        let boom = case hmElement mods of
                     AcidDamage -> AcidBoom
                     ColdDamage -> IceBoom
                     EnergyDamage -> EnergyBoom
                     FireDamage -> FireBoom
                     MagicDamage -> DarkBoom
                     PhysicalDamage -> SlashRight
                     RawDamage -> SlashRight
        addBoomDoodadAtPosition boom (if critical then 3 else 2) target
  case hmAppearance mods of
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

attackHit :: [AttackEffect] -> Position -> Position -> Double -> HitModifiers
          -> Script CombatEffect ()
attackHit effects origin target damage mods = do
  attackHitAnimation mods target
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
        inflictMentalEffect (hmAttackerIsAlly mods) hitTarget eff
                            (mult * damage)
      InflictPoison mult -> hits <$ inflictPoison hitTarget (mult * damage)
      InflictSlow mult -> affectStatus $ seApplyHaste $ Harmful (mult * damage)
      InflictStun mult ->
        if hmSeverity mods < HarshDamage then return hits
        else hits <$ inflictStun hitTarget (mult * damage)
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
  when (hmCritical mods) $ addFloatingWordOnTarget WordCritical hitTarget
  void $ dealDamageGeneral (hmSeverity mods) (hmFinalBlow mods)
                           ((hitTarget, hmElement mods, damage) : extraHits)
  when (KnockBack `elem` effects) $ do
    _ <- tryKnockBack hitTarget $ ipointDir (target `pSub` origin)
    return ()
  wait 16

instantKill :: Position -> HitModifiers -> Script CombatEffect ()
instantKill target mods = do
  attackHitAnimation mods target
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

-- TODO: This belongs in a different module.
characterCombatWalk :: CharacterNumber -> Position -> Script CombatEffect ()
characterCombatWalk charNum dest = do
  char <- areaGet (arsGetCharacter charNum)
  origin <- areaGet (arsCharacterPosition charNum)
  hits <- do
    if seInvisibility (chrStatus char) /= NoInvisibility then return [] else do
    let maybeHit entry = do
          guard $ not (monstIsAlly $ Grid.geValue entry) &&
            not (rectIntersects (expandPosition dest) $ Grid.geRect entry)
          attack <- listToMaybe $ monstAttacks $ Grid.geValue entry
          Just (Grid.geKey entry, attack)
    randomPermutation =<<
      areaGet (mapMaybe maybeHit . Grid.searchRect (expandPosition origin) .
               arsMonsters)
  let frames = 3 -- how many frames does each attack delays the walking
  also_ (wait (frames * length hits) >> charWalkTo charNum dest >>= wait) $ do
    concurrent_ (zip hits [0, frames ..]) $ \((key, attack), delay) -> do
      wait delay
      faceMonsterToward key origin
      setMonsterAnim key (AttackAnim 6)
      monsterPerformAttack key attack origin baseAttackModifiers
        { amCanMiss = True, amCriticalHit = Never, amDamageMultiplier = 0.75,
          amOffensive = False, amSeverity = LightDamage }

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

ranked :: a -> a -> a -> AbilityRank -> a
ranked v1 v2 v3 rank = case rank of { Rank1 -> v1; Rank2 -> v2; Rank3 -> v3 }

-------------------------------------------------------------------------------
