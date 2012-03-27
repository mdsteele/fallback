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

module Fallback.Scenario.Abilities
  (getAbility, abilityDescription, abilityFullDescription, abilityIconCoords,
   abilityMinPartyLevel)
where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (foldM, forM, forM_, replicateM_, when)
import Data.List (delete, intercalate, sort)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Data.TotalMap (makeTotalMap, tmAssocs)
import Fallback.Scenario.Script
import Fallback.State.Action
import Fallback.State.Area
import Fallback.State.Creature (mtIsDaemonic, mtIsUndead)
import Fallback.State.Item (WeaponData(..))
import Fallback.State.Party
import Fallback.State.Resources (ProjTag(..), SoundTag(..), StripTag(..))
import Fallback.State.Simple
import Fallback.State.Status (Invisibility(..), seApplyArmor)
import Fallback.State.Tags
  (AbilityTag(..), MonsterTag(..), abilityClassAndNumber, abilityName,
   classAbility)
import Fallback.State.Terrain (positionCenter)
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

-- TODO: Change function signature to take AbilityTag
getAbility :: CharacterClass -> AbilityNumber -> AbilityRank -> Ability
getAbility characterClass abilityNumber rank =
  case tag of
    Bash ->
      meta (FocusCost 1) MeleeOnly SingleTarget $
      \caster power endPos -> do
        let stun = power * ranked 0.4 0.7 0.9
        let effects = (if rank >= Rank3 then (InflictDaze 0.5 :) else id)
                      [InflictStun stun]
        attackWithExtraEffects caster endPos effects
    Valiance -> PassiveAbility
    SecondWind ->
      general (FocusCost 1) AutoTarget $ \caster power () -> do
        intBonus <- getIntellectBonus caster
        randMult <- getRandomR 0.9 1.1
        let healAmount = round (randMult * intBonus * power * ranked 20 35 55)
        playSound SndHeal
        pos <- areaGet (arsCharacterPosition caster)
        addBoomDoodadAtPosition HealBoom 4 pos
        healCharacter caster healAmount
    Hardiness -> PassiveAbility
    Shieldbreaker ->
      meta (FocusCost 1) MeleeOnly SingleTarget $
      \caster power endPos -> do
        let weakness = power * ranked 0.4 0.7 0.9
        attackWithExtraEffects caster endPos [InflictWeakness weakness]
    Parry -> PassiveAbility
    Spellshatter ->
      meta (FocusCost 1) MeleeOnly SingleTarget $
      \caster power endPos -> do
        let effect = power * ranked 0.4 0.7 0.9
        attackWithExtraEffects caster endPos [ReduceBuffs effect,
                                              InflictCurse effect]
    Riposte -> PassiveAbility
    Critical ->
      meta (FocusCost 1) MeleeOrRanged SingleTarget $
      \caster power endPos -> do
        char <- areaGet (arsGetCharacter caster)
        let wd = chrEquippedWeaponData char
        characterWeaponInitialAnimation caster endPos wd
        damage <- characterWeaponBaseDamage char wd
        let damage' = damage * power * ranked 1.5 1.75 2.0
        characterWeaponHit wd endPos True damage'
    FinalBlow -> PassiveAbility
    QuickAttack ->
      meta (FocusCost 1) MeleeOrRanged SingleTarget $
      \_ _ _ -> do
        return () -- FIXME
    Dodge -> PassiveAbility
    Immunity -> PassiveAbility
    Alacrity -> PassiveAbility
    BeastCall ->
      combat (mix AquaVitae AquaVitae) AutoTarget $
      \caster _power () -> do
        startPos <- areaGet (arsCharacterPosition caster)
        -- TODO Add other possible monster tags; at higher ranks, give better
        --      monster choices.  Incorporate power somehow.
        mtag <- getRandomElem $ [Wolf]
        playSound SndSummon
        summonAllyMonster startPos mtag
    FireShot ->
      meta (mix Naphtha Naphtha) RangedOnly SingleTarget $
      \caster power endPos -> do
        char <- areaGet (arsGetCharacter caster)
        let extra = power * ranked 0.4 0.7 0.9
        let wd = chrEquippedWeaponData char
        let wd' = wd { wdEffects = ExtraFireDamage extra : wdEffects wd }
        characterWeaponInitialAnimation caster endPos wd'
        (critical, damage) <- characterWeaponChooseCritical char =<<
                              characterWeaponBaseDamage char wd'
        when (rank >= Rank3) $ do
          setFields (FireWall $ power * 8) [endPos]
        characterWeaponHit wd' endPos critical damage
    Recuperation -> PassiveAbility
    PoisonShot ->
      meta (mix Limestone Limestone) RangedOnly SingleTarget $
      \caster power endPos -> do
        char <- areaGet (arsGetCharacter caster)
        let effects = ranked
              [InflictPoison (0.3 * power)]
              [InflictPoison (0.4 * power), ExtraAcidDamage (0.3 * power)]
              [InflictPoison (0.6 * power), ExtraAcidDamage (0.4 * power)]
        let wd = chrEquippedWeaponData char
        let wd' = wd { wdEffects = effects ++ wdEffects wd }
        characterWeaponInitialAnimation caster endPos wd'
        (critical, damage) <- characterWeaponChooseCritical char =<<
                              characterWeaponBaseDamage char wd'
        characterWeaponHit wd' endPos critical damage
    EagleEye -> PassiveAbility
    Fireball ->
      combat (mix AquaVitae Naphtha) (SingleTarget $ ofRadius 5) $
      \caster power endPos -> do
        characterBeginOffensiveAction caster endPos
        startPos <- areaGet (arsCharacterPosition caster)
        intBonus <- getIntellectBonus caster
        randMult <- getRandomR 0.9 1.1
        let damage = ranked 10 20 35 * power * intBonus * randMult
        addBallisticDoodad FireProj startPos endPos 300.0 >>= wait
        playSound SndFireDamage
        addBoomDoodadAtPosition FireBoom 3 endPos >> wait 8
        dealDamage [(HitPosition endPos, FireDamage, damage)] >> wait 16
    Conflagration ->
      combat (mix Naphtha Limestone) (aoeTarget 4 $ ofRadius $ ranked 1 2 2) $
      \caster power (endPos, targets) -> do
        characterBeginOffensiveAction caster endPos
        startPos <- areaGet (arsCharacterPosition caster)
        addBallisticDoodad FireProj startPos endPos 500.0 >>= wait
        playSound SndBoomSmall
        addShockwaveDoodad 24 (positionCenter endPos) $ \t ->
          let f = if t <= 0.6 then t * 0.9 / 0.6
                  else 0.9 + 0.1 * sin ((t - 0.6) / 0.4 * pi / 2)
          in (Tint 255 0 0 (floor ((1 - f) * 240)),
              8.0, f * ranked 58 88 88, f * ranked 74 110 110)
        wait 16
        intBonus <- getIntellectBonus caster
        randMult <- getRandomR 0.9 1.1
        when (rank >= Rank3) $ do
          return () -- FIXME initial burst of damage
        let damagePerSecond = (ranked 8 12 16) * power * intBonus * randMult
        setFields (FireWall damagePerSecond) targets
        wait 8
    ArmorAura ->
      combat (mix DryIce Limestone) AutoTarget $ \caster power () -> do
        intBonus <- getIntellectBonus caster
        hitTargets <- getAllAllyTargets
        playSound SndShielding
        concurrent_ hitTargets $ \hitTarget -> do
          randMult <- getRandomR 0.9 1.1
          -- TODO: add doodad, maybe wait a bit before applying status
          let armor = randMult * power * intBonus * ranked 0.2 0.4 0.6
          alterStatus hitTarget (seApplyArmor armor)
    Drain ->
      combat (mix Brimstone Limestone) (aoeTarget 5 4) $
      \caster _power (endPos, _targets) -> do
        characterBeginOffensiveAction caster endPos
        -- TODO write rest of Drain spell
    Detonate ->
      combat (mix Brimstone Potash) (aoeTarget 4 $ ofRadius 1) $
      \caster power (endPos, targets) -> do
        characterBeginOffensiveAction caster endPos
        startPos <- areaGet (arsCharacterPosition caster)
        intBonus <- getIntellectBonus caster
        hits <- forM targets $ \target -> do
          randMult <- getRandomR 0.9 1.1
          let damage = ranked 20 30 40 * power * intBonus * randMult
          return (HitPosition target, FireDamage, damage)
        also_ (addBallisticDoodad FireProj startPos endPos 100.0 >>= wait) $ do
          replicateM_ 2 $ do
            wait 2
            _ <- addBallisticDoodad FireProj startPos endPos 100.0
            return ()
        shakeCamera 20 20
        playSound SndBoomBig
        also_ (doExplosionDoodad FireBoom $ positionCenter endPos)
              (wait 5 >> dealDamage hits)
    Healing ->
      general (ManaCost 4) (AllyTarget $ ofRadius 8) $ \caster power eith -> do
        intBonus <- getIntellectBonus caster
        randMult <- getRandomR 0.9 1.1
        let healAmount = round (randMult * intBonus * power * ranked 20 35 55)
        playSound SndHeal
        case eith of
          Left pos -> do
            addBoomDoodadAtPosition HealBoom 4 pos
            return () -- FIXME heal at location
          Right charNum -> do
            pos <- areaGet (arsCharacterPosition charNum)
            addBoomDoodadAtPosition HealBoom 4 pos
            healCharacter charNum healAmount
    Disruption ->
      combat (ManaCost 6) (MultiTarget (ofRadius 4) (ranked 1 3 3)) $
      \caster power targets -> do
        characterBeginOffensiveAction caster (head targets)
        startPos <- areaGet (arsCharacterPosition caster)
        intBonus <- getIntellectBonus caster
        let baseDamage = intBonus * power * ranked 20 20 35
        forM_ targets $ \target -> do
          addBoomDoodadAtPosition DarkBoom 3 target
          addLightningDoodad (Tint 255 64 255 192) startPos target
        wait 12
        hits <- fmap catMaybes $ forM targets $ \target -> do
          mbOccupant <- areaGet (arsOccupant target)
          case mbOccupant of
            Just (Left charNum) -> do
              return $ Just (HitCharacter charNum, MagicDamage, 0)
            Just (Right monstEntry) -> do
              let mtype = monstType $ Grid.geValue monstEntry
              dmg <- if not (mtIsUndead mtype ||
                             rank >= Rank3 && mtIsDaemonic mtype)
                     then return 0 else (baseDamage *) <$> getRandomR 0.9 1.1
              return $ Just (HitMonster (Grid.geKey monstEntry),
                             MagicDamage, dmg)
            Nothing -> return Nothing
        dealDamage hits >> wait 12
    Clarity -> PassiveAbility
    GroupHeal ->
      general (ManaCost 20) AutoTarget $ \caster power () -> do
        intBonus <- getIntellectBonus caster
        let baseHealAmount = intBonus * power * ranked 20 35 55
        targets <- do
          -- TODO consider using getAllAllyTargets here instead
          characters <- getAllConsciousCharacters
          allies <- getAllAllyMonsters
          randomPermutation (map Left characters ++ map Right allies)
        playSound SndHeal
        forM_ targets $ \target -> do
          randMult <- getRandomR 0.9 1.1
          let healAmount = round (randMult * baseHealAmount)
          case target of
            Left charNum -> do
              pos <- areaGet (arsCharacterPosition charNum)
              addBoomDoodadAtPosition HealBoom 4 pos
              healCharacter charNum healAmount
            Right monstEntry -> do
              mapM_ (addBoomDoodadAtPosition HealBoom 4) $ prectPositions $
                Grid.geRect monstEntry
              healMonster (Grid.geKey monstEntry) healAmount
          wait 1
    Sunbeam ->
      combat (ManaCost 1) beamTarget $ \caster power (endPos, targets) -> do
        characterBeginOffensiveAction caster endPos
        startPos <- areaGet (arsCharacterPosition caster)
        let startPt = positionCenter startPos :: DPoint
        let endPt = startPt `pAdd`
                    (positionCenter endPos `pSub` startPt) `pMul`
                    (fromIntegral (length targets) /
                     fromIntegral (length (takeWhile (/= endPos) targets) + 1))
        addBeamDoodad (Tint 255 255 96 192) startPt endPt 20
        addBoomDoodadAtPoint LightBoom 3 (fmap round endPt)
        intBonus <- getIntellectBonus caster
        let baseDamage = intBonus * power * ranked 20 28 40
        spots <- fmap catMaybes $ forM targets $ \target -> do
          mbOccupant <- areaGet (arsOccupant target)
          return ((,) target <$> mbOccupant)
        forM_ (map fst spots) $ addBoomDoodadAtPosition LightBoom 2
        wait 8
        hits <- fmap concat $ forM (map snd spots) $ \occupant -> do
          randMult <- getRandomR 0.9 1.1
          let damage = randMult * baseDamage
          case occupant of
            Left charNum -> return [(HitCharacter charNum, FireDamage, damage)]
            Right monstEntry -> do
              let hitTarget = HitMonster (Grid.geKey monstEntry)
              let mtype = monstType $ Grid.geValue monstEntry
              let dealExtraDamage =
                    mtIsUndead mtype || rank >= Rank2 && mtIsDaemonic mtype
              return $ (hitTarget, FireDamage, damage) :
                       (if dealExtraDamage
                        then [(hitTarget, MagicDamage, damage * 0.7)] else [])
        dealDamage hits >> wait 8
    Shock ->
      combat (ManaCost 2) (SingleTarget $ ofRadius 5) $
      \caster power endPos -> do
        characterBeginOffensiveAction caster endPos
        startPos <- areaGet (arsCharacterPosition caster)
        intBonus <- getIntellectBonus caster
        randMult <- getRandomR 0.9 1.1
        let damage = randMult * intBonus * power * ranked 12 24 36
        replicateM_ (ranked 1 2 3) $ do
          addLightningDoodad (Tint 64 64 255 192) startPos endPos
        addBoomDoodadAtPosition EnergyBoom 3 endPos >> wait 12
        dealDamage [(HitPosition endPos, EnergyDamage, damage)] >> wait 12
    IceBolts ->
      combat (ManaCost 5) (MultiTarget (ofRadius 4) (ranked 2 3 4)) $
      \caster power targets -> do
        characterBeginOffensiveAction caster (head targets)
        startPos <- areaGet (arsCharacterPosition caster)
        intBonus <- getIntellectBonus caster
        concurrent_ targets $ \endPos -> do
          randMult <- getRandomR 0.9 1.1
          let damage = randMult * intBonus * power * ranked 12 16 20
          addBallisticDoodad IceProj startPos endPos 300.0 >>= wait
          addBoomDoodadAtPosition IceBoom 3 endPos >> wait 12
          dealDamage [(HitPosition endPos, ColdDamage, damage)] >> wait 12
    Vitriol ->
      combat (ManaCost 7) (splashTarget 4) $
      \caster power (center, targets) -> do
        characterBeginOffensiveAction caster center
        startPos <- areaGet (arsCharacterPosition caster)
        intBonus <- getIntellectBonus caster
        addBallisticDoodad AcidProj startPos center 300.0 >>= wait
        let hit target = do
              randMult <- getRandomR 0.9 1.1
              let damage = randMult * intBonus * power * ranked 16 20 20
              addBoomDoodadAtPosition AcidBoom 3 target >> wait 12
              when (rank >= Rank3) $ do
                let poison = randMult * intBonus * power * 20
                inflictPoison (HitPosition target) poison
              dealDamage [(HitPosition target, AcidDamage, damage)]
              wait 12
        also_ (hit center) $ do
          concurrent_ (delete center targets) $ \target -> do
            speed <- getRandomR 150 250
            addBallisticDoodad AcidProj center target speed >>= wait
            hit target
    Invisibility ->
      combat (ManaCost 5) (AllyTarget 6) $ \_caster _power eith -> do
        let invis = if rank >= Rank2 then MediumInvisibility
                    else MinorInvisibility
        let hitTarget = either HitPosition HitCharacter eith
        playSound SndIllusion
        grantInvisibility hitTarget invis
        when (rank >= Rank3) $ do
          return () -- FIXME grantBlessing hitTarget (power * whatever)
    Freeze ->
      combat (ManaCost 1) (aoeTarget 4 1) $
      \caster power (endPos, targets) -> do
        characterBeginOffensiveAction caster endPos
        intBonus <- getIntellectBonus caster
        randMult <- getRandomR 0.9 1.1
        let damage = power * intBonus * randMult * ranked 10 20 30
        forM_ targets $ \target -> do
          addBoomDoodadAtPosition IceBoom 3 target
        setFields (IceWall $ power * 8) targets
        dealDamage $ flip map targets $ \pos ->
          (HitPosition pos, ColdDamage, damage)
    Disjunction ->
      general (ManaCost 17) (aoeTarget 3 $ ofRadius 1) $
      \caster _power (endPos, targets) -> do
        whenCombat $ characterBeginOffensiveAction caster endPos
        addShockwaveDoodad 16 (positionCenter endPos) $ \t ->
          let f = 1 - 2 * abs (t - 0.5)
          in (Tint 255 255 255 128, 4.0, f * 58, f * 74)
        wait 8
        -- TODO also reduce status effects
        removeFields targets
    AcidRain ->
      combat (ManaCost 1) AutoTarget $ \caster power () -> do
        startPos <- areaGet (arsCharacterPosition caster)
        characterBeginOffensiveAction caster startPos
        intBonus <- getIntellectBonus caster
        targets <- flip3 foldM Set.empty allDirections $ \taken _dir -> do
          return taken -- FIXME
        concurrent_ (Set.elems targets) $ \target -> do
          addBallisticDoodad AcidProj startPos target 200.0 >>= wait
          randMult <- getRandomR 0.9 1.1
          let damage = randMult * intBonus * power * ranked 25 30 40
          addBoomDoodadAtPosition AcidBoom 3 target >> wait 12
          -- FIXME at rank 3, also do some poison damage
          dealDamage [(HitPosition target, AcidDamage, damage)]
          wait 12
    Luminaire ->
      combat (ManaCost 1) (aoeTarget 4 4) $
      \caster power (endPos, targets) -> do
        characterBeginOffensiveAction caster endPos
        intBonus <- getIntellectBonus caster
        let p0 = positionCenter endPos
        let p1 = p0 `pAdd` Point 84 0
            p2 = p0 `pSub` Point 0 108
            p3 = p0 `pSub` Point 84 0
            p4 = p0 `pAdd` Point 0 108
        let tint = Tint 128 128 255 128
        let duration = 44
        let height = 80
        addLightWallDoodad False tint duration height p1 p2
        addLightWallDoodad False tint duration height p2 p3
        addLightWallDoodad True  tint duration height p3 p4
        addLightWallDoodad True  tint duration height p4 p1
        wait 6
        concurrent_ (zip (sort targets) [0..]) $ \(target, n) -> do
          wait n
          randMult <- getRandomR 0.9 1.1
          let damage = randMult * intBonus * power * ranked 30 45 70
          addBoomDoodadAtPosition EnergyBoom 3 target
          wait 12
          dealDamage [(HitPosition target, EnergyDamage, damage)]
        wait 12
    _ -> PassiveAbility
  where
    tag = classAbility characterClass abilityNumber
    ranked v1 v2 v3 =
      case rank of { Rank1 -> v1; Rank2 -> v2; Rank3 -> v3 }
    mix i1 i2 = IngredientCost $ makeTotalMap $
                \i -> (if i == i1 then 1 else 0) + (if i == i2 then 1 else 0)
    combat cost tkind sfn = ActiveAbility cost $ CombatAbility tkind sfn
    meta cost matype tkindFn sfn =
      ActiveAbility cost $ MetaAttack matype tkindFn sfn
    general cost tkind sfn = ActiveAbility cost $ GeneralAbility tkind sfn

-------------------------------------------------------------------------------

abilityFullDescription :: AbilityTag -> AbilityRank -> String
abilityFullDescription abilTag abilRank =
  "{b}" ++ abilityName abilTag ++ "{_}  >  Rank " ++
  show (abilityRankNumber abilRank) ++ "  >  " ++ costString ++ "\n" ++
  abilityDescription abilTag where
    costString =
      case getAbility characterClass abilNum abilRank of
        PassiveAbility -> "Passive ability"
        ActiveAbility cost _ ->
          case cost of
            AdrenalineCost n -> "Cost: " ++ show n ++ " adrenaline"
            FocusCost n -> "Cost: " ++ show n ++ " focus"
            IngredientCost ing ->
              ("Cost: " ++) $ intercalate " + " $ map ingredientString $
              filter ((0 /=) . snd) $ tmAssocs ing
            ManaCost n -> "Cost: " ++ show n ++ " mana"
            NoCost -> "No cost"
    ingredientString (ingredient, n) =
      (if n == 1 then "" else show n ++ " ") ++
      case ingredient of
        AquaVitae -> "aqua vitae"
        Naphtha -> "naphtha"
        Limestone -> "limestone"
        Mandrake -> "mandrake"
        Potash -> "potash"
        Brimstone -> "brimstone"
        DryIce -> "dry ice"
        Quicksilver -> "quicksilver"
    (characterClass, abilNum) = abilityClassAndNumber abilTag

abilityDescription :: AbilityTag -> String
abilityDescription Bash =
  "Make a melee weapon attack, stunning the target.\n\
  \At rank 2, stuns the target more heavily.\n\
  \At rank 3, may also daze the target."
abilityDescription Valiance =
  "Permanently increases the rate at which you gain adrenaline by 5%.\n\
  \At rank 2, the increase rises to 10%.\n\
  \At rank 3, the increase rises to 20%."
abilityDescription SecondWind =
  "Heal yourself of some damage.\n\
  \At rank 2, requires only three action points instead of four.\n\
  \At rank 3, requires only two action points."
abilityDescription Hardiness =
  "Permanently increases your armor by 3%.\n\
  \At rank 2, the increase rises to 6%.\n\
  \At rank 3, the increase rises to 10%."
abilityDescription Shieldbreaker =
  "Make a melee weapon attack, impairing the enemy's defenses so that future\
  \ attacks will deal more damage.\n\
  \At rank 2, reduces the target's defenses more.\n\
  \At rank 3, this attack will be unmitigated by the target's armor."
abilityDescription Parry =
  "Permanently gives you a 3% chance to avoid any melee attack.\n\
  \At rank 2, your chance of parrying rises to 6%.\n\
  \At rank 3, your chance of parrying rises to 10%."
abilityDescription Spellshatter =
  "Make a melee weapon attack, reducing any and all beneficial status effects\
  \ currently affecting the target.\n\
  \At rank 2, also curses the target.\n\
  \At rank 3, FIXME."
abilityDescription Riposte =
  "Permanently gives you a 5% chance to counterattack every time you are\
  \ attacked in melee.\n\
  \At rank 2, your chance of counterattacking rises to 10%.\n\
  \At rank 3, your chance of counterattacking rises to 20%."
abilityDescription Critical =
  "Make a single weapon attack, dealing 1.5x damage.\n\
  \At rank 2, deals 1.75x damage.\n\
  \At rank 3, deals 2x damage."
abilityDescription FinalBlow =
  "Any time an enemy would barely survive your melee attack, the attack will\
  \ deal up to 15% more damage in order to leave the enemy dead.\n\
  \At rank 2, a final blow can deal up to 30% extra damage.\n\
  \At rank 3, a final blow can deal up to 50% extra damage."
abilityDescription QuickAttack =
  "Make a weapon attack, using only three action points instead of four.\n\
  \At rank 2, requires only two action points.\n\
  \At rank 3, requires only one action point."
abilityDescription Dodge =
  "Permanently gives you a 10% chance to avoid any ranged attack.\n\
  \At rank 2, your chance of dodging rises to 20%.\n\
  \At rank 3, your chance of dodging rises to 40%."
abilityDescription Vanish =
  "Become invisible until you attack or are attacked.  Only adjacent enemies\
  \ can see you, but they cannot counterattack if you move away.\n\
  \At rank 2, you stay invisible even if you are attacked.\n\
  \At rank 3, even adjacent enemies cannot see you."
abilityDescription Immunity =
  "Permanently increases your poison/acid resistance by 10%.\n\
  \At rank 2, the increase rises to 20%.\n\
  \At rank 3, the increase rises to 40%."
abilityDescription Stability =
  "Permanently increases your stun resistance by 10%.\n\
  \At rank 2, the increase rises to 20%.\n\
  \At rank 3, the increase rises to 40%."
abilityDescription Illusion =
  "Creates an illusory copy of yourself, to distract enemies from attacking\
  \ the real you.\n\
  \At rank 2, creates two illusions.\n\
  \At rank 3, creates three illusions."
abilityDescription Subsume =
  "Make a melee weapon attack, stealing the enemy's health and healing\
  \ yourself by one quarter the amount of damage you inflict.\n\
  \At rank 2, steals fully half the amount of damage you inflict.\n\
  \At rank 3, steals {i}all{_} of the damage you inflict."
abilityDescription Alacrity =
  "Permanently increases the rate at which your time-bar fills by 5%.\n\
  \At rank 2, the increase rises to 10%.\n\
  \At rank 3, the increase rises to 20%."
abilityDescription BeastCall =
  "Summons a wild animal to fight at your side.\n\
  \At rank 2, summons a stronger animal.\n\
  \At rank 3, summons {i}two{_} animals."
abilityDescription FireShot =
  "Adds additional fire damage to your next ranged weapon attack.\n\
  \At rank 2, adds more damage.\n\
  \At rank 3, also sets the target on fire."
abilityDescription Entangle =
  "Entangles a single target, causing them to walk slower for a short time.\n\
  \At rank 2, entangles a whole area.\n\
  \At rank 3, also deals a small amount of physical damage."
abilityDescription Recuperation =
  "Permanently increases the benefit you receive from restorative spells and\
  \ potions by 10%.\n\
  \At rank 2, the increase rises to 20%.\n\
  \At rank 3, the increase rises to 40%."
abilityDescription PoisonShot =
  "Poisons the target of your next ranged weapon attack.\n\
  \At rank 2, also deals additional acid damage.\n\
  \At rank 3, deals even more damage."
abilityDescription Charm =
  "Confuses a single enemy, so that it will sometimes attack its own allies.\n\
  \At rank 2, charms the target, so that it always attacks its allies.\n\
  \At rank 3, if the target resists being charmed, it takes damage."
abilityDescription EagleEye =
  "Permanently doubles your chance to hit with a ranged attack.\n\
  \At rank 2, also increases your ranged attack damage by 15%.\n\
  \At rank 3, also increases the maximum range of all your ranged attacks."
abilityDescription CurseShot =
  "Curses the target of your next ranged weapon attack.\n\
  \At rank 2, also slows the target.\n\
  \At rank 3, also lowers the target's defense."
abilityDescription Summon =
  "Brings a single, powerful creature into existence to fight at your side.\n\
  \At rank 2, summons a more powerful kind of creature.\n\
  \At rank 3, summons a truly deadly creature."
abilityDescription FrostShot =
  "Your next ranged weapon attack deals extra cold damage to a small area.\n\
  \At rank 2, also covers the area in ice.\n\
  \At rank 3, also stuns everything in the area."
abilityDescription Cure = "Restores some health for one target.\n\
  \At rank 2, heals more damage, and also reduces poison.\n\
  \At rank 3, heals even more damage."
abilityDescription Conflagration =
  "Sets an area on fire, continuously damaging those within.\n\
  \At rank 2, affects a larger area.\n\
  \At rank 3, also deals an initial burst of damage to the area."
abilityDescription PoisonGas =
  "Fills an area with a cloud of poisonous gas, continuously poisoning those\
  \ within.\n\
  \At rank 2, the gas is even more poisonous.\n\
  \At rank 3, the range of the spell is increased."
abilityDescription Drain =
  "Drains the health of all creatures in an area, and distributes the stolen\
  \ health among all allies.\n\
  \At rank 2, drains more health to give to allies.\n\
  \At rank 3, also drains helpful status effects, giving them instead to\
  \ allies."
abilityDescription Detonate =
  "Causes an explosion, dealing fire damage to an area.\n\
  \At rank 2, the explosion deals more damage.\n\
  \At rank 3, the explosion deals even more damage."
abilityDescription AdrenalineRush =
  "Instantly increases adrenaline for you and all adjacent allies.\n\
  \At rank 2, adds more adrenaline.\n\
  \At rank 3, affects all allies, nearby or not."
abilityDescription Rainbow =
  "Grants a random beneficial status effect to every ally.\n\
  \At rank 2, also inflicts a random harmful status effect on every enemy.\n\
  \At rank 3, grants/inflicts {i}two{_} status effects on every ally/enemy."
abilityDescription Healing = "Restores some health for one target.\n\
  \At rank 2, heals more damage.\n\
  \At rank 3, heals even more damage."
abilityDescription Disruption = "Deals major damage to an undead target.\n\
  \At rank 2, hits up to three targets.\n\
  \At rank 3, also damages daemonic targets."
abilityDescription Hinder =
  "Slows several targets, causing them to take turns less often.\n\
  \At rank 2, also ensnares the targets, causing them to walk slower.\n\
  \At rank 3, also has a chance to daze the targets."
abilityDescription Clarity =
  "Permanently increases your mental resistance by 20%.\n\
  \At rank 2, the increase rises to 40%.\n\
  \At rank 3, the increase rises to 70%."
abilityDescription Revive =
  "Revives a party member from unconsciousness during combat, restoring a\
  \ small portion of their health.\n\
  \At rank 2, restores more of the target's health.\n\
  \At rank 3, also restores some of the target's mana/focus."
abilityDescription GroupHeal = "Restores some health for all allies.\n\
  \At rank 2, heals more damage.\n\
  \At rank 3, heals even more damage."
abilityDescription LucentShield =
  "Shields a single ally from magical damage for a short time.\n\
  \At rank 2, the effect lasts longer.\n\
  \At rank 3, all allies are shielded."
abilityDescription Sunbeam =
  "Shoots an intense beam of heat and light, searing everything in its path. \
  \ Deals additional disruption damage to undead enemies.\n\
  \At rank 2, deals more damage, and disrupts daemonic enemies as well.\n\
  \At rank 3, deals massive damage."
abilityDescription Shock = "Strikes a single target with energy damage.\n\
  \At rank 2, also slows the target.\n\
  \At rank 3, also curses the target."
abilityDescription IceBolts = "Deals cold damage to multiple targets.\n\
  \At rank 2, hits up to three targets.\n\
  \At rank 3, hits up to four targets."
abilityDescription Vitriol = "Splashes a small area with damaging acid.\n\
  \At rank 2, deals more damage.\n\
  \At rank 3, also poisons the targets."
abilityDescription Invisibility =
  "Turn one ally invisible until she attacks or is attacked.  Only adjacent\
  \ enemies can see her, but they cannot counterattack if she moves away.\n\
  \At rank 2, the target stays invisible even if attacked.\n\
  \At rank 3, also blesses the target."
abilityDescription Lightning =
  "Strikes a target with energy damage and then forks to hit two other, nearby\
  \ targets.\n\
  \At rank 2, forks a second time, hitting more targets.\n\
  \At rank 3, forks a third time, hitting even more targets."
abilityDescription Hasten =
  "Hastens a single ally, so that they take turns more often.\n\
  \At rank 2, the effect lasts longer.\n\
  \At rank 3, hastens the caster as well as the target."
abilityDescription Freeze =
  "Deals cold damage to a small area, and covers it in ice.\n\
  \At rank 2, deals more damage.\n\
  \At rank 3, deals even more damage and also slows the targets."
abilityDescription Disjunction =
  "Purges fields and reduces status effects from an area.\n\
  \At rank 2, harmful status effects on enemies will remain.\n\
  \At rank 3, helpful status effects on allies will also remain."
abilityDescription AcidRain = "Sprays all nearby enemies with damaging acid.\n\
  \At rank 2, deals more damage.\n\
  \At rank 3, also poisons the targets."
abilityDescription Luminaire = "Deals major energy damage to a wide area.\n\
  \At rank 2, deals even more damage.\n\
  \At rank 3, deals massive damage."
abilityDescription _ = "FIXME abilityDescription"

-------------------------------------------------------------------------------

abilityIconCoords :: AbilityTag -> (Int, Int)
abilityIconCoords = (fromEnum *** fromEnum) . abilityClassAndNumber

abilityMinPartyLevel :: AbilityTag -> AbilityRank -> Int
abilityMinPartyLevel abilTag abilRank = ranked $
  case abilTag of
    -- Alchemist abilities:
    Fireball -> (1, 2, 10)
    Cure -> (2, 4, 8)
    Conflagration -> (2, 6, 10)
    PoisonGas -> (3, 9, 15)
    ArmorAura -> (4, 12, 20)
    Barrier -> (6, 12, 18)
    Drain -> (7, 13, 17)
    Detonate -> (10, 15, 20)
    AdrenalineRush -> (12, 18, 24)
    Rainbow -> (15, 20, 25)
    -- Cleric abilities:
    Healing -> (1, 5, 10)
    Blessing -> (2, 6, 9)
    Disruption -> (2, 4, 10)
    Restore -> (3, 6, 9)
    Hinder -> (3, 8, 14)
    Clarity -> (4, 11, 18)
    Revive -> (7, 17, 27)
    GroupHeal -> (8, 14, 20)
    LucentShield -> (9, 15, 20)
    Sunbeam -> (10, 20, 30)
    -- Magus abilities:
    Shock -> (1, 5, 15)
    IceBolts -> (2, 6, 10)
    Vitriol -> (2, 8, 12)
    Invisibility -> (3, 8, 10)
    Lightning -> (5, 10, 12)
    Hasten -> (6, 10, 14)
    Freeze -> (8, 12, 16)
    Disjunction -> (10, 15, 20)
    AcidRain -> (12, 18, 25)
    Luminaire -> (15, 25, 30)
    _ -> (1, 1, 1) -- TODO
  where
    ranked (a, b, c) = case abilRank of { Rank1 -> a; Rank2 -> b; Rank3 -> c }

-------------------------------------------------------------------------------
-- Private functions:

getIntellectBonus :: (FromAreaEffect f) => CharacterNumber
                  -> Script f PowerModifier
getIntellectBonus charNum = do
  char <- areaGet (arsGetCharacter charNum)
  return (1.01 ^^ chrGetStat Intellect char)

attackWithExtraEffects :: CharacterNumber -> Position -> [AttackEffect]
                       -> Script CombatEffect ()
attackWithExtraEffects caster target effects = do
  char <- areaGet (arsGetCharacter caster)
  let wd = chrEquippedWeaponData char
  let wd' = wd { wdEffects = effects ++ wdEffects wd }
  characterWeaponInitialAnimation caster target wd'
  (critical, damage) <- characterWeaponChooseCritical char =<<
                        characterWeaponBaseDamage char wd'
  characterWeaponHit wd' target critical damage

-------------------------------------------------------------------------------
