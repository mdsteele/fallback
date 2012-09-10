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

module Fallback.Scenario.Feats
  (featEffect, featCastingCost, featDescription, featIconCoords)
where

import Control.Monad (forM_, unless, void, when)

import Fallback.Constants (framesPerRound, maxAdrenaline)
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Action
import Fallback.State.Area
import Fallback.State.Creature
  (CreatureAnim(..), monstHealth, monstIsAlly, monstIsSummoned)
import Fallback.State.Item (wdAppearance)
import Fallback.State.Party
  (chrEquippedWeaponData, chrHealth, chrMaxHealth, partyGetCharacter)
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags (FeatTag(..), MonsterTag(..))
import Fallback.State.Terrain (positionCenter, prectRect)

-------------------------------------------------------------------------------

featEffect :: FeatTag -> FeatEffect
featEffect Concentrate = MetaAbility FullAP NormalCost 1.2
featEffect Offering = MetaAbility FullAP ZeroCost 3
featEffect SolarFlare =
  StandardFeat (MultiTarget 3) $ \caster targets -> do
    playSound SndLuminaire
    concurrent_ targets $ \target -> do
      characterWeaponAttack caster target baseAttackModifiers
        { amHitSound = False }
featEffect Energize =
  StandardFeat autoTarget $ \caster () -> do
    setCharacterAnim caster (AttackAnim 8)
    charNums <- getAllConsciousCharacters
    playSound SndHeal
    forM_ charNums $ \charNum -> do
      -- TODO doodad
      restoreMojoToFull charNum
      unless (charNum == caster) $ do
        alterAdrenaline charNum (const maxAdrenaline)
    wait 12
featEffect StarShield =
  StandardFeat autoTarget $ \caster () -> do
    setCharacterAnim caster (AttackAnim 8)
    -- TODO doodad
    let rounds = 15
    playSound SndShielding
    hitTargets <- getAllAllyTargets
    forM_ hitTargets $ \hitTarget -> do
      alterStatus hitTarget $
        seApplyMagicShield rounds . seApplyDefense (Beneficial rounds)
    wait 12
featEffect Zodiac =
  StandardFeat autoTarget $ \_caster () -> do
    entries <- randomPermutation =<< getAllEnemyMonsters
    forM_ entries $ \entry -> do
      damage <- getRandomR 60 90
      -- TODO sound
      addBoomDoodadAtPoint EnergyBoom 3 $ rectCenter $ prectRect $
        Grid.geRect entry
      wait 3
      dealDamage [(HitMonster (Grid.geKey entry), EnergyDamage, damage)]
    wait 24
featEffect Banish =
  StandardFeat (const $ aoeTarget 6 (ofRadius 2)) $
  \caster (endPos, targets) -> do
    characterOffensiveActionTowards caster 8 endPos
    -- TODO sound/doodad
    -- Unsummon all summoned monsters in the area:
    forM_ targets $ \target -> do
      mbOccupant <- areaGet (arsOccupant target)
      case mbOccupant of
        Just (Right entry)
          | monstIsSummoned (Grid.geValue entry) -> do
              unsummonMonster (Grid.geKey entry)
          | otherwise -> return ()
        _ -> return ()
    -- Imprison remaining enemies:
    let imprison prect = do
          forM_ (prectPositions $ expandPrect prect) $ \pos -> do
            duration <- getRandomR (8 * framesPerRound) (12 * framesPerRound)
            setFields (BarrierWall duration) [pos]
    forM_ targets $ \target -> do
      mbOccupant <- areaGet (arsOccupant target)
      case mbOccupant of
        Just (Right entry)
          | not $ monstIsAlly (Grid.geValue entry) -> do
              imprison (Grid.geRect entry)
          | otherwise -> return ()
        _ -> return ()
    wait 12
featEffect TidalForce =
  StandardFeat (flip aoeTarget (ofRadius 2) . (1 +)) $
  \caster (endPos, targets) -> do
    startPos <- areaGet (arsCharacterPosition caster)
    addBlasterDoodad (Tint 64 192 255 192) 6 100 startPos endPos 500 >>= wait
    let innerFn t th = (0.1 * t, Tint 0 (64 + round (60 * sin (th * 5))) 128 0)
        outerFn t _ = (t, Tint 255 255 255 (255 - round (t * 255)))
    addShockwaveDoodad 24 (positionCenter endPos) 132 165 innerFn outerFn
    wait 12
    damage <- getRandomR 45 65 -- TODO how much damage?
    dealDamage $ map (\p -> (HitPosition p, ColdDamage, damage)) targets
    massInflictMentalEffect True Dazed 8 targets
    wait 12
featEffect Eclipse =
  StandardFeat autoTarget $ \caster () -> do
    setCharacterAnim caster (AttackAnim 8)
    hitTargets <- getAllAllyTargets
    playSound SndIllusion
    forM_ hitTargets $ \hitTarget -> do
      -- TODO doodad?
      grantInvisibility hitTarget MajorInvisibility
    wait 12
featEffect LunarBeam =
  StandardFeat (const $ beamTarget) $ \caster (endPos, targets) -> do
    characterOffensiveActionTowards caster 24 endPos
    startPos <- areaGet (arsCharacterPosition caster)
    let startPt = positionCenter startPos :: DPoint
    let endPt = beamEndPoint startPt endPos targets
    playSound SndFreeze
    addBeamDoodad (Tint 96 255 255 192) startPt endPt (length targets + 24)
    concurrent_ (zip targets [0..]) $ \(target, n) -> do
      wait n
      addBoomDoodadAtPosition IceBoom 3 target
      wait 4
      damage <- getRandomR 175 225
      dealDamage [(HitPosition target, ColdDamage, damage)]
      setFields (IceWall 10) [target]
    wait 20
featEffect PulseOfLife =
  StandardFeat (const $ AllyTarget 9) $ \caster eith -> do
    setCharacterAnim caster (AttackAnim 8)
    let hitTarget = either HitPosition HitCharacter eith
    health <- getRandomR 450 550 -- TODO full health
    playSound SndRevive
    reviveTarget hitTarget health
featEffect Avatar =
  StandardFeat autoTarget $ \caster () -> do
    setCharacterAnim caster (AttackAnim 8)
    let hitTarget = HitCharacter caster
    playSound SndHeal
    healDamage . (:[]) . (,) hitTarget =<< getRandomR 90 110
    playSound SndBlessing
    playSound SndShielding
    playSound SndHaste
    alterStatus hitTarget $
      (seApplyHaste $ Beneficial 12) . (seApplyMagicShield 13) .
      (seApplyDefense $ Beneficial 14) . (seApplyBlessing $ Beneficial 15) .
      sePurgeAllBadEffects
featEffect AllCreation =
  StandardFeat autoTarget $ \caster () -> do
    degradeMonstersSummonedBy (Left caster)
    playSound SndSummon
    startPos <- areaGet (arsCharacterPosition caster)
    concurrent_ allDirections $ \dir -> do
      let pos = startPos `pAdd` dirDelta dir `pMul` 2 -- FIXME what if behind wall?
      monstTag <- getRandomElem [
        AcidCrab, Cobra, Firefly, Hound, LightningBug, Mantis, MonitorLizard,
        RabidBat, Roach, Rous, Spider, Unicorn, Wolf]
      let lifetime = framesPerRound * 21
      void $ trySummonMonsterNear pos (Left caster) monstTag lifetime False
featEffect JumpSlash =
  StandardFeat (const $ JumpTarget areaFn 3) $ \caster (endPos, targets) -> do
    characterOffensiveActionTowards caster 8 endPos
    charLeapTo caster endPos >>= wait
    setCharacterAnim caster (AttackAnim 8)
    concurrent_ targets $ \target -> do
      characterWeaponAttack caster target baseAttackModifiers
        { amCanBackstab = False, amDamageMultiplier = 2, amOffensive = False }
  where areaFn ars charNum end =
          [end `plusDir`
           ipointDir (end `pSub` arsCharacterPosition charNum ars)]
featEffect JumpStrike =
  StandardFeat (const $ JumpTarget areaFn 3) $ \caster (endPos, targets) -> do
    characterOffensiveActionTowards caster 8 endPos
    charLeapTo caster endPos >>= wait
    setCharacterAnim caster (AttackAnim 8)
    playSound SndHit4
    concurrent_ targets $ \target -> do
      characterWeaponAttack caster target baseAttackModifiers
        { amCanBackstab = False, amCanFinalBlow = False, amCriticalHit = Never,
          amHitSound = False, amOffensive = False }
  where areaFn _ _ center = map (center `plusDir`) allDirections
featEffect Rampage =
  StandardFeat (const $ JumpTarget areaFn 4) $ \caster (endPos, targets) -> do
    characterOffensiveActionTowards caster 8 endPos
    also_ (charWalkTo caster endPos >>= wait) $ do
    playSound SndHit4
    concurrent_ targets $ \target -> do
      characterWeaponAttack caster target baseAttackModifiers
        { amCanBackstab = False, amCanFinalBlow = False, amCriticalHit = Never,
          amDamageMultiplier = 1.1, amHitSound = False, amOffensive = False }
  where areaFn ars charNum endPos = drop 1 $ init $
          bresenhamPositions (arsCharacterPosition charNum ars) endPos
featEffect Longshot =
  StandardFeat (SingleTarget . (+ 3)) $ \caster target -> do
    characterWeaponAttack caster target baseAttackModifiers
      { amDamageMultiplier = 1.5 }
featEffect Pierce =
  StandardFeat (const $ beamTarget) $ \caster (endPos, targets) -> do
    characterOffensiveActionTowards caster 16 endPos
    startPos <- areaGet (arsCharacterPosition caster)
    let startPt = positionCenter startPos :: DPoint
    let endPt = beamEndPoint startPt endPos targets
    char <- areaGet (arsGetCharacter caster)
    let wd = chrEquippedWeaponData char
    let (projTag, launchSoundTag) =
          case wdAppearance wd of
            BowAttack -> (ArrowProj, SndArrow)
            ThrownAttack -> (StarProj, SndThrow)
            _ -> (SpearProj, SndThrow)
    playSound launchSoundTag
    _ <- addBlasterDoodad' (Tint 255 255 255 192) 3 70 startPt endPt 750
    frames <- addBulletDoodad projTag startPt endPt 750
    also_ (wait frames) $ do
    concurrent_ (zip targets [1..]) $ \(target, n) -> do
      occupied <- areaGet (arsOccupied target)
      when occupied $ do
      wait $ ceiling (fromIntegral frames * n /
                      fromIntegral (length targets) :: Double)
      baseDamage <- characterWeaponBaseDamage char wd
      playSound SndHit1
      addArrowHitDoodad 5 2 17 target
      dealDamage [(HitPosition target, PhysicalDamage, 1.2 * baseDamage)]
      wait 8
featEffect Shortshot =
  StandardFeat (SingleTarget . (subtract 1)) $ \caster target -> do
    characterWeaponAttack caster target baseAttackModifiers
      { amDamageMultiplier = 2 }
featEffect Glow = MetaAbility FullAP OneThirdCost 1
featEffect Amplify = MetaAbility FullAP NormalCost 1.5
featEffect Radiate = MetaAbility FullAP ZeroCost 1
featEffect Resonate = MetaAbility FullAP NormalCost 2
featEffect TimeStop = MetaAbility ZeroAP NormalCost 1
featEffect Catalyze = MetaAbility FullAP DoubleCost 3
featEffect Assassinate =
  StandardFeat SingleTarget $ \caster target -> do
    health <- do
      mbOccupant <- areaGet (arsOccupant target)
      case mbOccupant of
        Nothing -> return 0
        Just (Left charNum) -> areaGet (chrHealth . arsGetCharacter charNum)
        Just (Right entry) -> return $ monstHealth $ Grid.geValue entry
    characterWeaponAttack caster target $
      if health <= 250 then baseAttackModifiers
        { amCriticalHit = Always 1, amInstantKill = True }
      else baseAttackModifiers
        { amCanBackstab = False, amCanFinalBlow = False, amCriticalHit = Never,
          amDamageMultiplier = 0.2 }
featEffect Crisis =
  StandardFeat SingleTarget $ \caster target -> do
    party <- areaGet arsParty
    let char = partyGetCharacter party caster
    let frac = fromIntegral (chrHealth char) /
               fromIntegral (chrMaxHealth party char)
    let mult = 4 * frac * frac - 8 * frac + 5
    characterWeaponAttack caster target baseAttackModifiers
      { amCriticalHit = Never, amDamageMultiplier = mult }
featEffect _ = MetaAbility FullAP NormalCost 1.1 -- FIXME

-------------------------------------------------------------------------------

featCastingCost :: FeatTag -> CastingCost
featCastingCost Concentrate = AdrenalineCost 100
featCastingCost Offering = AdrenalineCost 10
featCastingCost SolarFlare = AdrenalineCost 30
featCastingCost Energize = AdrenalineCost 100
featCastingCost StarShield = AdrenalineCost 25
featCastingCost Zodiac = AdrenalineCost 50
featCastingCost Banish = AdrenalineCost 100
featCastingCost TidalForce = AdrenalineCost 40
featCastingCost Eclipse = AdrenalineCost 60
featCastingCost LunarBeam = AdrenalineCost 100
featCastingCost PulseOfLife = AdrenalineCost 50
featCastingCost Avatar = AdrenalineCost 80
--featCastingCost AllCreation = AdrenalineCost 100
featCastingCost TimeStop = AdrenalineCost 50
featCastingCost _ = NoCost -- FIXME

featDescription :: FeatTag -> String
featDescription Concentrate = "Use any one ability, at +20% power."
featDescription Offering = "Use any one ability for free, at triple power."
featDescription SolarFlare =
  "Deal massive fire damage to up to three enemies.  Any undead targets, no\
  \ matter how strong, are instantly destroyed."
featDescription Energize =
  "Completely refill mana, focus, and adrenaline for all allies."
featDescription StarShield =
  "Put a powerful shield around all allies, protecting them from both physical\
  \ and magical attacks."
featDescription Zodiac = "Deal major energy damage to all enemies."
featDescription Banish =
  "Banish all summoned monsters in a wide area, and imprison any remaining\
  \ enemies in barriers."
featDescription TidalForce =
  "Inflict ice damage and daze everything in a wide area."
featDescription Eclipse =
  "Instantly grant major invisibility to all allies.  Enemies will not be able\
  \ to see you until you attack."
featDescription LunarBeam =
  "Shoot a devastating beam of cold, damaging everything in a line and\
  \ covering it with ice."
featDescription PulseOfLife =
  "Restore a single ally to full health, even if they were unconscious."
featDescription Avatar =
  "Become a mighty warrior; heals, blesses, shields, and hastens yourself, and\
  \ cures you of all negative effects."
featDescription AllCreation =
  "Summon a multitude of wild animals and beasts to attack your enemies."
featDescription Envenom = "Slash one enemy, with a deadly poison."
featDescription JumpSlash = "Leap towards an enemy, bringing your blade down\
  \ on them for double damage."
featDescription JumpStrike = "Leap amongst your enemies, slashing everything\
  \ near you when you land."
featDescription Rampage =
  "Make a mad dash past your enemies, running them through as you go by."
featDescription Spincut =
  "Spin your blade deftly around you, slashing all adjacent enemies and\
  \ leaving allies untouched."
featDescription SweepSlash =
  "Sweep your blade in an arc, hitting multiple enemies, at +50% damage."
featDescription Whirlwind =
  "Spin your blade wildly around you, slashing everything next to you and\
  \ knocking them back."
featDescription NeutronBomb = "Damage and curse all enemies near the target."
featDescription Longshot = "Fire an arrow at +3 range and +50% damage."
featDescription Pierce = "Let fly your weapon with such force that it pierces\
  \ through all enemies in a straight line."
featDescription Shortshot =
  "Fire an arrow, at reduced range, for double damage."
featDescription TripleTap = "Fire three arrows at the same target."
featDescription Glow = "Use any one ability for one third of its normal cost."
featDescription Amplify = "Use any one ability, at +50% power."
featDescription Radiate = "Use any one ability for free."
featDescription Resonate = "Use any one ability, at double power."
featDescription TimeStop =
  "Use any one ability, without using up any action points."
featDescription Catalyze =
  "Use any one ability, at triple power, for double its normal cost."
featDescription Assassinate =
  "Attempt to strike a single, fatal blow on an enemy.  If the target has 250\
  \ remaining health or less, it is instantly killed; otherwise, it takes only\
  \ a small amount of damage."
featDescription Crisis =
  "Make a single, desparate blow against an enemy.  The less health you have\
  \ remaining, the more damage the attack will do."
featDescription _ = "??? FIXME ???"

featIconCoords :: FeatTag -> (Int, Int)
featIconCoords Concentrate = (9, 9)
featIconCoords Offering = (6, 0)
featIconCoords SolarFlare = (6, 1)
featIconCoords Energize = (6, 2)
featIconCoords StarShield = (7, 0)
featIconCoords Zodiac = (7, 1)
featIconCoords Banish = (7, 2)
featIconCoords TidalForce = (8, 0)
featIconCoords Eclipse = (8, 1)
featIconCoords LunarBeam = (8, 2)
featIconCoords PulseOfLife = (9, 0)
featIconCoords Avatar = (9, 1)
featIconCoords AllCreation = (9, 2)
featIconCoords JumpSlash = (6, 5)
featIconCoords JumpStrike = (6, 4)
featIconCoords Rampage = (6, 7)
featIconCoords Longshot = (7, 4)
featIconCoords Pierce = (7, 6)
featIconCoords Shortshot = (7, 5)
featIconCoords TripleTap = (7, 3)
featIconCoords Glow = (9, 3)
featIconCoords Amplify = (9, 4)
featIconCoords Radiate = (9, 5)
featIconCoords Resonate = (9, 6)
featIconCoords TimeStop = (9, 7)
featIconCoords Catalyze = (9, 8)
featIconCoords Assassinate = (8, 3)
featIconCoords Crisis = (8, 4)
featIconCoords _ = (9, 9) -- FIXME

-------------------------------------------------------------------------------

autoTarget :: Int -> TargetKind ()
autoTarget = const AutoTarget

beamEndPoint :: DPoint -> Position -> [Position] -> DPoint
beamEndPoint startPt endPos targets =
  startPt `pAdd`
  (positionCenter endPos `pSub` startPt) `pMul`
  (fromIntegral (length targets) /
   fromIntegral (length (takeWhile (/= endPos) targets) + 1))

-------------------------------------------------------------------------------
