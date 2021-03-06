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

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad (filterM, foldM, forM, forM_, replicateM_, unless, when)
import Data.Either (partitionEithers)
import Data.List (delete, find, sort)
import qualified Data.Map as Map (elems)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set

import Fallback.Constants (framesPerRound, maxActionPoints)
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import qualified Fallback.Data.Multimap as MM
import Fallback.Data.Point
import qualified Fallback.Data.Queue as Queue
import qualified Fallback.Data.TotalMap as TM (make)
import Fallback.Scenario.Script
import Fallback.State.Action
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.FOV (fieldOfView)
import Fallback.State.Item (WeaponData(..), unarmedWeaponData)
import Fallback.State.Party
import Fallback.State.Resources (ProjTag(..), SoundTag(..), StripTag(..))
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags
  (AbilityTag(..), MonsterTag(..), abilityClassAndNumber, abilityName,
   classAbility)
import Fallback.State.Terrain (positionCenter, terrainSize)
import Fallback.Utility (flip3, forMaybeM, maybeM, sortKey, unfoldM)

-------------------------------------------------------------------------------

-- TODO: Change function signature to take AbilityTag
getAbility :: CharacterClass -> AbilityNumber -> AbilityRank -> Ability
getAbility characterClass abilityNumber rank =
  case tag of
    SecondWind ->
      general' (ranked 4 4 3) (FocusCost 1) AutoTarget $ \caster power () -> do
        setCharacterAnim caster (AttackAnim 8)
        healAmount <- (power * ranked 20 35 55 *) <$> getRandomR 0.9 1.1
        playSound SndHeal
        healDamage [(HitCharacter caster, healAmount)]
        when (rank >= Rank2) $ do
          return () -- TODO reduce bad effects
        wait 16
    Valiance -> PassiveAbility
    Shieldbreaker ->
      meta (FocusCost 1) MeleeOnly SingleTarget $
      \caster power endPos -> do
        let weakness = power * ranked 0.4 0.7 0.9
        attackWithExtraEffects caster endPos [InflictWeakness weakness]
    Hardiness -> PassiveAbility
    Regenerate ->
      general (FocusCost 1) (AllyTarget 8) $ \_caster power eith -> do
        healAmount <- (ranked 40 70 120 * power *) <$> getRandomR 0.9 1.1
        playSound SndHeal
        grantRegeneration (either HitPosition HitCharacter eith) healAmount
    Parry -> PassiveAbility
    Spellshatter ->
      meta (FocusCost 1) MeleeOnly SingleTarget $
      \caster power endPos -> do
        let amount = power * ranked 0.04 0.07 0.09
        let effects =
              (if rank < Rank2 then id else (InflictCurse amount :))
              [ReduceMagicShield (amount * 2), PurgeInvisibility,
               ReduceBlessing amount, ReduceDefense amount, ReduceHaste amount]
        attackWithExtraEffects caster endPos effects
        -- TODO at rank 3, also change status of enemies adjacent to target
    Riposte -> PassiveAbility
    Critical ->
      meta (FocusCost 1) MeleeOrRanged SingleTarget $
      \caster power endPos -> do
        characterWeaponAttack caster endPos baseAttackModifiers
          { amCriticalHit = Always (power * ranked 1.5 1.75 2.0) }
    FinalBlow -> PassiveAbility
    QuickAttack ->
      meta' (ranked 3 2 1) (FocusCost 1) MeleeOrRanged SingleTarget $
      \caster power endPos -> do
        faceCharacterToward caster endPos
        setCharacterAnim caster (AttackAnim 6)
        characterWeaponAttack caster endPos baseAttackModifiers
          { amCriticalHit = Never, amDamageMultiplier = (1 - 0.25 ** power),
            amOffensive = False, amSeverity = LightDamage }
    Backstab -> PassiveAbility
    Vanish ->
      combat (FocusCost 1) (JumpTarget (const $ const $ const []) 6) $
      \caster _power (endPos, _) -> do
        -- TODO doodad?
        playSound SndIllusion
        grantInvisibility (HitCharacter caster) $
          if rank >= Rank2 then MajorInvisibility else MinorInvisibility
        emitEffect $ EffSetCharPosition caster endPos
        when (rank >= Rank3) $ do
          return () -- TODO reduce bad effects
    SmokeBomb ->
      general (FocusCost 1) (aoeTarget 5 $ ofRadius $ ranked 1 2 3) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 8 endPos
        let halflife = power * ranked 3 4 5 * fromIntegral framesPerRound
        -- TODO sound/doodad
        setFields (SmokeScreen halflife) targets
    Stability -> PassiveAbility
    RopeDart ->
      combat (FocusCost 1) (SingleTarget 5) $
      \caster power endPos -> do
        characterOffensiveActionTowards caster 8 endPos
        startPos <- areaGet (arsCharacterPosition caster)
        playSound SndThrow
        addExtendingHookshotDoodad startPos endPos >>= wait
        mbOccupant <- areaGet (arsOccupant endPos)
        case mbOccupant of
          Nothing -> playSound =<< getRandomElem [SndMiss1, SndMiss2]
          Just (Left charNum) -> do
            isBlocked <- areaGet $ \ars pos ->
              arsIsBlockedForParty ars pos ||
              isJust (arsCharacterAtPosition pos ars)
            let newPos = fromMaybe endPos $ find (not . isBlocked) $
                         bresenhamPositions startPos endPos
            addExtendedHookshotDoodad 16 startPos endPos
            playSound SndHit2
            wait 16
            characterOffensiveActionTowards caster 8 endPos
            addRetractingHookshotDoodad startPos endPos
            unless (newPos == endPos) $ do
              let time = round (2 * pDist (fromIntegral <$> endPos)
                                          (fromIntegral <$> newPos) :: Double)
              emitEffect $ EffSetCharPosition charNum newPos
              emitEffect $ EffSetCharAnim charNum $ WalkAnim time time endPos
              wait time
          Just (Right monstEntry) -> do
            let key = Grid.geKey monstEntry
            let isAlly = monstIsAlly $ Grid.geValue monstEntry
            let monstPos = rectTopleft (Grid.geRect monstEntry)
            isBlocked <- areaGet (arsIsBlockedForMonster monstEntry)
            let newPos = fromMaybe monstPos $ find (not . isBlocked) $
                         map ((monstPos `pSub` endPos) `pAdd`) $
                         bresenhamPositions startPos endPos
            -- Shoot out the rope.
            addExtendedHookshotDoodad 16 startPos endPos
            if isAlly then playSound SndHit2 >> wait 16 else do
              characterWeaponAttack caster endPos baseAttackModifiers
                { amCanBackstab = False, amCriticalHit = Never,
                  amDamageMultiplier = power, amOffensive = False,
                  amWeaponData = Just $ unarmedWeaponData
                    { wdDamageBonus = 10,
                      wdEffects = if rank < Rank2 then []
                                  else [InflictStun 1] } }
            -- Reel the target back in.
            characterOffensiveActionTowards caster 2 endPos
            addRetractingHookshotDoodad startPos endPos
            unless (newPos == monstPos) $ withMonsterEntry key $ \entry' -> do
              let time = round (2 * pDist (fromIntegral <$> monstPos)
                                          (fromIntegral <$> newPos) :: Double)
              ok <- emitAreaEffect $ EffTryMoveMonster key $ makeRect newPos $
                    rectSize $ Grid.geRect entry'
              unless ok $ fail "RopeDart: monster failed to move"
              faceMonsterAwayFrom key startPos
              setMonsterAnim key (WalkAnim time time monstPos)
              wait time
            -- If we're at rank three, slash the enemy after reeling them in
            -- (if we successfully reeled them in all the way).
            unless (isAlly || rank < Rank3) $ do
              withMonsterEntry key $ \entry' -> do
                maybeM (find ((rangeSqDist Melee >=) . pSqDist startPos) $
                        prectPositions $ Grid.geRect entry') $ \hitPos -> do
                  -- TODO: What if we have a ranged weapon equipped?
                  characterWeaponAttack caster hitPos baseAttackModifiers
    Dodge -> PassiveAbility
    Subsume ->
      meta (FocusCost 1) MeleeOnly SingleTarget $
      \caster power endPos -> do
        characterWeaponAttack caster endPos baseAttackModifiers
          { amDamageMultiplier = 1.2 * power,
            amExtraEffects = [StealHealth (ranked 0.25 0.5 1)] }
    Illusion ->
      combat (FocusCost 1) AutoTarget $ \caster power () -> do
        characterOffensiveAction caster 8
        wait 2
        startPos <- areaGet (arsCharacterPosition caster)
        monsterTag <- do
          char <- areaGet (arsGetCharacter caster)
          return $ case chrAppearance char of
                     Appearance0 -> RogueIllusion0
                     Appearance1 -> RogueIllusion1
                     Appearance2 -> RogueIllusion2
                     Appearance3 -> RogueIllusion3
        spots <- do
          isOccupied <- areaGet (flip arsOccupied)
          let okSpot pos = pos == startPos || not (isOccupied pos)
          directions <- randomPermutation allDirections
          randomPermutation =<< take (ranked 2 3 4) . filter okSpot <$>
            areaGet (arsAccessiblePositions directions startPos)
        playSound SndIllusion
        also_ (charWalkTo caster (head spots) >>= wait) $ do
          let lifetime = summonedLifetime power 21
          concurrent_ (tail spots) $ \spot -> do
            let monster = (makeMonster monsterTag)
                  { monstIsAlly = True,
                    monstSummoning = Just MonsterSummoning
                      { msMaxFrames = lifetime,
                        msRemainingFrames = lifetime,
                        msSummmoner = Left caster,
                        msUnsummonWhenSummonerGone = True },
                    monstTownAI = ChaseAI }
            mbEntry <- tryAddMonster spot monster
            maybeM (Grid.geKey <$> mbEntry) $ \key -> do
              faceMonsterAwayFrom key startPos
              setMonsterAnim key (WalkAnim 4 4 startPos)
              wait 4
    Alacrity -> PassiveAbility
    BeastCall ->
      general (3 `parts` AquaVitae) AutoTarget $
      \caster power () -> do
        setCharacterAnim caster (AttackAnim 8)
        degradeMonstersSummonedBy (Left caster)
        playSound SndSummon
        replicateM_ (ranked 1 1 2) $ do
          mtag <- getRandomElem [
            AcidCrab, Cobra, Firefly, Hound, LightningBug, Mantis,
            MonitorLizard, RabidBat, Roach, Rous, Spider, Unicorn, Wolf]
          let lifetime = summonedLifetime power 21
          mbEntry <- trySummonMonster (Left caster) mtag lifetime False
          maybeM mbEntry $ \entry -> do
          when (rank >= Rank2) $ do
            alterStatus (HitMonster $ Grid.geKey entry)
                        (seApplyBlessing $ Beneficial (power * 6))
        wait 16
    FireShot ->
      meta (2 `parts` Naphtha) RangedOnly SingleTarget $
      \caster power endPos -> do
        let effects = (if rank < Rank3 then id
                       else ((SetField $ FireWall $ power * 8) :)) $
                      [ExtraDamage FireDamage (power * ranked 0.4 0.7 0.9)]
        attackWithExtraEffects caster endPos effects
    Entangle ->
      combat (2 `parts` Limestone) (aoeTarget 5 $ ofRadius $ ranked 0 1 1) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 8 endPos
        -- TODO doodad/sound
        -- TODO at rank 3, deal damage
        forM_ targets $ \target -> do
          amount <- (8 * power *) <$> getRandomR 0.9 1.1
          setFields (Webbing amount) [target]
    FirstAid ->
      general (2 `parts` Mandrake) AutoTarget $ \caster power () -> do
        setCharacterAnim caster (AttackAnim 12)
        let baseHealAmount = ranked 20 30 40 * power
        targets <- randomPermutation =<< getAllAllyTargets
        -- TODO only affect adjacent targets
        playSound SndHeal
        forM_ targets $ \target -> do
          randMult <- getRandomR 0.9 1.1
          healDamage [(target, randMult * baseHealAmount)]
          wait 2
        wait 12
    Immunity -> PassiveAbility
    Charm ->
      combat (2 `parts` Potash) (SingleTarget 4) $ \caster power endPos -> do
        characterOffensiveActionTowards caster 8 endPos
        -- TODO doodad/sound
        duration <- (8 * power *) <$> getRandomR 0.9 1.1
        inflictMentalEffect True (HitPosition endPos)
                            (ranked Confused Charmed Charmed) duration
        -- TODO at rank 3, deal damage if the effect fails
    CurseShot ->
      meta (2 `parts` Brimstone) RangedOnly SingleTarget $
      \caster power endPos -> do
        let effects = ranked
              [InflictCurse (0.15 * power)]
              [InflictCurse (0.25 * power), InflictSlow (0.15 * power)]
              [InflictCurse (0.35 * power), InflictSlow (0.25 * power),
               InflictWeakness (0.15 * power)]
        attackWithExtraEffects caster endPos effects
    EagleEye -> PassiveAbility
    FrostShot -> PassiveAbility -- FIXME
    Summon -> PassiveAbility -- FIXME
    Fireball ->
      combat (mix AquaVitae Naphtha) (SingleTarget $ ranked 5 5 7) $
      \caster power endPos -> do
        characterOffensiveActionTowards caster 8 endPos
        startPos <- areaGet (arsCharacterPosition caster)
        damage <- (ranked 10 20 30 * power *) <$> getRandomR 0.9 1.1
        addBallisticDoodad FireProj startPos endPos 300.0 >>= wait
        playSound SndFireDamage
        addBoomDoodadAtPosition FireBoom 3 endPos >> wait 8
        dealDamage [(HitPosition endPos, FireDamage, damage)] >> wait 16
    Cure ->
      general (mix AquaVitae Mandrake) (AllyTarget 8) $
      \_caster power eith -> do
        let hitTarget = either HitPosition HitCharacter eith
        playSound SndHeal
        when (rank >= Rank2) $ alterStatus hitTarget sePurgeMentalEffects
        cureAmount <- (power * ranked 25 40 60 *) <$> getRandomR 0.9 1.1
        curePoison hitTarget cureAmount
        healAmount <- (power * ranked 20 35 55 *) <$> getRandomR 0.9 1.1
        healDamage [(hitTarget, healAmount)]
    Conflagration ->
      combat (mix Naphtha Limestone) (aoeTarget 4 $ ofRadius $ ranked 1 2 2) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 16 endPos
        startPos <- areaGet (arsCharacterPosition caster)
        addBallisticDoodad FireProj startPos endPos 500.0 >>= wait
        playSound SndBoomSmall
        let innerFn t = (max 0 (t - 0.1), Tint 255 0 0 0)
            outerFn t = (max 0 (t + 0.1), Tint 255 0 0 (floor ((1 - t) * 140)))
            trans fn t _ = fn $ if t <= 0.6 then t * 0.9 / 0.6
                                else 0.9 + 0.1 * sin ((t - 0.6) / 0.4 * pi / 2)
        addShockwaveDoodad 24 (positionCenter endPos) (ranked 58 88 88)
                           (ranked 74 110 110) (trans innerFn) (trans outerFn)
        wait 16
        when (rank >= Rank3) $ do
          return () -- FIXME initial burst of damage
        damagePerRound <- (ranked 8 12 16 * power *) <$> getRandomR 0.9 1.1
        setFields (FireWall damagePerRound) targets
        wait 8
    PoisonGas ->
      combat (mix Potash AquaVitae) (aoeTarget 4 $ ofRadius 1) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 8 endPos
        damagePerRound <- (ranked 16 28 36 * power *) <$> getRandomR 0.9 1.1
        setFields (PoisonCloud damagePerRound) targets
        when (rank >= Rank3) $ do
          return () -- TODO inflict curse on the area
    ArmorAura ->
      combat (mix DryIce Limestone) AutoTarget $ \caster power () -> do
        setCharacterAnim caster (AttackAnim 8)
        hitTargets <- getAllAllyTargets
        playSound SndShielding
        concurrent_ hitTargets $ \hitTarget -> do
          -- TODO: add doodad, maybe wait a bit before applying status
          rounds <- (ranked 8 10 14 * power *) <$> getRandomR 0.9 1.1
          alterStatus hitTarget $ seApplyDefense $ Beneficial rounds
        wait 12
    Barrier ->
      general (mix Mandrake Naphtha) (wallTarget 5 $ ranked 1 2 3) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 8 endPos
        let baseDuration = ranked 3 4 5 * power * fromIntegral framesPerRound
        playSound SndBarrier
        forM_ targets $ \target -> do
          duration <- round . (baseDuration *) <$> getRandomR 0.9 1.1
          setFields (BarrierWall duration) [target]
    Drain ->
      combat (mix Brimstone Limestone) (aoeTarget 5 $ SqDist 4) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 16 endPos
        playSound SndDrain
        -- Start a whirlwind doodad over the targeted area.
        let swooshTint = Tint 255 255 255 96
            swooshThickness = 4
        forM_ [0 .. 3] $ \idx -> do
          addSwooshDoodad swooshTint swooshThickness 32 12 $ \t ->
            positionCenter endPos `pSub` Point 0 (t * t * 120) `pAdd`
            pPolar (90 * (1 - t) * (1 - t)) (4 * pi * t + idx * pi/2)
        -- Wait for the whirlwind doodad to partially complete before dealing
        -- damage.
        wait 10
        -- If we're at rank 3, drain beneficial status effects from the area.
        totalDelta <- if rank < Rank3 then return zeroStatusDelta else do
          massAlterStatus targets $ \hitTarget -> do
            alterStatus hitTarget $
              seReduceBlessing (power * 8) . seReduceDefense (power * 8) .
              seReduceHaste (power * 8) . seReduceMagicShield (power * 8) .
              seSetInvisibility NoInvisibility
        -- Deal damage to the area.
        let baseDamage = power * ranked 10 20 30
        hits <- forM targets $ \target -> do
          damage <- (baseDamage *) <$> getRandomR 0.8 1.2
          return (HitPosition target, MagicDamage, damage)
        totalDamage <- dealDamageTotal hits
        -- Wait for the whirlwind doodad to mostly finish.
        wait 22
        -- Start a new doodad, sending the whirlwind swooshes to each ally.
        allyTargets <- getAllAllyTargets
        do let p1 = positionCenter endPos `pSub` Point 0 110
           let p2 = p1 `pSub` Point 0 90
           startPos <- areaGet (arsCharacterPosition caster)
           let p3 = positionCenter startPos `pSub` Point 0 90
           forM_ allyTargets $ \target -> do
             p4 <- positionCenter <$> getHitTargetHeadPos target
             addSwooshDoodad swooshTint swooshThickness 24 12 $
               cubicBezierCurve p1 p2 p3 p4
        -- Wait for the swooshes to reach the allies.
        wait 24
        -- Divide up the total damage done and distribute it to all allies.
        playSound SndHeal
        let healAmount = fromIntegral totalDamage /
                         fromIntegral (length allyTargets)
        healDamage $ map (flip (,) healAmount) $ allyTargets
        -- If we're at rank 3, divide up the status effects drained and
        -- distribute them to all allies.
        unless (rank < Rank3 || null allyTargets) $ do
          let delta = totalDelta `divStatusDelta` length allyTargets
          forM_ allyTargets $ \target -> do
            alterStatus target (applyStatusDelta delta)
        -- Wait for the healing animation to complete.
        wait 24
    Detonate ->
      combat (mix Brimstone Potash) (aoeTarget 4 $ ofRadius 1) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 12 endPos
        startPos <- areaGet (arsCharacterPosition caster)
        hits <- forM targets $ \target -> do
          damage <- (ranked 20 30 40 * power *) <$> getRandomR 0.9 1.1
          return (HitPosition target, FireDamage, damage)
        also_ (addBallisticDoodad FireProj startPos endPos 100.0 >>= wait) $ do
          replicateM_ 2 $ do
            wait 2
            _ <- addBallisticDoodad FireProj startPos endPos 100.0
            return ()
        shakeCamera 20 20
        playSound SndBoomBig
        forkScript $ doExplosionDoodad FireBoom $ positionCenter endPos
        wait 5
        addRemains Crater endPos
        dealDamage hits
    AdrenalineRush ->
      general (mix Mandrake Quicksilver) AutoTarget $ \caster power () -> do
        setCharacterAnim caster (AttackAnim 8)
        -- At rank 3, affects the whole party; otherwise affects only the
        -- caster and any adjacent characters.
        charNums <- do
          -- TODO: If we're keeping monster adrenline, then this should affect
          -- allies, not just characters.
          conscious <- getAllConsciousCharacters
          if rank >= Rank3 then return conscious else do
          startPos <- areaGet (arsCharacterPosition caster)
          flip filterM conscious $ \charNum -> do
            pos <- areaGet (arsCharacterPosition charNum)
            return (pos `adjacent` startPos)
        -- TODO doodad/sound
        forM_ charNums $ \charNum -> do
          amount <- round . (ranked 15 25 25 * power *) <$> getRandomR 0.9 1.1
          alterAdrenaline charNum (+ amount)
    Rainbow ->
      combat (mix DryIce Quicksilver) AutoTarget $ \caster power () -> do
        characterOffensiveAction caster 12
        do let alpha limit time =
                 floor $ min limit $ 2 * limit * (1 - 2 * abs (time - 0.5))
           let tintFn time = Tint 255 255 255 $ alpha 32 time
           let radialFn time theta =
                 let phase n = floor $ min 255 $
                       128 * (1 + sin (theta + 3*pi * time + n * (2*pi/3)))
                 in (705, Tint (phase 0) (phase 1) (phase 2) (alpha 220 time))
           center <- areaGet (positionCenter . arsCharacterPosition caster)
           addRadialGradientDoodad 45 center tintFn radialFn
        playSound SndRainbow
        wait 20
        let numEffects = ranked 1 2 3
        allies <- getAllAllyTargets
        forM_ allies $ \hitTarget -> do
          effectFn <- foldr (.) id . take numEffects <$> randomPermutation
            [seApplyBlessing (Beneficial (10 * power)),
             seApplyDefense (Beneficial (10 * power)),
             seApplyHaste (Beneficial (10 * power)),
             seApplyMagicShield (10 * power),
             seSetInvisibility MajorInvisibility]
          alterStatus hitTarget effectFn
        enemies <- getAllEnemyMonsters
        forM_ enemies $ \monstEntry -> do
          let hitTarget = HitMonster $ Grid.geKey monstEntry
          sequence_ =<< take numEffects <$> randomPermutation
            [alterStatus hitTarget (seApplyBlessing (Harmful (10 * power))),
             alterStatus hitTarget (seApplyDefense (Harmful (10 * power))),
             alterStatus hitTarget (seApplyHaste (Harmful (10 * power))),
             alterStatus hitTarget (seApplyEntanglement (15 * power)),
             inflictPoison hitTarget (power * 100)]
        wait 25
    Healing ->
      general (ManaCost 4) (AllyTarget 8) $ \_caster power eith -> do
        healAmount <- (ranked 20 35 55 * power *) <$> getRandomR 0.9 1.1
        playSound SndHeal
        healDamage [(either HitPosition HitCharacter eith, healAmount)]
    Blessing | rank < Rank3 ->
      general cost (AllyTarget 6) $ \caster power eith -> do
        -- TODO when rank=2, also affect allies adjacent to target
        doSpell caster power [either HitPosition HitCharacter eith]
             | otherwise ->
      general cost AutoTarget $ \caster power () -> do
        doSpell caster power =<< getAllAllyTargets
      where
        cost = ManaCost 1
        doSpell _caster power hitTargets = do
          playSound SndBlessing
          concurrent_ hitTargets $ \hitTarget -> do
            -- TODO: add doodad, maybe wait a bit before applying status
            rounds <- (ranked 10 12 12 * power *) <$> getRandomR 0.9 1.1
            alterStatus hitTarget $ seApplyBlessing $ Beneficial rounds
    Disruption ->
      combat (ManaCost 6) (MultiTarget (ranked 1 3 3) 4) $
      \caster power targets -> do
        characterOffensiveActionTowards caster 12 (head targets)
        startPos <- areaGet (arsCharacterPosition caster)
        let baseDamage = ranked 20 20 35 * power
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
    Restore ->
      general (ManaCost 6) (aoeTarget 8 $ ofRadius 1) $
      \_caster power (endPos, targets) -> do
        -- TODO doodad/sound
        totalDelta <- massAlterStatus targets $ \hitTarget -> do
          curePoison hitTarget (power * ranked 100 150 200)
          alterStatus hitTarget (sePurgeEntanglement . sePurgeMentalEffects)
          when (rank >= Rank2) $ alterStatus hitTarget $
            seReduceCurse (power * 8) . seReduceWeakness (power * 8) .
            seReduceSlow (power * 8)
        when (rank >= Rank3) $ do
          let prect = makeRect (endPos `pSub` Point 2 2) (5, 5)
          enemies <- areaGet (map (HitMonster . Grid.geKey) .
                              filter (not . monstIsAlly . Grid.geValue) .
                              Grid.searchRect prect . arsMonsters)
          unless (null enemies) $ do
            let delta = totalDelta `divStatusDelta` length enemies
            forM_ enemies $ \hitTarget -> do
              alterStatus hitTarget (applyStatusDelta delta)
    Hinder -> PassiveAbility -- FIXME
    Revive ->
      combat (ManaCost 1) (AllyTarget 9) $ \_caster power eith -> do
        health <- (ranked 50 80 95 * power *) <$> getRandomR 0.9 1.1
        playSound SndRevive
        let hitTarget = either HitPosition HitCharacter eith
        reviveTarget hitTarget health
    Clarity -> PassiveAbility
    GroupHeal ->
      general (ManaCost 20) AutoTarget $ \caster power () -> do
        setCharacterAnim caster (AttackAnim 12)
        let baseHealAmount = ranked 20 35 55 * power
        targets <- randomPermutation =<< getAllAllyTargets
        playSound SndHeal
        forM_ targets $ \target -> do
          randMult <- getRandomR 0.9 1.1
          healDamage [(target, randMult * baseHealAmount)]
          wait 2
        wait 12
    LucentShield | rank < Rank3 ->
      combat cost (AllyTarget 6) $ \caster power eith -> do
        doSpell caster power [either HitPosition HitCharacter eith]
                 | otherwise ->
      combat cost AutoTarget $ \caster power () -> do
        doSpell caster power =<< getAllAllyTargets
      where
        cost = ManaCost 1
        doSpell caster power hitTargets = do
          setCharacterAnim caster (AttackAnim 8)
          playSound SndShielding
          concurrent_ hitTargets $ \hitTarget -> do
            -- TODO: add doodad, maybe wait a bit before applying status
            rounds <- (ranked 6 8 8 * power *) <$> getRandomR 0.9 1.1
            alterStatus hitTarget $ seApplyMagicShield rounds
          wait 12
    Sunbeam ->
      combat (ManaCost 1) beamTarget $ \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 16 endPos
        startPos <- areaGet (arsCharacterPosition caster)
        let startPt = positionCenter startPos :: DPoint
        let endPt = startPt `pAdd`
                    (positionCenter endPos `pSub` startPt) `pMul`
                    (fromIntegral (length targets) /
                     fromIntegral (length (takeWhile (/= endPos) targets) + 1))
        playSound SndSunbeam
        addBeamDoodad (Tint 255 255 96 192) startPt endPt 20
        addBoomDoodadAtPoint LightBoom 3 (fmap round endPt)
        let baseDamage = ranked 20 28 40 * power
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
      combat (ManaCost 2) (SingleTarget 5) $
      \caster power endPos -> do
        characterOffensiveActionTowards caster 12 endPos
        startPos <- areaGet (arsCharacterPosition caster)
        damage <- (ranked 12 24 36 * power *) <$> getRandomR 0.9 1.1
        playSound SndLightning
        replicateM_ (ranked 1 2 3) $ do
          addLightningDoodad (Tint 255 192 64 192) startPos endPos
        addBoomDoodadAtPosition EnergyBoom 3 endPos >> wait 12
        dealDamage [(HitPosition endPos, EnergyDamage, damage)] >> wait 12
    IceBolts ->
      combat (ManaCost 5) (MultiTarget (ranked 2 3 4) 4) $
      \caster power targets -> do
        characterOffensiveActionTowards caster 8 (head targets)
        startPos <- areaGet (arsCharacterPosition caster)
        concurrent_ targets $ \endPos -> do
          damage <- (ranked 12 16 20 * power *) <$> getRandomR 0.9 1.1
          addBallisticDoodad IceProj startPos endPos 300.0 >>= wait
          addBoomDoodadAtPosition IceBoom 3 endPos >> wait 12
          dealDamage [(HitPosition endPos, ColdDamage, damage)] >> wait 12
    Vitriol ->
      combat (ManaCost 7) (splashTarget 4) $
      \caster power (center, targets) -> do
        characterOffensiveActionTowards caster 8 center
        startPos <- areaGet (arsCharacterPosition caster)
        addBallisticDoodad AcidProj startPos center 300.0 >>= wait
        let hit target = do
              damage <- (ranked 16 20 20 * power *) <$> getRandomR 0.9 1.1
              addBoomDoodadAtPosition AcidBoom 3 target >> wait 12
              when (rank >= Rank3) $ do
                inflictPoison (HitPosition target) damage
              dealDamage [(HitPosition target, AcidDamage, damage)]
              wait 12
        forkScript $ hit center
        concurrent_ (delete center targets) $ \target -> do
          speed <- getRandomR 150 250
          addBallisticDoodad AcidProj center target speed >>= wait
          hit target
    Invisibility ->
      combat (ManaCost 5) (AllyTarget 6) $
      \caster _power eith -> do
        setCharacterAnim caster (AttackAnim 8)
        let hitTarget = either HitPosition HitCharacter eith
        playSound SndIllusion
        grantInvisibility hitTarget $
          if rank >= Rank2 then MajorInvisibility else MinorInvisibility
        when (rank >= Rank3) $ do
          return () -- FIXME grantBlessing hitTarget (power * whatever)
        wait 12
    Hasten ->
      combat (ManaCost 10) (AllyTarget 6) $ \caster power eith -> do
        setCharacterAnim caster (AttackAnim 8)
        amount <- (ranked 8 12 12 * power *) <$> getRandomR 0.9 1.1
        playSound SndHaste
        alterStatus (either HitPosition HitCharacter eith) $
          seApplyHaste $ Beneficial amount
        when (rank >= Rank3) $ do
          alterStatus (HitCharacter caster) $ seApplyHaste $ Beneficial amount
    Lightning ->
      combat (ManaCost 5) (SingleTarget 4) $ \caster power endPos -> do
        characterOffensiveActionTowards caster 12 endPos
        let baseDamage = ranked 10 20 30 * power
        let maxForks = ranked 1 2 3
        startPos <- areaGet (arsCharacterPosition caster)
        let hasEnemy pos = do
              mbOccupant <- areaGet (arsOccupant pos)
              case mbOccupant of
                Just (Right monstEntry) ->
                  return $ not $ monstIsAlly $ Grid.geValue monstEntry
                _ -> return False
        size <- terrainSize <$> areaGet arsTerrain
        isOpaque <- areaGet arsIsOpaque
        let pickHits origin remain delay (queue, hits) = do
              if remain <= 0 then return (queue, hits) else do
              targets <-
                fmap (take 2 . sortKey (pSqDist origin)) $
                (randomPermutation =<<) $ filterM hasEnemy $
                filter (flip Set.notMember hits) $ Set.toList $
                fieldOfView size isOpaque (ofRadius 2) origin Set.empty
              queue' <- flip3 foldM queue targets $ \q target -> do
                delay' <- (delay +) <$> getRandomR 4 8
                return $ Queue.insert (origin, target, remain - 1, delay') q
              return (queue', foldr Set.insert hits targets)
        bolts <- flip unfoldM (Queue.singleton (startPos, endPos, maxForks, 0),
                               Set.singleton endPos) $ \(queue, hits) -> do
          flip (maybe $ return Nothing) (Queue.pop queue) $
               \(bolt@(_origin, target, remain, delay), queue') -> do
            (queue'', hits') <- pickHits target remain delay (queue', hits)
            return $ Just (bolt, (queue'', hits'))
        concurrent_ bolts $ \(origin, target, remain, delay) -> do
          wait delay
          damage <- (baseDamage *) <$> getRandomR 0.9 1.1
          playSound SndLightning
          replicateM_ ((remain + 2) `div` 2) $ do
            addLightningDoodad (Tint 255 192 64 192) origin target
          addBoomDoodadAtPosition EnergyBoom 3 target >> wait 6
          dealDamage [(HitPosition target, EnergyDamage, damage)] >> wait 18
    Freeze ->
      combat (ManaCost 1) (coneTarget (ranked 45 60 60) (ranked 4 4 5)) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 8 endPos
        damage <- (ranked 10 20 30 * power *) <$> getRandomR 0.9 1.1
        playSound SndFreeze
        forM_ targets $ \target -> do
          addBoomDoodadAtPosition IceBoom 3 target
        setFields (IceWall $ power * 8) targets
        dealDamage $ flip map targets $ \pos ->
          (HitPosition pos, ColdDamage, damage)
        wait 12
    Disjunction ->
      general (ManaCost {-17-} 1) (aoeTarget 3 $ ofRadius 1) $
      \caster _power (endPos, targets) -> do
        characterOffensiveActionTowards caster 12 endPos
        let shift = 0.5
            innerFn t _ = (max 0 (sin (t * (pi + shift) - shift)),
                           Tint 255 255 255 0)
            outerFn t _ = (max 0 (sin (t * (pi + shift))),
                           Tint 255 255 255 128)
        addShockwaveDoodad 24 (positionCenter endPos) 58 74 innerFn outerFn
        wait 12
        -- TODO also reduce status effects
        removeFields targets
        wait 12
    AcidRain ->
      combat (ManaCost 1) AutoTarget $ \caster power () -> do
        characterOffensiveAction caster 8
        startPos <- areaGet (arsCharacterPosition caster)
        targets <- do
          let chooseDirTargets positions = do
                (enemies, empties) <- fmap partitionEithers $
                                  forMaybeM (Set.toList positions) $ \pos -> do
                  mbOccupant <- areaGet (arsOccupant pos)
                  return $ case mbOccupant of
                             Nothing -> Just (Right pos)
                             Just (Left _) -> Nothing
                             Just (Right entry) ->
                               if monstIsAlly (Grid.geValue entry)
                               then Nothing else Just (Left pos)
                fmap (take $ ranked 1 2 2) $ (++) <$> randomPermutation enemies
                                                  <*> randomPermutation empties
          fmap concat . sequence =<< Map.elems . MM.collapse chooseDirTargets .
            MM.fromList . map (\pos -> (startPos `dirTo` pos, pos)) .
            Set.toList <$> areaGet (arsVisibleForCharacter caster)
        concurrent_ targets $ \target -> do
          addBallisticDoodad AcidProj startPos target 350.0 >>= wait
          damage <- (ranked 25 30 40 * power *) <$> getRandomR 0.9 1.1
          addBoomDoodadAtPosition AcidBoom 3 target >> wait 12
          when (rank >= Rank3) $ inflictPoison (HitPosition target) damage
          dealDamage [(HitPosition target, AcidDamage, damage)]
          wait 12
    Luminaire ->
      combat (ManaCost 1) (aoeTarget 4 $ SqDist 4) $
      \caster power (endPos, targets) -> do
        characterOffensiveActionTowards caster 20 endPos
        let p0 = positionCenter endPos
        let p1 = p0 `pAdd` Point 84 0
            p2 = p0 `pSub` Point 0 108
            p3 = p0 `pSub` Point 84 0
            p4 = p0 `pAdd` Point 0 108
        let tint = Tint 255 192 128 128
        let duration = 44
        let height = 80
        addLightWallDoodad False tint duration height p1 p2
        addLightWallDoodad False tint duration height p2 p3
        addLightWallDoodad True  tint duration height p3 p4
        addLightWallDoodad True  tint duration height p4 p1
        wait 6
        playSound SndLuminaire
        concurrent_ (zip (sort targets) [0..]) $ \(target, n) -> do
          wait n
          damage <- (ranked 30 45 70 * power *) <$> getRandomR 0.9 1.1
          addBoomDoodadAtPosition EnergyBoom 3 target
          wait 12
          dealDamage [(HitPosition target, EnergyDamage, damage)]
        wait 12
  where
    tag = classAbility characterClass abilityNumber
    ranked v1 v2 v3 =
      case rank of { Rank1 -> v1; Rank2 -> v2; Rank3 -> v3 }
    mix i1 i2 = IngredientCost $ TM.make $
                \i -> (if i == i1 then 1 else 0) + (if i == i2 then 1 else 0)
    parts n ing = IngredientCost $ TM.make $ \i -> if i == ing then n else 0
    combat = combat' maxActionPoints
    combat' apNeeded cost tkind sfn =
      ActiveAbility apNeeded cost $ CombatAbility tkind sfn
    general = general' maxActionPoints
    general' apNeeded cost tkind sfn =
      ActiveAbility apNeeded cost $ GeneralAbility tkind sfn
    meta = meta' maxActionPoints
    meta' apNeeded cost matype tkindFn sfn =
      ActiveAbility apNeeded cost $ MetaAttack matype tkindFn sfn

-------------------------------------------------------------------------------

abilityFullDescription :: AbilityTag -> AbilityRank -> String
abilityFullDescription abilTag abilRank =
  "{b}" ++ abilityName abilTag ++ "{_}  --  Rank " ++
  show (abilityRankNumber abilRank) ++ "  --  " ++ costString ++ "\n" ++
  abilityDescription abilTag where
    costString =
      case getAbility characterClass abilNum abilRank of
        PassiveAbility -> "Passive ability"
        ActiveAbility _ cost _ -> costDescription cost
    (characterClass, abilNum) = abilityClassAndNumber abilTag

abilityDescription :: AbilityTag -> String
abilityDescription SecondWind =
  "Heal yourself of some damage.\n\
  \At rank 2, also partially cures you of negative effects.\n\
  \At rank 3, requires only three action points instead of four."
abilityDescription Valiance =
  "Permanently increases the rate at which you gain adrenaline by 10%.\n\
  \At rank 2, the increase rises to 20%.\n\
  \At rank 3, the increase rises to 30%."
abilityDescription Shieldbreaker =
  "Make a melee weapon attack, weakening the enemy's defenses so that future\
  \ attacks will deal more damage.\n\
  \At rank 2, also knocks the target back.\n\
  \At rank 3, has a chance to also daze the target."
abilityDescription Hardiness =
  "Permanently increases your armor by 3%.\n\
  \At rank 2, the increase rises to 6%.\n\
  \At rank 3, the increase rises to 10%."
abilityDescription Regenerate =
  "Cause a single ally to start regenerating health, healing them continuously\
  \ over time.\n\
  \At rank 2, regenerates more health, and for a longer time.\n\
  \At rank 3, regenerates even more health, for an even longer time."
abilityDescription Parry =
  "Permanently gives you an extra 3% chance to avoid any melee attack.\n\
  \At rank 2, your chance of parrying rises to 6%.\n\
  \At rank 3, your chance of parrying rises to 10%."
abilityDescription Spellshatter =
  "Make a powerful melee weapon attack, reducing any and all beneficial status\
  \ effects currently affecting the target.\n\
  \At rank 2, also curses the target.\n\
  \At rank 3, the status changes also affect enemies adjacent to the target."
abilityDescription Riposte =
  "Permanently gives you a 10% chance to counterattack every time you are\
  \ attacked in melee.\n\
  \At rank 2, your chance of counterattacking rises to 20%.\n\
  \At rank 3, your chance of counterattacking rises to 35%."
abilityDescription Critical =
  "Attack with your weapon, automatically scoring a critical hit and dealing\
  \ 1.5x damage.\n\
  \At rank 2, the critical hit deals double damage.\n\
  \At rank 3, the critical hit deals triple damage."
abilityDescription FinalBlow =
  "Any time an enemy would barely survive your melee attack, the attack will\
  \ deal up to 20% more damage in order to leave the enemy dead.\n\
  \At rank 2, a final blow can deal up to 40% extra damage.\n\
  \At rank 3, a final blow can deal up to 70% extra damage."
abilityDescription QuickAttack =
  "Make a weapon attack, using only three action points instead of four.  This\
  \ attack will not reveal you if you were invisible.\n\
  \At rank 2, requires only two action points.\n\
  \At rank 3, requires only one action point."
abilityDescription Backstab =
  "All your melee attacks get a 15% damage bonus whenever another ally is\
  \ adjacent to your target.\n\
  \At rank 2, you also get a 25% damage bonus when you're invisible.\n\
  \At rank 3, you also get a 20% damage bonus when you're standing in smoke."
abilityDescription Vanish =
  "Slip away a short distance, and become invisible until you next attack. \
  \ Only adjacent enemies will be able to see you, but they cannot\
  \ counterattack if you move away.\n\
  \At rank 2, even adjacent enemies won't see you.\n\
  \At rank 3, also partially cures you of negative effects."
abilityDescription SmokeBomb =
  "Create a cloud of opaque smoke, blocking line-of-sight for spells and\
  \ ranged attacks.\n\
  \At rank 2, creates a larger and longer-lasting cloud of smoke.\n\
  \At rank 3, creates a huge cloud of smoke."
abilityDescription Stability =
  "Permanently increases your stun resistance by 10%.\n\
  \At rank 2, the increase rises to 20%.\n\
  \At rank 3, the increase rises to 40%."
abilityDescription RopeDart =
  "Hook onto a distant enemy, damaging them and pulling them towards you. \
  \ When used on an ally, pulls them towards you without harming them.\n\
  \At rank 2, also heavily stuns the enemy.\n\
  \At rank 3, you slash the enemy with your weapon after reeling them in."
abilityDescription Dodge =
  "Permanently gives you an extra 5% chance to avoid any ranged weapon\
  \ attack.\n\
  \At rank 2, your chance of dodging rises to 10%.\n\
  \At rank 3, your chance of dodging rises to 20%."
abilityDescription Subsume =
  "Make a powerful melee weapon attack, stealing the enemy's health and\
  \ healing yourself by one quarter the amount of damage you inflict.\n\
  \At rank 2, steals fully half the amount of damage you inflict.\n\
  \At rank 3, steals {i}all{_} of the damage you inflict."
abilityDescription Illusion =
  "Create an illusory copy of yourself, to distract enemies from attacking\
  \ the real you.\n\
  \At rank 2, creates two illusions.\n\
  \At rank 3, creates three illusions."
abilityDescription Alacrity =
  "Permanently increases the rate at which your time-bar fills by 5%.\n\
  \At rank 2, the increase rises to 10%.\n\
  \At rank 3, the increase rises to 20%."
abilityDescription BeastCall =
  "Summon a wild animal to fight at your side.\n\
  \At rank 2, the summoned animal starts out blessed.\n\
  \At rank 3, summons {i}two{_} animals."
abilityDescription FireShot =
  "Add additional fire damage to your next ranged weapon attack.\n\
  \At rank 2, adds more damage.\n\
  \At rank 3, also sets the target on fire."
abilityDescription Entangle =
  "Cover a small area with entangling webs, causing those within to walk\
  \ slower.\n\
  \At rank 2, also knocks the entangled creatures away from you.\n\
  \At rank 3, also deals a small amount of physical damage."
abilityDescription FirstAid =
  "Restores a small amount of health to yourself and to all allies adjacent to\
  \ you.\n\
  \At rank 2, heals more damage, and also reduces weakness.\n\
  \At rank 3, heals even more damage, and also reduces slowness."
abilityDescription Immunity =
  "Permanently increases your poison/acid resistance by 10%.\n\
  \At rank 2, the increase rises to 20%.\n\
  \At rank 3, the increase rises to 40%."
abilityDescription Charm =
  "Confuse a single enemy, so that it will sometimes attack its own allies.\n\
  \At rank 2, charms the target, so that it always attacks its allies.\n\
  \At rank 3, if the target resists being charmed, it takes damage."
abilityDescription CurseShot =
  "Curse the target of your next ranged weapon attack.\n\
  \At rank 2, also slows the target.\n\
  \At rank 3, also lowers the target's defense."
abilityDescription EagleEye =
  "Permanently increases your chance to hit with a ranged weapon attack.\n\
  \At rank 2, also increases your ranged attack damage by 15%.\n\
  \At rank 3, also increases the maximum range of all your ranged attacks."
abilityDescription FrostShot =
  "Your next ranged weapon attack deals extra cold damage to a small area.\n\
  \At rank 2, also covers the area in ice.\n\
  \At rank 3, also stuns everything in the area."
abilityDescription Summon =
  "Bring a single, powerful creature into existence to fight at your side.\n\
  \At rank 2, summons a more powerful kind of creature.\n\
  \At rank 3, summons a truly deadly creature."
abilityDescription Fireball =
  "Hurl a ball of fire at a single target.\n\
  \At rank 2, the fireball does more damage.\n\
  \At rank 3, the fireball does even more damage and has a longer range."
abilityDescription Cure =
  "Restore health and reduce poison for one target.\n\
  \At rank 2, heals more damage, and also alleviates mental effects.\n\
  \At rank 3, heals even more damage."
abilityDescription Conflagration =
  "Set an area on fire, continuously damaging those within.\n\
  \At rank 2, affects a larger area.\n\
  \At rank 3, also deals an initial burst of damage to the area."
abilityDescription PoisonGas =
  "Fill an area with a cloud of toxic gas, continuously poisoning those\
  \ within.\n\
  \At rank 2, the gas is even more poisonous.\n\
  \At rank 3, also curses everything within the area."
abilityDescription ArmorAura =
  "Increase the armor of all allies for a short time, reducing physical damage\
  \ taken by 25%.\n\
  \At rank 2, the effect lasts longer.\n\
  \At rank 3, the effect lasts a very long time."
abilityDescription Barrier =
  "Create a impenetrable wall of magic, temporarily blocking all movement and\
  \ attacks.\n\
  \At rank 2, the wall is wider and lasts longer.\n\
  \At rank 3, the wall is even wider and longer-lasting."
abilityDescription Drain =
  "Drain the health of all creatures in an area, and distribute the stolen\
  \ health among all allies.\n\
  \At rank 2, drains more health to give to allies.\n\
  \At rank 3, also drains helpful status effects, giving them instead to\
  \ allies."
abilityDescription Detonate =
  "Set off an explosion, dealing fire damage to an area.\n\
  \At rank 2, the explosion deals more damage.\n\
  \At rank 3, the explosion deals even more damage."
abilityDescription AdrenalineRush =
  "Instantly increase adrenaline for you and all adjacent allies.\n\
  \At rank 2, adds more adrenaline.\n\
  \At rank 3, affects all allies, nearby or not."
abilityDescription Rainbow =
  "Grant a random beneficial status effect to every ally, and inflict a random\
  \ harmful status effect on every enemy.\n\
  \At rank 2, grants/inflicts {i}two{_} status effects on every ally/enemy.\n\
  \At rank 3, grants/inflicts {i}three{_} status effects on every ally/enemy."
abilityDescription Healing = "Restores some health for one ally.\n\
  \At rank 2, heals more damage.\n\
  \At rank 3, heals even more damage."
abilityDescription Blessing =
  "Bless a single ally, making them harder for enemies to hit, and making\
  \ their own weapon attacks hit more often and do more damage.\n\
  \At rank 2, also blesses any allies adjacent to the target.\n\
  \At rank 3, blesses all allies."
abilityDescription Disruption = "Channel energy to deal major damage to a\
  \ single undead creature.  Has no effect on living creatures.\n\
  \At rank 2, hits up to three targets instead of one.\n\
  \At rank 3, the spell can also damage daemonic creatures."
abilityDescription Restore =
  "Cures poison, alleviates mental effects, and washes away entanglement\
  \ from everyone within an area.\n\
  \At rank 2, also reduces curses, weakness, and slowness.\n\
  \At rank 3, the removed effects are inflicted upon nearby enemies."
abilityDescription Hinder =
  "Slow several targets, causing them to take turns less often.\n\
  \At rank 2, also ensnares the targets, causing them to walk slower.\n\
  \At rank 3, also has a chance to daze the targets."
abilityDescription Revive =
  "Revive a party member from unconsciousness during combat, restoring a\
  \ small portion of their health.\n\
  \At rank 2, restores more of the target's health.\n\
  \At rank 3, also restores some of the target's mana/focus."
abilityDescription Clarity =
  "Permanently increases your mental resistance by 20%.\n\
  \At rank 2, the increase rises to 40%.\n\
  \At rank 3, the increase rises to 70%."
abilityDescription GroupHeal = "Restore some health for all allies.\n\
  \At rank 2, heals more damage.\n\
  \At rank 3, heals even more damage."
abilityDescription LucentShield =
  "Shield a single ally from magical damage for a short time.\n\
  \At rank 2, the effect lasts longer.\n\
  \At rank 3, all allies are shielded."
abilityDescription Sunbeam =
  "Shoot an intense beam of heat and light, searing everything in its path. \
  \ Deals additional disruption damage to undead creatures.\n\
  \At rank 2, deals more damage, and disrupts daemonic creatures as well.\n\
  \At rank 3, deals massive damage."
abilityDescription Shock = "Strike a single target with energy damage.\n\
  \At rank 2, also slows the target.\n\
  \At rank 3, also curses the target."
abilityDescription IceBolts = "Deal cold damage to multiple targets.\n\
  \At rank 2, hits up to three targets.\n\
  \At rank 3, hits up to four targets."
abilityDescription Vitriol = "Splash a small area with damaging acid.\n\
  \At rank 2, the acid deals more damage.\n\
  \At rank 3, also poisons the targets, dealing even more damage over time."
abilityDescription Invisibility =
  "Turn one ally invisible until she next attacks.  Only adjacent enemies will\
  \ be able to see her, but they cannot counterattack if she moves away.\n\
  \At rank 2, the target will be invisible even to adjacent enemies.\n\
  \At rank 3, also blesses the target."
abilityDescription Lightning =
  "Strike a target with energy damage and then fork to hit two other, nearby\
  \ targets.\n\
  \At rank 2, forks a second time, hitting more targets.\n\
  \At rank 3, forks a third time, hitting even more targets."
abilityDescription Hasten =
  "Hasten a single ally, so that they take turns more often.\n\
  \At rank 2, the effect lasts longer.\n\
  \At rank 3, hastens the caster as well as the target."
abilityDescription Freeze =
  "Deal cold damage to a small area, and cover it in ice.\n\
  \At rank 2, deals more damage.\n\
  \At rank 3, deals even more damage and also slows the targets."
abilityDescription Disjunction =
  "Purge fields and reduce status effects from an area.\n\
  \At rank 2, harmful status effects on enemies will remain.\n\
  \At rank 3, helpful status effects on allies will also remain."
abilityDescription AcidRain =
  "Spray globs of damaging acid at your enemies in all directions.  Most\
  \ effective when you are surrounded.\n\
  \At rank 2, sprays twice as many acid globs, hitting more enemies.\n\
  \At rank 3, also poisons the targets, dealing even more damage over time."
abilityDescription Luminaire = "Deal major energy damage to a wide area.\n\
  \At rank 2, deals even more damage.\n\
  \At rank 3, deals massive damage."

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
    IceBolts -> (2, 4, 10)
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

attackWithExtraEffects :: CharacterNumber -> Position -> [AttackEffect]
                       -> Script CombatEffect ()
attackWithExtraEffects caster target effects = do
  characterWeaponAttack caster target baseAttackModifiers
    { amExtraEffects = effects }

massAlterStatus :: (FromAreaEffect f) => [Position]
                -> (HitTarget -> Script f ()) -> Script f StatusDelta
massAlterStatus positions action = do
  occupants <- areaGet (arsOccupants positions)
  fmap sumStatusDeltas $ forM occupants $ \occupant -> do
    let hitTarget = either HitCharacter (HitMonster . Grid.geKey) occupant
    oldStatus <- getStatus hitTarget
    action hitTarget
    newStatus <- getStatus hitTarget
    return $ makeStatusDelta oldStatus newStatus

-- | Given a power modifier and a base lifetime in rounds for a summoned
-- monster, return the lifetime in frames.
summonedLifetime :: PowerModifier -> Double -> Int
summonedLifetime power rounds =
  round (power * rounds * fromIntegral framesPerRound)

-------------------------------------------------------------------------------
