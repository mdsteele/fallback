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

import Control.Monad (forM_, unless)

import Fallback.Constants (maxAdrenaline)
import qualified Fallback.Data.Grid as Grid (geKey)
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Action
import Fallback.State.Area (arsGetCharacter)
import Fallback.State.Creature (CreatureAnim(..))
import Fallback.State.Party (chrEquippedWeaponData)
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status (Invisibility(..))
import Fallback.State.Tags (FeatTag(..))

-------------------------------------------------------------------------------

featEffect :: FeatTag -> FeatEffect
featEffect Offering = MetaAbility ZeroCost 3
featEffect SolarFlare =
  StandardFeat (MultiTarget 3) $ \_caster _targets -> do
    return ()  -- FIXME
featEffect Energize =
  StandardFeat autoTarget $ \caster () -> do
    charNums <- getAllConsciousCharacters
    playSound SndHeal
    forM_ charNums $ \charNum -> do
      -- TODO add doodad and wait
      restoreMojoToFull charNum
      unless (charNum == caster) $ do
        alterAdrenaline charNum (const maxAdrenaline)
featEffect Zodiac =
  StandardFeat autoTarget $ \_ () -> do
    entries <- randomPermutation =<< getAllEnemyMonsters
    forM_ entries $ \entry -> do
      damage <- getRandomR 40 60 -- TODO how much damage?
      dealDamage [(HitMonster (Grid.geKey entry), EnergyDamage, damage)]
      -- TODO doodad/sound
      wait 3
featEffect Eclipse =
  StandardFeat autoTarget $ \_caster () -> do
    hitTargets <- getAllAllyTargets
    playSound SndIllusion
    forM_ hitTargets $ \hitTarget -> do
      -- TODO add doodad and wait
      grantInvisibility hitTarget MajorInvisibility
featEffect JumpSlash =
  StandardFeat (const $ JumpTarget areaFn 3) $ \caster (endPos, targets) -> do
    characterBeginOffensiveAction caster endPos
    wait =<< charLeapTo caster endPos
    char <- areaGet (arsGetCharacter caster)
    let wd = chrEquippedWeaponData char
    setCharacterAnim caster (AttackAnim 8)
    concurrent_ targets $ \target -> do
      (critical, damage) <- characterWeaponChooseCritical char =<<
                            characterWeaponBaseDamage char wd
      characterWeaponHit wd target critical (damage * 1.5)
  where areaFn _ start end = [end `plusDir` ipointDir (end `pSub` start)]
featEffect JumpStrike =
  StandardFeat (const $ JumpTarget areaFn 3) $ \caster (endPos, targets) -> do
    characterBeginOffensiveAction caster endPos
    wait =<< charLeapTo caster endPos
    char <- areaGet (arsGetCharacter caster)
    let wd = chrEquippedWeaponData char
    setCharacterAnim caster (AttackAnim 8)
    concurrent_ targets $ \target -> do
      damage <- characterWeaponBaseDamage char wd
      characterWeaponHit wd target False damage
  where areaFn _ _ center = map (center `plusDir`) allDirections
featEffect Shortshot =
  StandardFeat (SingleTarget . (subtract 1)) $ \caster target -> do
    char <- areaGet (arsGetCharacter caster)
    let wd = chrEquippedWeaponData char
    characterWeaponInitialAnimation caster target wd
    (critical, damage) <- characterWeaponChooseCritical char =<<
                          characterWeaponBaseDamage char wd
    characterWeaponHit wd target critical (damage * 1.5)
featEffect Longshot =
  StandardFeat (SingleTarget . (+ 3)) $ \caster target -> do
    char <- areaGet (arsGetCharacter caster)
    let wd = chrEquippedWeaponData char
    characterWeaponInitialAnimation caster target wd
    (critical, damage) <- characterWeaponChooseCritical char =<<
                          characterWeaponBaseDamage char wd
    characterWeaponHit wd target critical (damage * 1.25)
featEffect Glow = MetaAbility OneThirdCost 1
featEffect Amplify = MetaAbility NormalCost 1.5
featEffect Radiate = MetaAbility ZeroCost 1
featEffect Resonate = MetaAbility NormalCost 2
featEffect _ = MetaAbility NormalCost 1.1 -- FIXME

-------------------------------------------------------------------------------

featCastingCost :: FeatTag -> CastingCost
featCastingCost Offering = AdrenalineCost 10
featCastingCost SolarFlare = AdrenalineCost 30
featCastingCost Energize = AdrenalineCost 100
featCastingCost StarShield = AdrenalineCost 25
featCastingCost Zodiac = AdrenalineCost 50
featCastingCost Imprison = AdrenalineCost 100
featCastingCost TidalForce = AdrenalineCost 40
featCastingCost Eclipse = AdrenalineCost 60
featCastingCost LunarBeam = AdrenalineCost 100
featCastingCost PulseOfLife = AdrenalineCost 50
featCastingCost Avatar = AdrenalineCost 80
featCastingCost AllCreation = AdrenalineCost 100
featCastingCost _ = NoCost -- FIXME

featDescription :: FeatTag -> String
featDescription Offering = "Use any one ability for free, at triple power."
featDescription SolarFlare =
  "Deal massive fire damage to up to three enemies.  Any undead targets, no\
  \ matter how strong, are instantly destroyed."
featDescription Energize =
  "Completely refill mana/focus/adrenaline for all allies."
featDescription StarShield =
  "Put a powerful shield around all allies, protecting them from both physical\
  \ and magical attacks."
featDescription Zodiac = "Deal major energy damage to all enemies."
featDescription Imprison = "??? FIXME"
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
featDescription JumpSlash = "Leap towards an enemy, bringing your blade down\
  \ on them for 1.5x damage."
featDescription JumpStrike = "Leap amongst your enemies, slashing everything\
  \ near you when you land."
featDescription Shortshot =
  "Fire an arrow at reduced range, but with +50% damage."
featDescription Longshot = "Fire an arrow at +3 range and +25% damage."
featDescription Glow = "Use any one ability for one third of its normal cost."
featDescription Amplify = "Use any one ability, at 1.5x power."
featDescription Radiate = "Use any one ability for free."
featDescription Resonate = "Use any one ability, at double power."
featDescription _ = "??? FIXME ???"

featIconCoords :: FeatTag -> (Int, Int)
featIconCoords Offering = (6, 0)
featIconCoords SolarFlare = (6, 1)
featIconCoords Energize = (6, 2)
featIconCoords StarShield = (7, 0)
featIconCoords Zodiac = (7, 1)
featIconCoords Imprison = (7, 2)
featIconCoords TidalForce = (8, 0)
featIconCoords Eclipse = (8, 1)
featIconCoords LunarBeam = (8, 2)
featIconCoords PulseOfLife = (9, 0)
featIconCoords Avatar = (9, 1)
featIconCoords AllCreation = (9, 2)
featIconCoords Shortshot = (7, 5)
featIconCoords Longshot = (7, 4)
featIconCoords Multishot = (7, 3)
featIconCoords _ = (6, 3) -- FIXME

-------------------------------------------------------------------------------

autoTarget :: Int -> TargetKind ()
autoTarget = const AutoTarget

-------------------------------------------------------------------------------
