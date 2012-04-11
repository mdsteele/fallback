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
import Fallback.Scenario.Script
import Fallback.State.Action
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status (Invisibility(..))
import Fallback.State.Tags (FeatTag(..))

-------------------------------------------------------------------------------

featEffect :: FeatTag -> FeatEffect
featEffect Offering = MetaAbility ZeroCost 3
featEffect SolarFlare =
  StandardFeat (MultiTarget 6 3) $ \_caster _targets -> do
    return ()  -- FIXME
featEffect Energize =
  StandardFeat AutoTarget $ \caster () -> do
    charNums <- getAllConsciousCharacters
    playSound SndHeal
    forM_ charNums $ \charNum -> do
      -- TODO add doodad and wait
      restoreMojoToFull charNum
      unless (charNum == caster) $ do
        alterAdrenaline charNum (const maxAdrenaline)
featEffect Zodiac =
  StandardFeat AutoTarget $ \_ () -> do
    entries <- randomPermutation =<< getAllEnemyMonsters
    forM_ entries $ \entry -> do
      damage <- getRandomR 40 60 -- TODO how much damage?
      dealDamage [(HitMonster (Grid.geKey entry), EnergyDamage, damage)]
      -- TODO doodad/sound
      wait 3
featEffect Eclipse =
  StandardFeat AutoTarget $ \_caster () -> do
    hitTargets <- getAllAllyTargets
    playSound SndIllusion
    forM_ hitTargets $ \hitTarget -> do
      -- TODO add doodad and wait
      grantInvisibility hitTarget MajorInvisibility
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
featCastingCost _ = AdrenalineCost 1 -- FIXME

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
featDescription Longshot = "Fire an arrow at +3 range and +25% damage."
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
featIconCoords Longshot = (7, 4)
featIconCoords Multishot = (7, 3)
featIconCoords _ = (6, 3) -- FIXME

-------------------------------------------------------------------------------
{-
getFeat :: FeatTag -> CombatFeat
getFeat Concentrate = CombatFeat
  { cfName = "Concentrate",
    cfDescription = "Use an ability at 110% power",
    cfIconCoords = (0, 0), -- FIXME
    cfCastingCost = (AdrenalineCost 100),
    cfEffect = MetaAbility NormalCost 1.1 }
getFeat Offering = CombatFeat
  { cfName = "Offering",
    cfDescription = "Use an ability for free, at triple power",
    cfIconCoords = (9, 0),
    cfCastingCost = AdrenalineCost 10,
    cfEffect = MetaAbility ZeroCost 3.0 }
getFeat SolarFlare = CombatFeat
  { cfName = "Solar Flare",
    cfDescription = "Deal massive damage to three targets",
    cfIconCoords = (9, 1),
    cfCastingCost = AdrenalineCost 30,
    cfEffect = StandardFeat (MultiTarget 6 3) $ \_ _ -> do
      return () } -- TODO
getFeat Energize = CombatFeat
  { cfName = "Energize",
    cfDescription = "Restore mana/focus/adrenaline for all allies",
    cfIconCoords = (9, 2),
    cfCastingCost = AdrenalineCost 100,
    cfEffect = StandardFeat AutoTarget $ \caster () -> do
      -- TODO add doodads
      playSound SndHeal
      forM_ [minBound .. maxBound] $ \charNum -> do
        restoreMojoToFull charNum
        unless (charNum == caster) $ do
          alterAdrenaline charNum (const maxAdrenaline) }
getFeat StarShield = CombatFeat
  { cfName = "Star Shield",
    cfDescription = "Put a powerful shield around all allies",
    cfIconCoords = (8, 0),
    cfCastingCost = AdrenalineCost 25,
    cfEffect = StandardFeat AutoTarget $ \_ () -> do
      return () } -- TODO
getFeat Zodiac = CombatFeat
  { cfName = "Zodiac",
    cfDescription = "Deal energy damage to all enemies",
    cfIconCoords = (8, 1),
    cfCastingCost = AdrenalineCost 50,
    cfEffect = StandardFeat AutoTarget $ \_ () -> do
      entries <- randomPermutation =<< getAllEnemyMonsters
      forM_ entries $ \entry -> do
        damage <- getRandomR 40 60 -- TODO how much damage?
        dealDamage [(HitMonster (Grid.geKey entry), EnergyDamage, damage)]
        -- TODO doodad/sound
        wait 3
      return () } -- TODO
getFeat Imprison = CombatFeat
  { cfName = "Imprison",
    cfDescription = "Trap a group of enemies within a barrier",
    cfIconCoords = (8, 2),
    cfCastingCost = AdrenalineCost 100,
    cfEffect = StandardFeat AutoTarget $ \_ () -> do
      return () } -- TODO
getFeat TidalForce = CombatFeat
  { cfName = "Tidal Force",
    cfDescription = "Damage and daze everything in an area",
    cfIconCoords = (7, 0),
    cfCastingCost = AdrenalineCost 40,
    cfEffect = StandardFeat (aoeTarget 7 (ofRadius 2)) $
      \_ _ -> do
        return () } -- TODO
getFeat Eclipse = CombatFeat
  { cfName = "Eclipse",
    cfDescription = "Grant major invisibility to the whole party",
    cfIconCoords = (7, 1),
    cfCastingCost = AdrenalineCost 60,
    cfEffect = StandardFeat AutoTarget $ \_ () -> do
      -- TODO add doodads
      playSound SndIllusion
      mapM_ (\charNum -> grantInvisibility (HitCharacter charNum)
                                           MajorInvisibility)
            [minBound .. maxBound] }
getFeat LunarBeam = CombatFeat
  { cfName = "Lunar Beam",
    cfDescription = "Shoot a devastating beam of ice",
    cfIconCoords = (7, 2),
    cfCastingCost = AdrenalineCost 100,
    cfEffect = StandardFeat beamTarget $ \_ _ -> do
      return () } -- TODO
getFeat PulseOfLife = CombatFeat
  { cfName = "Pulse of Life",
    cfDescription = "Revive and heal one ally",
    cfIconCoords = (6, 0),
    cfCastingCost = AdrenalineCost 50,
    cfEffect = StandardFeat AutoTarget $ \_ () -> do
      return () } -- TODO
getFeat Avatar = CombatFeat
  { cfName = "Avatar",
    cfDescription = "Become a mighty warrior",
    cfIconCoords = (6, 1),
    cfCastingCost = AdrenalineCost 80,
    cfEffect = StandardFeat AutoTarget $ \_ () -> do
      return () } -- TODO
getFeat AllCreation = CombatFeat
  { cfName = "All Creation",
    cfDescription = "Summon a multitude of creatures",
    cfIconCoords = (6, 2),
    cfCastingCost = AdrenalineCost 100,
    cfEffect = StandardFeat AutoTarget $ \_ () -> do
      return () } -- TODO
getFeat _ = CombatFeat -- TODO
  { cfName = "Do Nothing",
    cfDescription = "Really, do nothing",
    cfIconCoords = (9, 0),
    cfCastingCost = NoCost,
    cfEffect = StandardFeat AutoTarget $ \_ () -> debug "I did nothing." }
-}
-------------------------------------------------------------------------------
