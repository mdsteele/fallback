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

module Fallback.Scenario.MonsterSpells
  (prepMonsterSpell)
where

import Control.Applicative ((<$>))

import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Resources (SoundTag(..), StripTag(..))
import Fallback.State.Simple
import Fallback.State.Tags (MonsterSpellTag(..))
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

prepMonsterSpell :: MonsterSpellTag -> Grid.Entry Monster
                 -> Script CombatEffect (Maybe (Int, Script CombatEffect Int))
prepMonsterSpell FireSpray ge = do
  ifRandom 0.8 $ do
  let maxRange = 5
  let key = Grid.geKey ge
  let rect = Grid.geRect ge
  targets <- randomPermutation =<<
             filter (flip3 rangeTouchesRect (ofRadius maxRange) rect) <$>
             areaGet arsPartyPositions
  -- TODO only hit targets we can see
  let numTargets = length targets
  ifSatisfies (numTargets >= 2) $ do
  yieldSpell numTargets $ do
  monsterBeginOffensiveAction key (head targets)
  origin <- getMonsterHeadPos key
  playSound SndBreath
  concurrent_ (zip targets [0..]) $ \(target, index) -> do
    wait (index * 4)
    addBlasterDoodad (Tint 255 0 0 128) 6 100 origin target 320 >>= wait
    addBoomDoodadAtPosition FireBoom 3 target
    playSound SndFireDamage
    wait 4
    dealDamage [(HitPosition target, FireDamage, 30)]
    wait 20
  return 3
prepMonsterSpell TeleportAway ge = do
  let rect = adjustRect1 (-1) $ Grid.geRect ge
  numAdjacentFoes <- error "FIXME" rect
  ifRandom (fromIntegral numAdjacentFoes * 0.25) $ do
  -- FIXME pick destination far from foes
  yieldSpell numAdjacentFoes $ do
  -- FIXME perform teleport to destination
  return 5
prepMonsterSpell _ _ = return Nothing -- FIXME

-------------------------------------------------------------------------------

ifRandom :: (FromAreaEffect f) => Double -> Script f (Maybe a)
         -> Script f (Maybe a)
ifRandom probability action = do
  number <- getRandomR 0 1
  ifSatisfies (probability > number) action

ifSatisfies :: (Monad m) => Bool -> m (Maybe a) -> m (Maybe a)
ifSatisfies ok action = if ok then action else return Nothing

yieldSpell :: Int -> Script CombatEffect Int
           -> Script CombatEffect (Maybe (Int, Script CombatEffect Int))
yieldSpell impact action = return $ Just (impact, action)

-------------------------------------------------------------------------------
