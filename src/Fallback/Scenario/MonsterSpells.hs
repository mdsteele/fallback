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
  (tryMonsterSpell)
where

import Control.Applicative ((<$>))

import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Resources (StripTag(..))
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags (MonsterSpellTag(..))
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

tryMonsterSpell :: MonsterSpellTag -> Grid.Entry Monster
                -> Script CombatEffect Bool
tryMonsterSpell FireSpray ge = do
  ifRandom 0.35 $ do
  let maxRange = 5
  let rect = Grid.geRect ge
  targets <- randomPermutation =<<
             filter (flip3 rangeTouchesRect (ofRadius maxRange) rect) <$>
             areaGet arsPartyPositions
  -- TODO only hit targets we can see
  ifSatisfies (length targets >= 2) $ do
  monsterBeginOffensiveAction (Grid.geKey ge) (head targets)
  origin <- getMonsterHeadPos (Grid.geKey ge)
  -- TODO sound
  concurrent_ (zip targets [0..]) $ \(target, index) -> do
    wait (index * 4)
    addBlasterDoodad (Tint 255 0 0 128) 6 60 origin target 320 >>= wait
    addBoomDoodadAtPosition FireBoom 3 target
    wait 4
    dealDamage [(HitPosition target, FireDamage, 30)]
    wait 20
tryMonsterSpell IceBomb ge = do
  ifRandom 0.5 $ do
  alterStatus (HitMonster $ Grid.geKey ge)
              (seApplyArmor (-10) . seApplyHaste (10))
  --alterStatus (HitMonster $ geKey ge) (seApplyArmor (-10) . seApplyMagicShield 10)

-------------------------------------------------------------------------------

ifRandom :: (FromAreaEffect f) => Double -> Script f a -> Script f Bool
ifRandom probability action = do
  number <- getRandomR 0 1
  ifSatisfies (probability > number) action

ifSatisfies :: (FromAreaEffect f) => Bool -> Script f a -> Script f Bool
ifSatisfies ok action = do
  if ok then action >> return True else return False

-------------------------------------------------------------------------------
