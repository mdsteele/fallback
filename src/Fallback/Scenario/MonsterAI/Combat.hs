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

module Fallback.Scenario.MonsterAI.Combat
  (defaultMonsterCombatAI)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow ((***), second)
import Control.Monad (forM, unless)
import Data.List (partition)
import qualified Data.Set as Set

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.MonsterAI.Script
import Fallback.Scenario.MonsterAI.Spells (prepMonsterSpell)
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Pathfind (pathfindRectToRanges)
import Fallback.State.Simple
import Fallback.State.Tags (MonsterSpellTag)
import Fallback.Utility (forEither)

-------------------------------------------------------------------------------

defaultMonsterCombatAI :: Grid.Key Monster -> Script CombatEffect ()
defaultMonsterCombatAI key = do
  done <- tryMonsterSpells key
  unless done $ do
  ge <- demandMonsterEntry key
  let attacks = monstAttacks $ Grid.geValue ge
  if null attacks then fleeMonsterCombatAI ge else do
  attack <- getRandomElem attacks
  let sqDist = rangeSqDist $ maRange attack
  isBlocked <- areaGet (arsIsBlockedForMonster ge)
  goals <- getMonsterOpponentPositions key
  loopM (4 :: Int) $ \ap -> do
    rect <- Grid.geRect <$> demandMonsterEntry key
    case pathfindRectToRanges isBlocked rect goals sqDist 20 of
      Just (pos : _) -> do
        walkMonster 4 key pos
        return $ if ap <= 1 then Nothing else Just (ap - 1)
      Just [] -> Nothing <$ do
        visible <- getMonsterFieldOfView key
        let targets = filter (\pos -> rangeTouchesRect pos sqDist rect &&
                              Set.member pos visible) goals
        if null targets then return () else do
        target <- getRandomElem targets
        monsterPerformAttack key attack target
      Nothing -> return Nothing

loopM :: (Monad m) => a -> (a -> m (Maybe a)) -> m ()
loopM input fn = maybe (return ()) (flip loopM fn) =<< fn input

fleeMonsterCombatAI :: Grid.Entry Monster -> Script CombatEffect ()
fleeMonsterCombatAI _ge = do
  return () -- FIXME

-------------------------------------------------------------------------------

tryMonsterSpells :: Grid.Key Monster -> Script CombatEffect Bool
tryMonsterSpells key = do
  alterMonsterSpells key $ map $ second $ max 0 . subtract 1
  entry <- demandMonsterEntry key
  let (ready, notReady) = partition ((0 >=) . snd) $ monstSpells $
                          Grid.geValue entry
  prepped <- forM ready $ \(tag, _) -> (,) tag <$> prepMonsterSpell tag entry
  let (able, notAble) =
        forEither prepped $ \(tag, mbCasting) ->
          case mbCasting of
            Nothing -> Right (tag, 0)
            Just (impact, action) -> Left (impact, (tag, action))
  if null able then return False else do
  let (best, notBest) = (map snd *** map snd) $
                        partition ((maximum (map fst able) ==) . fst) able
  ((chosenTag, chosenAction), notChosen) <- removeRandomElem best
  alterMonsterSpells key $ const $
    map (flip (,) 0 . fst) (notChosen ++ notBest) ++ notAble ++ notReady
  cooldown <- chosenAction
  alterMonsterSpells key ((chosenTag, cooldown) :)
  return True

-- | Make changes to the monster's spells, if the monster's still alive.
alterMonsterSpells :: (FromAreaEffect f) => Grid.Key Monster
                   -> ([(MonsterSpellTag, Int)] -> [(MonsterSpellTag, Int)])
                   -> Script f ()
alterMonsterSpells key fn = do
  withMonsterEntry key $ \entry -> do
    let monst = Grid.geValue entry
    emitAreaEffect $ EffReplaceMonster key $
      Just monst { monstSpells = fn (monstSpells monst) }

-------------------------------------------------------------------------------
