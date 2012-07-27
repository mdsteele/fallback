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
import Fallback.State.Status (seMentalEffect)
import Fallback.Utility (forEither)

-------------------------------------------------------------------------------

defaultMonsterCombatAI :: Grid.Key Monster -> Script CombatEffect ()
defaultMonsterCombatAI key = do
  charmed <- do
    se <- getStatus (HitMonster key)
    case seMentalEffect se of
      Just Dazed -> return False
      Just Confused -> randomBool 0.5
      Just Charmed -> return True
      Nothing -> return False
  if charmed then attackAI True key else do
  done <- tryMonsterSpells key
  unless done $ attackAI False key

attackAI :: Bool -> Grid.Key Monster -> Script CombatEffect ()
attackAI charmed key = do
  ge <- demandMonsterEntry key
  let attacks = monstAttacks $ Grid.geValue ge
  if null attacks then fleeAI charmed ge else do
  attack <- getRandomElem attacks
  let sqDist = rangeSqDist $ maRange attack
  isBlocked <- areaGet (arsIsBlockedForMonster ge)
  goals <- getMonsterOpponentPositions charmed key
  if null goals then drunkAI key else do
  loopM (4 :: Int) $ \actionPoints -> do
    entry <- demandMonsterEntry key
    let rect = Grid.geRect entry
    case pathfindRectToRanges isBlocked rect goals sqDist 20 of
      Just (pos : _) -> do
        -- TODO: Invite attacks of opportunity
        walkMonster 4 key pos
        let entangled = monstIsEntangled $ Grid.geValue entry
        let actionPoints' = actionPoints - (if entangled then 2 else 1)
        return $ if actionPoints' <= 0 then Nothing else Just actionPoints'
      Just [] -> Nothing <$ do
        visible <- getMonsterFieldOfView key
        let targets = filter (\pos -> rangeTouchesRect pos sqDist rect &&
                              Set.member pos visible) goals
        if null targets then return () else do
        target <- getRandomElem targets
        monsterPerformAttack key attack target baseAttackModifiers
          { amCanMiss = True }
      Nothing -> return Nothing

drunkAI :: Grid.Key Monster -> Script CombatEffect ()
drunkAI _key = return () -- TODO move randomly

fleeAI :: Bool -> Grid.Entry Monster -> Script CombatEffect ()
fleeAI _charmed _ge = do
  return () -- FIXME run away from opponents

loopM :: (Monad m) => a -> (a -> m (Maybe a)) -> m ()
loopM input fn = maybe (return ()) (flip loopM fn) =<< fn input

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
                   -> ([(MonsterSpell, Int)] -> [(MonsterSpell, Int)])
                   -> Script f ()
alterMonsterSpells key fn = do
  withMonsterEntry key $ \entry -> do
    let monst = Grid.geValue entry
    emitAreaEffect $ EffReplaceMonster key $
      Just monst { monstSpells = fn (monstSpells monst) }

-------------------------------------------------------------------------------
