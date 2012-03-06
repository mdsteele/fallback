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

module Fallback.Scenario.MonsterAI
  (defaultMonsterCombatAI, monsterTownStep)
where

import Control.Monad (unless, when)

import Fallback.Data.Grid (GridEntry(..))
import Fallback.Data.Point
import Fallback.Scenario.MonsterSpells (tryMonsterSpell)
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Pathfind (pathfindRectToRange, pathfindRectToRanges)
import Fallback.State.Simple
import Fallback.State.Tags (MonsterSpellTag)
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------

-- FIXME Move this to another file, presumably in Scenario
defaultMonsterCombatAI :: GridEntry Monster -> Script CombatEffect ()
defaultMonsterCombatAI ge = do
  done <- tryMonsterSpells ge (mtSpells $ monstType $ geValue ge)
  unless done $ do
  let attacks = mtAttacks $ monstType $ geValue ge
  if null attacks then fleeMonsterCombatAI ge else do
  attack <- getRandomElem attacks
  isBlocked <- areaGet (arsIsBlockedForMonster ge)
  goals <- areaGet arsPartyPositions -- TODO also include party allies
  let rect = geRect ge
  let sqDist = rangeSqDist $ maRange attack
  let path = pathfindRectToRanges isBlocked rect goals sqDist 20
  if null path then return () else do
  let path' = take 4 $ drop 1 path
  mapM_ (walkMonster 4 $ geKey ge) path'
  if length path' > 3 then return () else do
  let targets = filter (flip3 rangeTouchesRect sqDist rect) goals
  if null targets then return () else do
  target <- getRandomElem targets
  monsterPerformAttack (geKey ge) attack target

fleeMonsterCombatAI :: GridEntry Monster -> Script CombatEffect ()
fleeMonsterCombatAI _ge = do
  return () -- FIXME

tryMonsterSpells :: GridEntry Monster -> [MonsterSpellTag]
                 -> Script CombatEffect Bool
tryMonsterSpells _ [] = return False
tryMonsterSpells ge (spell : spells) = do
  done <- tryMonsterSpell spell ge
  if done then return True else tryMonsterSpells ge spells

-------------------------------------------------------------------------------

-- | Given a monster in town mode, return the script to run for the monster's
-- turn.  The script returns 'True' if the monster wants to start combat.
monsterTownStep :: GridEntry Monster -> Script TownEffect Bool
monsterTownStep ge = do
  isBlocked <- areaGet (arsIsBlockedForMonster ge)
  partyPos <- getPartyPosition
  case monstTownAI monst of
    ChaseAI -> do
      if rectTopleft rect `pSqDist` partyPos > ofRadius 25 then
        return False else do
      let path = pathfindRectToRange isBlocked rect partyPos 2 30
      if null path then return False else do
      remaining <- takeStep path
      return (remaining <= 3)
    GuardAI home -> do
      let partyPath = pathfindRectToRange isBlocked rect partyPos 2 5
      -- TODO only chase the party so far from home
      if null partyPath then do
        let homePath = pathfindRectToRange isBlocked rect home 0 30
        unless (null homePath) $ do
          _ <- takeStep homePath
          return ()
        return False
       else do
        remaining <- takeStep partyPath
        return (remaining <= 3)
    ImmobileAI -> do
      if monstIsAlly monst then return False else do
      let attacks = mtAttacks $ monstType monst
      if null attacks then return False else do
      --let attack = maximumKey (rangeSqDist . maRange) attacks
      return False -- FIXME
    PatrolAI home goal -> do
      let partyPath = pathfindRectToRange isBlocked rect partyPos 2 5
      if null partyPath then do
        let patrolPath = pathfindRectToRange isBlocked rect goal 0 30
        unless (null patrolPath) $ do
          remaining <- takeStep patrolPath
          when (remaining <= 0) $ do
            emitAreaEffect $ EffReplaceMonster (geKey ge) $
              Just monst { monstTownAI = PatrolAI goal home }
        return False
       else do
        remaining <- takeStep partyPath
        return (remaining <= 3)
    _ -> return False -- FIXME
  where
    monst = geValue ge
    rect = geRect ge
    takeStep path = do
      let (time, steps) = if mtWalksFast $ monstType monst
                          then (2, 2) else (4, 1)
      mapM_ (walkMonster time $ geKey ge) $ take steps $ drop 1 path
      return (length path - steps - 1)

-------------------------------------------------------------------------------
