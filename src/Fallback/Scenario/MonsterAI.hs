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

import Control.Applicative ((<$), (<$>))
import Control.Monad (unless, when)
import qualified Data.Set as Set

import Fallback.Constants (sightRangeSquared)
import Fallback.Data.Grid (GridEntry(..), GridKey, rectPositions)
import Fallback.Data.Point
import Fallback.Scenario.MonsterSpells (tryMonsterSpell)
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.FOV (fieldOfView)
import Fallback.State.Pathfind (pathfindRectToRange, pathfindRectToRanges)
import Fallback.State.Simple
import Fallback.State.Tags (MonsterSpellTag)
import Fallback.State.Terrain (terrainSize)

-------------------------------------------------------------------------------

defaultMonsterCombatAI :: GridEntry Monster -> Script CombatEffect ()
defaultMonsterCombatAI ge = do
  done <- tryMonsterSpells ge (mtSpells $ monstType $ geValue ge)
  unless done $ do
  let attacks = mtAttacks $ monstType $ geValue ge
  if null attacks then fleeMonsterCombatAI ge else do
  attack <- getRandomElem attacks
  isBlocked <- areaGet (arsIsBlockedForMonster ge)
  goals <- getMonsterOpponentPositions (geKey ge)
  let rect = geRect ge
  let sqDist = rangeSqDist $ maRange attack
  let path = pathfindRectToRanges isBlocked rect goals sqDist 20
  if null path then return () else do
  let path' = take 4 $ drop 1 path
  mapM_ (walkMonster 4 $ geKey ge) path'
  if length path' > 3 then return () else do
  visible <- getMonsterVisibility (geKey ge)
  let targets = filter (\pos -> rangeTouchesRect pos sqDist rect &&
                                Set.member pos visible) goals
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
      stepTowardsParty path
    GuardAI home -> do
      let partyPath = pathfindRectToRange isBlocked rect partyPos 2 5
      -- TODO only chase the party so far from home
      if not (null partyPath) then stepTowardsParty partyPath else do
      let homePath = pathfindRectToRange isBlocked rect home 0 30
      unless (null homePath) $ () <$ takeStep homePath
      return False
    ImmobileAI -> do
      if monstIsAlly monst then return False else do
      let attacks = mtAttacks $ monstType monst
      if null attacks then return False else do
      let sqDist = maximum $ map (rangeSqDist . maRange) attacks
      if not (rangeTouchesRect partyPos sqDist rect) then return False else do
      canMonsterSeeParty (geKey ge)
    MindlessAI -> do
      canSee <- canMonsterSeeParty (geKey ge)
      if not canSee then return False else do
      let path = pathfindRectToRange isBlocked rect partyPos 2 20
      if null path then return False else do
      stepTowardsParty path
    PatrolAI home goal -> do
      let partyPath = pathfindRectToRange isBlocked rect partyPos 2 5
      if not (null partyPath) then stepTowardsParty partyPath else do
      let patrolPath = pathfindRectToRange isBlocked rect goal 0 30
      unless (null patrolPath) $ do
        remaining <- takeStep patrolPath
        when (remaining <= 0) $ do
          setMonsterTownAI (geKey ge) (PatrolAI goal home)
      return False
  where
    monst = geValue ge
    rect = geRect ge
    takeStep path = do
      let (time, steps) = if mtWalksFast $ monstType monst
                          then (2, 2) else (4, 1)
      mapM_ (walkMonster time $ geKey ge) $ take steps $ drop 1 path
      return (length path - steps - 1)
    stepTowardsParty path = do
      remaining <- takeStep path
      return (remaining <= 3 && not (monstIsAlly monst))

-------------------------------------------------------------------------------

getMonsterOpponentPositions :: (FromAreaEffect f) => GridKey Monster
                            -> Script f [Position]
getMonsterOpponentPositions key = do
  maybeMonsterEntry key [] $ \entry -> do
  let isAlly = monstIsAlly $ geValue entry
  positions1 <- if isAlly then return [] else areaGet arsPartyPositions
  positions2 <- concatMap (rectPositions . geRect) <$>
                if isAlly then getAllEnemyMonsters else getAllAllyMonsters
  return (positions1 ++ positions2)

-- | Return the set of positions visible to the specified monster.
getMonsterVisibility :: (FromAreaEffect f) => GridKey Monster
                     -> Script f (Set.Set Position)
getMonsterVisibility key = do
  maybeMonsterEntry key Set.empty $ \entry -> do
  size <- terrainSize <$> areaGet arsTerrain
  isOpaque <- areaGet arsIsOpaque
  return (foldr (fieldOfView size isOpaque sightRangeSquared) Set.empty $
          rectPositions $ geRect entry)

-- | Return 'True' if the monster can see the party, 'False' otherwise.
canMonsterSeeParty :: GridKey Monster -> Script TownEffect Bool
canMonsterSeeParty key = do
  maybeMonsterEntry key False $ \entry -> do
  visible <- areaGet arsVisibleForParty
  return $ any (`Set.member` visible) $ rectPositions $ geRect entry

-- | Call an action with the monster's grid entry, or return the given default
-- value if the monster doesn't exist.
maybeMonsterEntry :: (FromAreaEffect f) => GridKey Monster -> a
                  -> (GridEntry Monster -> Script f a) -> Script f a
maybeMonsterEntry key defaultValue action = do
  mbEntry <- lookupMonsterEntry key
  maybe (return defaultValue) action mbEntry

-------------------------------------------------------------------------------
