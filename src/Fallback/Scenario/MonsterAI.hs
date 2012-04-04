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
import qualified Fallback.Data.Grid as Grid
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
import Fallback.Utility (maybeM)

-------------------------------------------------------------------------------

defaultMonsterCombatAI :: Grid.Entry Monster -> Script CombatEffect ()
defaultMonsterCombatAI ge = do
  let key = Grid.geKey ge
  done <- tryMonsterSpells ge (mtSpells $ monstType $ Grid.geValue ge)
  unless done $ do
  let attacks = mtAttacks $ monstType $ Grid.geValue ge
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
        visible <- getMonsterVisibility key
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

tryMonsterSpells :: Grid.Entry Monster -> [MonsterSpellTag]
                 -> Script CombatEffect Bool
tryMonsterSpells _ [] = return False
tryMonsterSpells ge (spell : spells) = do
  done <- tryMonsterSpell spell ge
  if done then return True else tryMonsterSpells ge spells

-------------------------------------------------------------------------------

-- | Given a monster in town mode, return the script to run for the monster's
-- turn.  The script returns 'True' if the monster wants to start combat.
monsterTownStep :: Grid.Entry Monster -> Script TownEffect Bool
monsterTownStep ge = do
  isBlocked <- areaGet (arsIsBlockedForMonster ge)
  partyPos <- getPartyPosition
  case monstTownAI monst of
    ChaseAI -> do
      if rectTopleft rect `pSqDist` partyPos > ofRadius 25 then
        return False else do
      maybe (return False) stepTowardsParty
            (pathfindRectToRange isBlocked rect partyPos 2 30)
    GuardAI home -> do
      case pathfindRectToRange isBlocked rect partyPos 2 5 of
        Just path -> stepTowardsParty path
        Nothing -> do
          maybeM (pathfindRectToRange isBlocked rect home 0 30) takeStep_
          return False
    ImmobileAI -> do
      if monstIsAlly monst then return False else do
      let attacks = mtAttacks $ monstType monst
      if null attacks then return False else do
      let sqDist = maximum $ map (rangeSqDist . maRange) attacks
      if not (rangeTouchesRect partyPos sqDist rect) then return False else do
      canMonsterSeeParty key
    MindlessAI -> do
      canSee <- canMonsterSeeParty key
      if not canSee then return False else do
      maybe (return False) stepTowardsParty
            (pathfindRectToRange isBlocked rect partyPos 2 20)
    PatrolAI home goal -> do
      case pathfindRectToRange isBlocked rect partyPos 2 5 of
        Just path -> stepTowardsParty path
        Nothing -> do
          maybeM (pathfindRectToRange isBlocked rect goal 0 30) $ \path -> do
            remaining <- takeStep path
            when (remaining <= 0) $ do
              setMonsterTownAI key (PatrolAI goal home)
          return False
  where
    monst = Grid.geValue ge
    rect = Grid.geRect ge
    key = Grid.geKey ge
    takeStep_ path = () <$ takeStep path
    takeStep path = do
      let (time, steps) = if mtWalksFast $ monstType monst
                          then (2, 2) else (4, 1)
      mapM_ (walkMonster time key) $ take steps path
      return (length path - steps)
    stepTowardsParty path = do
      remaining <- takeStep path
      return (remaining <= 3 && not (monstIsAlly monst))

-------------------------------------------------------------------------------

getMonsterOpponentPositions :: (FromAreaEffect f) => Grid.Key Monster
                            -> Script f [Position]
getMonsterOpponentPositions key = do
  maybeMonsterEntry key [] $ \entry -> do
  let isAlly = monstIsAlly $ Grid.geValue entry
  positions1 <- if isAlly then return [] else areaGet arsPartyPositions
  positions2 <- concatMap (prectPositions . Grid.geRect) <$>
                if isAlly then getAllEnemyMonsters else getAllAllyMonsters
  return (positions1 ++ positions2)

-- | Return the set of positions visible to the specified monster.
getMonsterVisibility :: (FromAreaEffect f) => Grid.Key Monster
                     -> Script f (Set.Set Position)
getMonsterVisibility key = do
  maybeMonsterEntry key Set.empty $ \entry -> do
  size <- terrainSize <$> areaGet arsTerrain
  isOpaque <- areaGet arsIsOpaque
  return (foldr (fieldOfView size isOpaque sightRangeSquared) Set.empty $
          prectPositions $ Grid.geRect entry)

-- | Return 'True' if the monster can see the party, 'False' otherwise.
canMonsterSeeParty :: Grid.Key Monster -> Script TownEffect Bool
canMonsterSeeParty key = do
  maybeMonsterEntry key False $ \entry -> do
  visible <- areaGet arsVisibleForParty
  return $ any (`Set.member` visible) $ prectPositions $ Grid.geRect entry

-- | Call an action with the monster's grid entry, or return the given default
-- value if the monster doesn't exist.
maybeMonsterEntry :: (FromAreaEffect f) => Grid.Key Monster -> a
                  -> (Grid.Entry Monster -> Script f a) -> Script f a
maybeMonsterEntry key defaultValue action = do
  mbEntry <- lookupMonsterEntry key
  maybe (return defaultValue) action mbEntry

-------------------------------------------------------------------------------
