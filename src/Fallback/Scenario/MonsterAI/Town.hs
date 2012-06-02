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

module Fallback.Scenario.MonsterAI.Town
  (monsterTownStep)
where

import Control.Applicative ((<$), (<$>))
import Control.Monad (unless, when)
import qualified Data.Set as Set

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Pathfind (pathfindRectToRange)
import Fallback.State.Simple
import Fallback.Utility (maybeM)

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
            (pathfindRectToRange isBlocked rect partyPos (SqDist 2) 30)
    DrunkAI zone -> do
      -- TODO if non-ally and party nearby, chase party
      -- TODO if outside of zone, pathfind back to zone
      pos' <- (rectTopleft rect `plusDir`) <$> getRandomElem allDirections
      when (rectContains zone pos') $ do
        blocked <- areaGet (flip (arsIsBlockedForMonster ge) pos')
        unless blocked $ takeStep_ [pos']
      return False
    GuardAI home -> do
      case pathfindRectToRange isBlocked rect partyPos (SqDist 2) 5 of
        Just path -> stepTowardsParty path
        Nothing -> do
          maybeM (pathfindRectToRange isBlocked rect home (SqDist 0) 30)
                 takeStep_
          return False
    ImmobileAI -> do
      if monstIsAlly monst then return False else do
      let attacks = monstAttacks monst
      if null attacks then return False else do
      let sqDist = maximum $ map (rangeSqDist . maRange) attacks
      if not (rangeTouchesRect partyPos sqDist rect) then return False else do
      canMonsterSeeParty key
    MindlessAI -> do
      canSee <- canMonsterSeeParty key
      if not canSee then return False else do
      maybe (return False) stepTowardsParty
            (pathfindRectToRange isBlocked rect partyPos (SqDist 2) 20)
    PatrolAI home goal -> do
      case pathfindRectToRange isBlocked rect partyPos (SqDist 2) 5 of
        Just path -> stepTowardsParty path
        Nothing -> do
          maybeM (pathfindRectToRange isBlocked rect goal (SqDist 0) 30) $
                 \path -> do
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
      let (time, steps) = if monstWalksFast monst then (2, 2) else (4, 1)
      mapM_ (walkMonster time key) $ take steps path
      return (length path - steps)
    stepTowardsParty path = do
      remaining <- takeStep path
      return (remaining <= 3 && not (monstIsAlly monst))

-------------------------------------------------------------------------------

-- | Return 'True' if the party is within the monster's field of view, 'False'
-- otherwise.
canMonsterSeeParty :: Grid.Key Monster -> Script TownEffect Bool
canMonsterSeeParty key = do
  maybeMonsterEntry key False $ \entry -> do
  visible <- areaGet arsVisibleForParty
  return $ any (`Set.member` visible) $ prectPositions $ Grid.geRect entry

-------------------------------------------------------------------------------