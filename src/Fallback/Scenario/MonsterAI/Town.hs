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

import Control.Applicative ((<$>))
import Control.Monad (unless, void, when)
import qualified Data.Set as Set

import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Script (demandTerrainRect, lookupTerrainMark)
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Pathfind (pathfindRectToRange, pathfindRectToRanges)
import Fallback.State.Simple
import Fallback.Utility (maybeM)

-------------------------------------------------------------------------------

-- | Given a monster in town mode, return the script to run for the monster's
-- turn.  The script returns 'True' if the monster wants to start combat.
monsterTownStep :: Grid.Entry Monster -> Script TownEffect Bool
monsterTownStep ge = do
  isBlocked <- areaGet (arsIsBlockedForMonster ge)
  partyPos <- getPartyPosition
  -- TODO: if (ally and?) party can't move, move away from party (unless
  --       Immobile)
  case monstTownAI monst of
    ChaseAI -> do
      if rectTopleft rect `pSqDist` partyPos > ofRadius 25 then
        return False else do
      maybe (return False) stepTowardsParty
            (pathfindRectToRange isBlocked rect partyPos (SqDist 2) 30)
    DrunkAI zoneKey -> do
      zone <- demandTerrainRect zoneKey
      -- TODO if non-ally and party nearby, chase party
      -- TODO if outside of zone, pathfind back to zone
      pos' <- (rectTopleft rect `plusDir`) <$> getRandomElem allDirections
      when (rectContains zone pos') $ do
        blocked <- areaGet (flip (arsIsBlockedForMonster ge) pos')
        unless blocked $ takeStep_ [pos']
      return False
    GuardAI chaseSteps homeMark faceDir -> do
      case if ally then Nothing else
             pathfindRectToRange isBlocked rect partyPos
                                 (SqDist 2) chaseSteps of
        -- TODO: if already far from home, don't chase party
        Just path -> stepTowardsParty path
        Nothing -> do
          homePositions <- lookupTerrainMark homeMark
          maybeM (pathfindRectToRanges isBlocked rect homePositions
                                       (SqDist 0) 60) $ \path -> do
            if null path then setMonsterFaceDir key faceDir else takeStep_ path
          return False
    ImmobileAI -> do
      if ally then return False else do
      let attacks = monstAttacks monst
      if null attacks then return False else do
      let sqDist = maximum $ map (rangeSqDist . maRange) attacks
      if prectSqDistToPosition rect partyPos > sqDist then return False else do
      canMonsterSeeParty key
    MindlessAI -> do
      canSee <- canMonsterSeeParty key
      if not canSee then return False else do
      maybe (return False) stepTowardsParty
            (pathfindRectToRange isBlocked rect partyPos (SqDist 2) 20)
    PatrolAI homeMark goalMark -> do
      case if ally then Nothing else
             pathfindRectToRange isBlocked rect partyPos (SqDist 2) 5 of
        Just path -> stepTowardsParty path
        Nothing -> do
          goals <- lookupTerrainMark goalMark
          maybeM (pathfindRectToRanges isBlocked rect goals (SqDist 0) 30) $
                 \path -> do
            remaining <- takeStep path
            when (remaining <= 0) $ do
              setMonsterTownAI (PatrolAI goalMark homeMark) key
          return False
  where
    monst = Grid.geValue ge
    rect = Grid.geRect ge
    key = Grid.geKey ge
    ally = monstIsAlly monst
    takeStep_ path = void $ takeStep path
    takeStep path = if null path then return 0 else do
      let pathLength = length path
      let (frames, numSteps) = if monstWalksFast monst && pathLength > 1
                               then (2, 2) else (4, 1)
      mapM_ (walkMonster frames key) $ take numSteps path
      return (pathLength - numSteps)
    stepTowardsParty path = do
      remaining <- takeStep path
      return (remaining <= 3 && not ally)

-------------------------------------------------------------------------------

-- | Return 'True' if the party is within the monster's field of view, 'False'
-- otherwise.
canMonsterSeeParty :: Grid.Key Monster -> Script TownEffect Bool
canMonsterSeeParty key = do
  maybeMonsterEntry key False $ \entry -> do
  visible <- areaGet arsVisibleForParty
  return $ any (`Set.member` visible) $ prectPositions $ Grid.geRect entry

-------------------------------------------------------------------------------
