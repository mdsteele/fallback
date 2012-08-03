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

module Fallback.Scenario.MonsterAI.Script
  (getFieldOfViewFrom, getMonsterFieldOfView, getMonsterOpponentPositions,
   getMonsterVisibleTargetsInRange, planCombatPath)
where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (guard)
import Data.List (maximumBy, minimumBy)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set

import Fallback.Constants (maxActionPoints, sightRangeSquared)
import qualified Fallback.Data.Grid as Grid
import qualified Fallback.Data.PriorityQueue as PQ
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.FOV (fieldOfView)
import Fallback.State.Party (chrStatus)
import Fallback.State.Simple (DamageType(..), Field(..), Invisibility(..))
import Fallback.State.Status (seInvisibility)
import Fallback.State.Terrain (terrainSize)
import Fallback.Utility (forMaybeM)

-------------------------------------------------------------------------------

-- | Returnt the set of positions that would be visible to a creature occupying
-- the given 'PRect'.
getFieldOfViewFrom :: (FromAreaEffect f) => PRect
                   -> Script f (Set.Set Position)
getFieldOfViewFrom prect = do
  size <- areaGet (terrainSize . arsTerrain)
  isOpaque <- areaGet arsIsOpaque
  return (foldr (fieldOfView size isOpaque sightRangeSquared) Set.empty $
          prectPositions prect)

-- | Return the set of positions visible to the specified monster.
getMonsterFieldOfView :: (FromAreaEffect f) => Grid.Key Monster
                      -> Script f (Set.Set Position)
getMonsterFieldOfView key = do
  maybeMonsterEntry key Set.empty $ \entry -> do
  getFieldOfViewFrom $ Grid.geRect entry

-- | Return the list of positions occupied by foes of the given monster,
-- excepting those that are invisible to the monster.  The first argument
-- indicates whether the monster is charmed; if it is true, this will return
-- positions of the monster's friends instead.
getMonsterOpponentPositions :: (FromAreaEffect f) => Bool -> Grid.Key Monster
                            -> Script f [Position]
getMonsterOpponentPositions charmed key = do
  maybeMonsterEntry key [] $ \entry -> do
  let isAlly = charmed /= monstIsAlly (Grid.geValue entry)
  let eyePrect = expandPrect $ Grid.geRect entry
  positions1 <- if isAlly then return [] else do
    charNums <- getAllConsciousCharacters
    forMaybeM charNums $ \charNum -> do
      let getPosition = areaGet (arsCharacterPosition charNum)
      if charmed then Just <$> getPosition else do
      invis <- areaGet (seInvisibility . chrStatus . arsGetCharacter charNum)
      case invis of
        NoInvisibility -> Just <$> getPosition
        MinorInvisibility -> do
          pos <- getPosition
          return $ if rectContains eyePrect pos then Just pos else Nothing
        MajorInvisibility -> return Nothing
  positions2 <- do
    entries <- if isAlly then getAllEnemyMonsters else getAllAllyMonsters
    return $ flip concatMap entries $ \entry' ->
      if Grid.geKey entry' == key then [] else
        let prect' = Grid.geRect entry'
            positions = prectPositions prect'
        in if charmed then positions else
             case monstInvisibility $ Grid.geValue entry' of
               NoInvisibility -> positions
               MinorInvisibility ->
                 if rectIntersects eyePrect prect' then positions else []
               MajorInvisibility -> []
  return (positions1 ++ positions2)

-- | Return the list of positions that are 1) occupied by foes of the monster,
-- 2) visible to the monster, and 3) within the given radius of the monster's
-- head.
getMonsterVisibleTargetsInRange :: (FromAreaEffect f) => Int
                                -> Grid.Key Monster -> Script f [Position]
getMonsterVisibleTargetsInRange radius key = do
  viewField <- getMonsterFieldOfView key
  headPos <- getMonsterHeadPos key
  let ok pos = pos `pSqDist` headPos <= ofRadius radius &&
               pos `Set.member` viewField
  filter ok <$> getMonsterOpponentPositions False key

-------------------------------------------------------------------------------

-- | Plan out the best path for a monster to walk on a combat turn, assuming it
-- starts with full action points.  The given evaluation function takes the
-- final rect occupied by the monster, and a boolean indicating whether the
-- monster would still be able to attack after walking this path; it returns a
-- double indicating how desirable that situation would be (higher is better),
-- and any extra desired information.  The best possible destination and path
-- will be selected, by subtracting a penalty from each destination score
-- (roughly equal to the expected damage taken by walking to that destination,
-- e.g. from fields or attacks of opportunity) and picking the maximum result;
-- the final return value of this function gives the path the monster should
-- walk to get to the chosen destination, and the extra information that was
-- returned for that destination.
planCombatPath :: Grid.Key Monster
               -> (PRect -> Bool -> Script CombatEffect (Double, a))
               -> Script CombatEffect ([Position], a)
planCombatPath key evaluationFn = do
  entry <- demandMonsterEntry key
  let size = rectSize (Grid.geRect entry)
  let invisible = monstInvisibility (Grid.geValue entry) > NoInvisibility
  let damagePenalty dmgType damage =
        expectedDamage (Grid.geValue entry) dmgType damage +
        (if invisible then 5 else 0)
  isBlocked <- areaGet (arsIsBlockedForMonster entry)
  fieldsPenalty <- do
    fields <- areaGet arsFields
    let fieldPenalty pos =
          case Map.lookup pos fields of
            Just (FireWall dmg) -> (False, damagePenalty FireDamage dmg)
            Just (IceWall dmg) -> (False, damagePenalty ColdDamage dmg)
            Just (PoisonCloud dmg) -> (False, damagePenalty AcidDamage dmg)
            Just (Webbing rounds) -> (True, 20 + rounds)
            _ -> (False, 0)
    return ((or *** sum) . unzip . map fieldPenalty . prectPositions .
            flip makeRect size)
  let makeChildPlan plans plan dir = do
        let pos' = cpPosition plan `plusDir` dir
        guard $ not $ isBlocked pos'
        -- Calculate penalties:
        let (entangle, fieldPenalty) = fieldsPenalty pos'
        let movePenalty = if isCardinal dir then 0.02 else 0.03
        -- TODO include AoO penalties
        let plan' = CombatPlan
              { cpEntangled = cpEntangled plan || entangle,
                cpPenalty = cpPenalty plan + fieldPenalty + movePenalty,
                cpPosition = pos',
                cpRemainingAP = cpRemainingAP plan -
                                (if cpEntangled plan then 2 else 1),
                cpReversePath = pos' : cpReversePath plan }
        let isBetter plan'' = cpRemainingAP plan'' >= cpRemainingAP plan' &&
                              cpPenalty plan'' <= cpPenalty plan'
        guard $ maybe True (not . any isBetter) (Map.lookup pos' plans)
        Just plan'
  let search queue plans =
        case PQ.pop queue of
          Just (plan, queue') ->
            let children = mapMaybe (makeChildPlan plans plan) allDirections
                enqueue plan' = PQ.insert (cpPriority plan) plan'
                record plan' = Map.insertWith (++) (cpPosition plan') [plan']
            in search (foldr enqueue queue' $
                       filter ((0 <) . cpRemainingAP) children)
                      (foldr record plans children)
          Nothing -> map (minimumBy $ comparing cpPenalty) $ Map.elems plans
  let evaluatePlan plan = do
        (score, info) <- evaluationFn (makeRect (cpPosition plan) size)
                                      (cpRemainingAP plan > 0)
        -- Add an extra penalty for ending on damaging fields.
        let stoppingPenalty = snd $ fieldsPenalty $ cpPosition plan
        return (score - cpPenalty plan - stoppingPenalty,
                (reverse (cpReversePath plan), info))
  let initPlan = CombatPlan
        { cpEntangled = monstIsEntangled (Grid.geValue entry),
          cpPenalty = 0, cpPosition = rectTopleft $ Grid.geRect entry,
          cpRemainingAP = maxActionPoints, cpReversePath = [] }
  fmap (snd . maximumBy (comparing fst)) $ mapM evaluatePlan $
    search (PQ.singleton (cpPriority initPlan) initPlan)
           (Map.singleton (cpPosition initPlan) [initPlan])

-- Penalties:
-- * Moving onto webbing -> 20 + duration
-- * Moving onto a damaging field -> dmg * resist (+10 if invisible)
-- * Ending on a damaging field -> dmg * resist * 4 / speed (+20 if invisible)
-- * Moving away from a foe (unless invisible) -> expected dmg

data CombatPlan = CombatPlan
  { cpEntangled :: Bool, -- is the monster entangled by now?
    cpPenalty :: Double, -- roughly, expected damage taken on this path
    cpPosition :: Position, -- monster position at end of path
    cpRemainingAP :: Int, -- remaining action points after following path
    cpReversePath :: [Position] } -- reversed path to follow

cpPriority :: CombatPlan -> (Int, Double)
cpPriority plan =
  (negate ((if cpEntangled plan then half else id) (cpRemainingAP plan)),
   cpPenalty plan)

-------------------------------------------------------------------------------
