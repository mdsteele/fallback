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

module Fallback.Scenario.MonsterAI.Spells
  (prepMonsterSpell)
where

import Control.Applicative ((<$>))
import Control.Arrow (right)
import Control.Monad (filterM, foldM, forM, forM_, void, when)
import Data.List (maximumBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as Set

import Fallback.Constants (framesPerRound)
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.MonsterAI.Script
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Party (partyDifficulty)
import Fallback.State.Pathfind (allPathsFrom)
import Fallback.State.Resources (SoundTag(..), StripTag(..))
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Terrain (positionCenter, prectRect)
import Fallback.Utility (ceilDiv, flip3, sumM)

-------------------------------------------------------------------------------

prepMonsterSpell :: MonsterSpell -> Grid.Entry Monster
                 -> Script CombatEffect (Maybe (Int, Script CombatEffect Int))
prepMonsterSpell CrossBeam ge = do
  let key = Grid.geKey ge
  let isAlly = monstIsAlly (Grid.geValue ge)
  (numTargets, path) <- maximizePosition key $ \pos -> do
    flip sumM [DirE, DirS, DirW, DirN] $ \dir -> do
      targets <- areaGet (flip3 arsBeamPositions pos (pos `plusDir` dir))
      -- TODO don't count foes we can't see
      sumM (scoreForTarget isAlly) targets
  ifSatisfies (numTargets >= 2) $ do
  ifRandom (fromIntegral numTargets * 0.25) $ do
  yieldSpell numTargets $ do
  walkAlongPath key path
  monsterOffensiveAction key 12
  startPos <- getMonsterHeadPos key
  let startPt = positionCenter startPos
  concurrent_ [DirE, DirS, DirW, DirN] $ \dir -> do
    targets <-
      areaGet (flip3 arsBeamPositions startPos (startPos `plusDir` dir))
    if null targets then return () else do
    let endPos = last targets
    let endPt = positionCenter endPos
    addBeamDoodad (Tint 255 32 32 192) startPt endPt 20
    addBoomDoodadAtPoint FireBoom 3 (fmap round endPt)
    hits <- fmap catMaybes $ forM targets $ \target -> do
      mbOccupant <- areaGet (arsOccupant target)
      case mbOccupant of
        Nothing -> return Nothing
        Just _ -> do
          damage <- (50 *) <$> getRandomR 0.7 1.3
          return $ Just (target, (HitPosition target, FireDamage, damage))
    forM_ (map fst hits) $ addBoomDoodadAtPosition FireBoom 3
    wait 4
    dealDamage $ map snd hits
  wait 20
  return 4
prepMonsterSpell FireSpray ge = do
  ifRandom 0.8 $ do
  let maxRange = 5
  let key = Grid.geKey ge
  let rect = Grid.geRect ge
  viewField <- getMonsterFieldOfView key
  targets <- fmap (take 5) $ randomPermutation =<<
             filter (flip3 rangeTouchesRect (ofRadius maxRange) rect) .
             filter (`Set.member` viewField) <$>
             getMonsterOpponentPositions False key
  let numTargets = length targets
  ifSatisfies (numTargets >= 2) $ do
  yieldSpell numTargets $ do
  monsterOffensiveActionToward key (4 * numTargets) (head targets)
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
prepMonsterSpell FrostMissiles ge = do
  numHits <- do
    difficulty <- areaGet (partyDifficulty . arsParty)
    let maxHits = if difficulty < Ruthless then 5 else 8
    let monst = Grid.geValue ge
    let maxHealth = monstMaxHealth monst
    return ((maxHealth - monstHealth monst) `ceilDiv`
            (maxHealth `div` maxHits))
  let key = Grid.geKey ge
  viewField <- getMonsterFieldOfView key
  potentialTargets <- filter (`Set.member` viewField) <$>
                      getMonsterOpponentPositions False key
  ifSatisfies (not $ null potentialTargets) $ do
  yieldSpell numHits $ do
  targets <- fmap fst $ flip3 foldM ([], Set.empty) [1 .. numHits] $
             \(targets, victims) _ -> do
    target <- getRandomElem potentialTargets
    mbVictim <- areaGet (fmap (right Grid.geKey) . arsOccupant target)
    return ((target, maybe False (`Set.notMember` victims) mbVictim) : targets,
            maybe victims (flip Set.insert victims) mbVictim)
  let p0 = rectCenter $ fmap fromIntegral $ prectRect $ Grid.geRect ge
  concurrent_ (zip targets [0..]) $ \((target, knockback), index) -> do
    wait (index * (51 - 3 * index) `div` 2)
    monsterOffensiveActionToward key 8 target
    dir <- if not knockback then getRandomElem allDirections else do
      getRandomElem allDirections -- TODO choose best knockback dir
    p1 <- pAdd p0 . flip pPolar 400 <$> getRandomR 0 (2 * pi)
    let p3 = positionCenter target
    let p2 = p3 `pSub` (fromIntegral <$> dirDelta dir) `pMul` 300
    let delay = 24
    addSwooshDoodad (Tint 128 96 255 192) 4 delay 12 $
      cubicBezierCurve p0 p1 p2 p3
    wait delay
    playSound SndFreeze
    addBoomDoodadAtPosition IceBoom 2 target
    wait 4
    damage <- getRandomR 30 50
    dealDamage [(HitPosition target, ColdDamage, damage)]
    when knockback $ void $ tryKnockBack (HitPosition target) dir
  return 2
prepMonsterSpell (IceBeam cooldown) ge = do
  let key = Grid.geKey ge
  potentialTargets <- getMonsterVisibleTargetsInRange 6 key
  ifSatisfies (not $ null potentialTargets) $ do
  (center, numIces) <- maximizeFor potentialTargets $ \center -> do
    flip sumM (circleArea center $ ofRadius 1) $ \pos -> do
      mbField <- areaGet (Map.lookup pos . arsFields)
      case mbField of
        Just (IceWall _) -> return 0
        _ -> do
          mbOccupant <- areaGet (arsOccupant pos)
          case mbOccupant of
            Just (Left _charNum) -> return 1
            Just (Right monstEntry) ->
              return $ if monstIsAlly (Grid.geValue monstEntry) then 1 else -1
            Nothing -> return 0
  yieldSpell (numIces + 1) $ do
  monsterOffensiveActionToward key 25 center
  playSound SndBreath
  origin <- getMonsterHeadPos key
  addBlasterDoodad (Tint 64 255 255 128) 6 200 origin center 350 >>= wait
  playSound SndFreeze
  addBoomDoodadAtPosition IceBoom 3 center
  wait 4
  damage <- getRandomR 60 90
  setFields (IceWall $ damage / 4) (circleArea center $ ofRadius 1)
  dealDamage [(HitPosition center, ColdDamage, damage)]
  wait 20
  return cooldown
prepMonsterSpell (Shell benefit cooldown duration) ge = do
  ifSatisfies (not $ isBeneficial $ seDefense $ monstStatus $
               Grid.geValue ge) $ do
  yieldSpell benefit $ do
  setMonsterAnim (Grid.geKey ge) (AttackAnim 8)
  playSound SndShielding
  alterMonsterStatus (Grid.geKey ge) $ seApplyDefense (Beneficial duration)
  return cooldown
prepMonsterSpell (SummonOne dep benefit cooldown duration tags) ge = do
  ifSatisfies (not $ null tags) $ do
  tag <- getRandomElem tags
  yieldSpell benefit $ do
  setMonsterAnim (Grid.geKey ge) (AttackAnim 8)
  let lifetime = round (duration * fromIntegral framesPerRound)
  let summoner = Right $ Grid.geKey ge
  degradeMonstersSummonedBy summoner
  playSound SndSummon
  _ <- trySummonMonster summoner tag lifetime dep
  return cooldown
prepMonsterSpell TeleportAway ge = do
  ifSatisfies False $ do -- FIXME
  numAdjacentFoes <- length <$> foesInRect (monstIsAlly $ Grid.geValue ge)
                                           (expandPrect $ Grid.geRect ge)
  ifRandom (fromIntegral numAdjacentFoes * 0.25) $ do
  -- FIXME pick destination far from foes
  yieldSpell numAdjacentFoes $ do
  -- FIXME perform teleport to destination
  return 5
prepMonsterSpell (TurnSelfInvisible cooldown) ge = do
  ifSatisfies (monstInvisibility (Grid.geValue ge) < MajorInvisibility) $ do
  ifRandom 0.5 $ do
  yieldSpell 1 $ do
  playSound SndIllusion
  grantInvisibility (HitMonster $ Grid.geKey ge) MajorInvisibility
  wait 8
  return cooldown
prepMonsterSpell _ _ = return Nothing -- FIXME

-------------------------------------------------------------------------------

-- | Perform the action with the given probability, otherwise return 'Nothing'.
ifRandom :: (FromAreaEffect f) => Double -> Script f (Maybe a)
         -> Script f (Maybe a)
ifRandom probability action = do
  number <- getRandomR 0 1
  ifSatisfies (probability > number) action

-- | If the predicate is 'True', perform the action, otherwise return
-- 'Nothing'.
ifSatisfies :: (Monad m) => Bool -> m (Maybe a) -> m (Maybe a)
ifSatisfies ok action = if ok then action else return Nothing

yieldSpell :: Int -> Script CombatEffect Int
           -> Script CombatEffect (Maybe (Int, Script CombatEffect Int))
yieldSpell impact action = return $ Just (impact, action)

-- | Out of all positions the monster could get to within three steps (leaving
-- the final action point for using the spell/ability), find the one that
-- maximizes the return value of the given function, returning both the
-- calculated value and the path that leads to the chosen position.
maximizePosition :: Grid.Key Monster -> (Position -> Script CombatEffect Int)
                 -> Script CombatEffect (Int, [Position])
maximizePosition key getImpact = do
  entry <- demandMonsterEntry key
  let startPos = rectTopleft $ Grid.geRect entry
  let impactify path = do
        impact <- getImpact (last (startPos : path))
        return (impact, path)
  directions <- randomPermutation allDirections
  isBlocked <- areaGet (arsIsBlockedForMonster entry)
  fmap (maximumBy (comparing fst)) $ mapM impactify $ ([] :) $
    takeWhile ((3 >=) . length) $ allPathsFrom directions isBlocked startPos

walkAlongPath :: Grid.Key Monster -> [Position] -> Script CombatEffect ()
walkAlongPath key path = forM_ path (walkMonster 4 key)

-- | Return 0 if the given position is unoccupied, 1 if it is occupied by a
-- foe, and -1 if it is occupied by a friend.
scoreForTarget :: Bool -> Position -> Script CombatEffect Int
scoreForTarget isAlly target = do
  mbOccupant <- areaGet (arsOccupant target)
  return $ case mbOccupant of
             Nothing -> 0
             Just (Left _) -> if isAlly then -1 else 1
             Just (Right entry) ->
               if monstIsAlly (Grid.geValue entry) == isAlly then -1 else 1

-- | Return a list of all foes that are at least partially within the given
-- rectangle.
foesInRect :: Bool -> PRect
           -> Script CombatEffect [Either CharacterNumber (Grid.Key Monster)]
foesInRect isAlly prect = do
  charNums <- if isAlly then return [] else do
    allCharNums <- getAllConsciousCharacters
    let isCharInRect charNum =
          rectContains prect <$> areaGet (arsCharacterPosition charNum)
    filterM isCharInRect allCharNums
  monsters <- do
    allMonsters <- areaGet (Grid.searchRect prect . arsMonsters)
    return $ map Grid.geKey $ filter ((isAlly ==) . monstIsAlly .
                                      Grid.geValue) allMonsters
  return (map Left charNums ++ map Right monsters)

maximizeFor :: (Monad m, Ord b) => [a] -> (a -> m b) -> m (a, b)
maximizeFor list fn = begin list where
  begin [] = error "maximizeFor: empty list"
  begin (first : rest) = do
    value <- fn first
    maximize first value rest
  maximize best value [] = return (best, value)
  maximize best value (a : as) = do
    value' <- fn a
    if value' > value then maximize a value' as else maximize best value as

-------------------------------------------------------------------------------
