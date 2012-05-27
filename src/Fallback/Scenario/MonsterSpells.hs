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
  (prepMonsterSpell)
where

import Control.Applicative ((<$>))
import Control.Monad (filterM, forM, forM_)
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Pathfind (allPathsFrom)
import Fallback.State.Resources (SoundTag(..), StripTag(..))
import Fallback.State.Simple
import Fallback.State.Tags (MonsterSpellTag(..))
import Fallback.State.Terrain (positionCenter)
import Fallback.Utility (flip3, sumM)

-------------------------------------------------------------------------------

prepMonsterSpell :: MonsterSpellTag -> Grid.Entry Monster
                 -> Script CombatEffect (Maybe (Int, Script CombatEffect Int))
prepMonsterSpell CrossBeam ge = do
  let key = Grid.geKey ge
  let isAlly = monstIsAlly (Grid.geValue ge)
  (numTargets, path) <- maximizePosition key $ \pos -> do
    flip sumM [DirE, DirS, DirW, DirN] $ \dir -> do
      targets <- areaGet (flip3 arsBeamPositions pos (pos `plusDir` dir))
      sumM (scoreForTarget isAlly) targets
  ifSatisfies (numTargets >= 2) $ do
  ifRandom (fromIntegral numTargets * 0.25) $ do
  yieldSpell numTargets $ do
  walkAlongPath key path
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
  targets <- randomPermutation =<<
             filter (flip3 rangeTouchesRect (ofRadius maxRange) rect) <$>
             areaGet arsPartyPositions
  -- TODO only hit targets we can see
  let numTargets = length targets
  ifSatisfies (numTargets >= 2) $ do
  yieldSpell numTargets $ do
  monsterBeginOffensiveAction key (head targets)
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
prepMonsterSpell TeleportAway ge = do
  ifSatisfies False $ do -- FIXME
  numAdjacentFoes <- length <$> foesInRect (monstIsAlly $ Grid.geValue ge)
                                           (adjustRect1 (-1) $ Grid.geRect ge)
  ifRandom (fromIntegral numAdjacentFoes * 0.25) $ do
  -- FIXME pick destination far from foes
  yieldSpell numAdjacentFoes $ do
  -- FIXME perform teleport to destination
  return 5
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
    allMonsters <- areaGet (flip Grid.searchRect prect . arsMonsters)
    return $ map Grid.geKey $ filter ((isAlly ==) . monstIsAlly .
                                      Grid.geValue) allMonsters
  return (map Left charNums ++ map Right monsters)

-------------------------------------------------------------------------------
