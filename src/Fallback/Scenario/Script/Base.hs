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

{-# LANGUAGE GADTs, Rank2Types #-}

module Fallback.Scenario.Script.Base
  (-- * Basics
   FromAreaEffect(..), ToAreaEffect(..), emitAreaEffect, areaGet,
   -- * Control
   wait, alsoWith, also_, concurrent, concurrent_, concurrentAny,
   whenCombat, unlessCombat, whenDifficulty,
   -- * Query
   HitTarget(..), getHitTargetOccupant, lookupMonsterEntry, withMonsterEntry,
   getAllConsciousCharacters, getAllAllyMonsters, getAllEnemyMonsters,
   getAllAllyTargets,
   -- * Randomization
   randomBool, getRandomR, getRandomElem, randomPermutation,
   -- * Sound
   playSound, startMusic, stopMusic, fadeOutMusic,
   -- * Debugging
   debug)
where

import Control.Applicative ((<$>))
import Control.Monad (forM_, replicateM_, when)
import Data.Array (elems)
import qualified Data.Array.ST as STArray
import System.Random (Random)

import Fallback.Control.Script
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point (Position)
import Fallback.State.Area
import Fallback.State.Party
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.Utility (maybeM)

-------------------------------------------------------------------------------
-- Basics:

class FromAreaEffect f where
  fromAreaEffect :: AreaEffect a -> f a
  isWaitEffect :: f a -> (a -> Script f b) -> Maybe (Script f b)

instance FromAreaEffect AreaEffect where
  fromAreaEffect = id
  isWaitEffect EffWait fn = Just (fn ())
  isWaitEffect _ _ = Nothing

instance FromAreaEffect CombatEffect where
  fromAreaEffect = EffCombatArea
  isWaitEffect (EffCombatArea EffWait) fn = Just (fn ())
  isWaitEffect _ _ = Nothing

instance FromAreaEffect TownEffect where
  fromAreaEffect = EffTownArea
  isWaitEffect (EffTownArea EffWait) fn = Just (fn ())
  isWaitEffect _ _ = Nothing

class ToAreaEffect f where toAreaEffect :: f a -> AreaEffect a
instance ToAreaEffect PartyEffect where
  toAreaEffect = EffAreaCommon . EffAreaParty
instance ToAreaEffect AreaCommonEffect where toAreaEffect = EffAreaCommon
instance ToAreaEffect AreaEffect where toAreaEffect = id

emitAreaEffect :: (ToAreaEffect e, FromAreaEffect f) => e a -> Script f a
emitAreaEffect = emitEffect . fromAreaEffect . toAreaEffect

areaGet :: (FromAreaEffect f) => (forall s. (AreaState s) => s -> a)
        -> Script f a
areaGet = emitAreaEffect . EffAreaGet

-------------------------------------------------------------------------------
-- Control:

-- | Pause the script for the given number of frames.
wait :: (FromAreaEffect f) => Int -> Script f ()
wait n = replicateM_ n (emitAreaEffect EffWait)

-- | Run two scripts in parallel, combining their final results with the given
-- function.
alsoWith :: (FromAreaEffect f) => (a -> b -> c) -> Script f a -> Script f b
         -> Script f c
alsoWith oper script1 script2 =
  case execScript script1 of
    ResultFinal value1 -> fmap (value1 `oper`) script2
    ResultEffect eff1 sfn1 ->
      case isWaitEffect eff1 sfn1 of
        Nothing -> do
          value1 <- emitEffect eff1
          alsoWith oper (sfn1 value1) script2
        Just script1' ->
          case execScript script2 of
            ResultFinal value2 -> fmap (`oper` value2) continue1
            ResultEffect eff2 sfn2 ->
              case isWaitEffect eff2 sfn2 of
                Nothing -> do
                  value2 <- emitEffect eff2
                  alsoWith oper continue1 (sfn2 value2)
                Just script2' -> do
                  emitAreaEffect EffWait
                  alsoWith oper script1' script2'
          where continue1 = emitAreaEffect EffWait >> script1'

-- | Run two scripts in parallel.
also_ :: (FromAreaEffect f) => Script f () -> Script f () -> Script f ()
also_ = alsoWith const

-- | Map the script function over the given list, running all the scripts in
-- parallel and returning the list of the final results.
concurrent :: (FromAreaEffect f) => [a] -> (a -> Script f b) -> Script f [b]
concurrent list fn = foldr (alsoWith (:)) (return []) $ map fn list

-- | Map the script function over the given list, running all the scripts in
-- parallel.
concurrent_ :: (FromAreaEffect f) => [a] -> (a -> Script f ()) -> Script f ()
concurrent_ list fn = foldr also_ (return ()) $ map fn list

-- | Map the script function over the given list, running all the scripts in
-- parallel and returning 'True' if any of the scripts returned 'True'.
concurrentAny :: (FromAreaEffect f) => [a] -> (a -> Script f Bool)
              -> Script f Bool
concurrentAny list fn = foldr (alsoWith (||)) (return False) $ map fn list

-- | Run the given script if we are in town mode, otherwise do nothing.
unlessCombat :: (FromAreaEffect f) => Script TownEffect () -> Script f ()
unlessCombat = emitAreaEffect . EffIfCombat (return ())

-- | Run the given script if we are in combat mode, otherwise do nothing.
whenCombat :: (FromAreaEffect f) => Script CombatEffect () -> Script f ()
whenCombat = emitAreaEffect . flip EffIfCombat (return ())

whenDifficulty :: (FromAreaEffect f) => (Difficulty -> Bool) -> Script f ()
               -> Script f ()
whenDifficulty fn script = do
  difficulty <- areaGet (partyDifficulty . arsParty)
  when (fn difficulty) script

-------------------------------------------------------------------------------
-- Query:

data HitTarget = HitCharacter CharacterNumber
               | HitMonster (Grid.Key Monster)
               | HitPosition Position

-- | Determine the character or monster at the given 'HitTarget', if any.
getHitTargetOccupant :: (FromAreaEffect f) => HitTarget
                     -> Script f (Maybe (Either CharacterNumber
                                                (Grid.Entry Monster)))
getHitTargetOccupant (HitCharacter charNum) = return $ Just $ Left charNum
getHitTargetOccupant (HitMonster monstKey) =
  fmap Right <$> lookupMonsterEntry monstKey
getHitTargetOccupant (HitPosition pos) = areaGet (arsOccupant pos)

-- | Attempt to retrieve the entry for a given monster key.
lookupMonsterEntry :: (FromAreaEffect f) => Grid.Key Monster
                   -> Script f (Maybe (Grid.Entry Monster))
lookupMonsterEntry key = areaGet (Grid.lookup key . arsMonsters)

withMonsterEntry :: (FromAreaEffect f) => Grid.Key Monster
                 -> (Grid.Entry Monster -> Script f ()) -> Script f ()
withMonsterEntry key action = do
  mbEntry <- lookupMonsterEntry key
  maybeM mbEntry action

-- | Return a list of all character numbers for characters who are conscious
-- (have non-zero health), whether or not they are under a mental effect.
getAllConsciousCharacters :: (FromAreaEffect f) => Script f [CharacterNumber]
getAllConsciousCharacters = do
  party <- areaGet arsParty
  return $ filter (chrIsConscious . partyGetCharacter party)
                  [minBound .. maxBound]

-- | Return a list of all ally monster grid entries.  In combat mode, this will
-- only include monsters within the combat arena.
getAllAllyMonsters :: (FromAreaEffect f) => Script f [Grid.Entry Monster]
getAllAllyMonsters = do
  monsters <- areaGet arsMonsters
  return $ filter (monstIsAlly . Grid.geValue) $ Grid.entries monsters

-- | Return a list of all enemy (non-ally) monster grid entries.  In combat
-- mode, this will only include monsters within the combat arena.
getAllEnemyMonsters :: (FromAreaEffect f) => Script f [Grid.Entry Monster]
getAllEnemyMonsters = do
  monsters <- areaGet arsMonsters
  return $ filter (not . monstIsAlly . Grid.geValue) $ Grid.entries monsters

-- | Get the 'HitTarget' for each conscious party member and each living ally
-- monster (in no particular order).
getAllAllyTargets :: (FromAreaEffect f) => Script f [HitTarget]
getAllAllyTargets = do
  chars <- map HitCharacter <$> getAllConsciousCharacters
  allies <- map (HitMonster . Grid.geKey) <$> getAllAllyMonsters
  return (chars ++ allies)

-------------------------------------------------------------------------------
-- Randomization:

-- | Return 'True' with the given probability.
randomBool :: (FromAreaEffect f) => Double -> Script f Bool
randomBool probTrue = (probTrue >) <$> getRandomR 0 1

-- | Choose a random element from a non-empty list.
getRandomElem :: (FromAreaEffect f) => [a] -> Script f a
getRandomElem list = if null list then fail "getRandomElem: empty list"
                     else (list !!) <$> getRandomR 0 (length list - 1)

-- | Choose a random value within the given range.  @'getRandomR' a b@ is
-- the 'Script' monad equivalent of @'randomRIO' (a, b)@.
getRandomR :: (FromAreaEffect f, Random a) => a -> a -> Script f a
getRandomR lo hi = emitAreaEffect $ EffRandom lo hi

-- | Return a random permutation of the given list.
randomPermutation :: (FromAreaEffect f) => [a] -> Script f [a]
randomPermutation list = do
  let size = length list
  pairs <- mapM (\i -> (,) i <$> getRandomR 0 i) [size - 1, size - 2 .. 1]
  return $ elems $ STArray.runSTArray $ do
    arr <- STArray.newListArray (0, size - 1) list
    forM_ pairs $ \(i, j) -> when (i /= j) $ do
      x <- STArray.readArray arr i
      STArray.writeArray arr i =<< STArray.readArray arr j
      STArray.writeArray arr j x
    return arr

-------------------------------------------------------------------------------
-- Sound:

playSound :: (FromAreaEffect f) => SoundTag -> Script f ()
playSound tag = do
  resources <- areaGet arsResources
  emitAreaEffect $ EffPlaySound $ rsrcSound resources tag

-- | Start new music looping.  If any music is already playing, stop it.
startMusic :: (FromAreaEffect f) => MusicTag -> Script f ()
startMusic = emitAreaEffect . EffMusicStart

-- | If any music is currently playing, stop it immediately.
stopMusic :: (FromAreaEffect f) => Script f ()
stopMusic = emitAreaEffect $ EffMusicStop

-- | If any music is currently playing, fade it out over the given number of
-- seconds.
fadeOutMusic :: (FromAreaEffect f) => Double -> Script f ()
fadeOutMusic = emitAreaEffect . EffMusicFadeOut

-------------------------------------------------------------------------------
-- Debugging:

-- | Print a string to the terminal console, for debugging.
debug :: (FromAreaEffect f) => String -> Script f ()
debug = emitAreaEffect . EffDebug

-------------------------------------------------------------------------------
