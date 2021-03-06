{- ============================================================================
| Copyright 2010 Matthew D. Steele <mdsteele@alum.mit.edu>                    |
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

{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnboxedTuples #-}

module Fallback.Utility
  (-- * Numeric functions
   ceilDiv, fmod, hypot, isFinite, square, squash,
   -- * List functions
   mapEither, forEither, firstJust,
   groupKey, minimumKey, maximumKey, nubKey, sortKey,
   -- * Function combinators
   flip3, flip4,
   -- * Monadic functions
   anyM, forMaybeM, maybeM, sumM, unfoldM, untilM, whenM,
   -- * IO
   delayFinalizers)
where

import Control.Arrow ((&&&))
import Control.Monad (foldM, liftM, msum, when)
import Data.Function (on)
import Data.List (groupBy, maximumBy, minimumBy, nubBy, sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Foreign.C.Types (CDouble(CDouble))
import qualified GHC.IO
import qualified GHC.Prim

-------------------------------------------------------------------------------
-- Numeric functions:

infixl 7 `ceilDiv`
-- | Perform ceiling division of two integral values: @10 `ceilDiv` 5 == 2@ and
-- @11 `ceilDiv` 5 == 3@.  The divisor must be positive.
ceilDiv :: (Integral a) => a -> a -> a
ceilDiv a b = (a + b - 1) `div` b

infix 7 `fmod`
fmod :: Double {-^dividend-} -> Double {-^modulus-} -> Double
fmod d m = d - m * fromIntegral (floor (d / m) :: Int)

-- | Compute @sqrt(x*x + y*y)@ without overflow\/underflow.
hypot :: Double -> Double -> Double
hypot x y = realToFrac $ c_hypot (realToFrac x) (realToFrac y)
foreign import ccall unsafe "math.h hypot" c_hypot ::
  CDouble -> CDouble -> CDouble

-- | Return 'True' if the number is non-infinite and non-NaN, 'False'
-- otherwise.
isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)

-- -- | Add or subtract a multiple of 2pi from the given number so that the result
-- --   is between -pi and pi.
-- mod2pi :: Double -> Double
-- mod2pi x = assert (isFinite x) (fmod (x + pi) (2 * pi) - pi)

square :: (Num a) => a -> a
square x = x * x

-- | \"Squash\" a value so that it falls between -@limit@ and +@limit@.  When
-- the input value is near zero, the curve is near-linear and the return value
-- will be approximately equal to the input value; as the magnitude of the
-- input value grows larger, the return value will taper off smoothly so that
-- its magnitude never exceeds the limit.  The limit must be positive.
squash :: Double {-^limit-} -> Double {-^value-} -> Double
squash limit value = 2 * limit / (1 + exp (-2 * value / limit)) - limit

-------------------------------------------------------------------------------
-- List functions:

-- | Map values and separate the 'Left' and 'Right' results.
mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither _ [] = ([], [])
mapEither fn (x : xs) =
  let (bs, cs) = mapEither fn xs
  in case fn x of Left b -> (b : bs, cs)
                  Right c -> (bs, c : cs)

-- | 'forEither' is 'mapEither' with its arguments flipped.
forEither :: [a] -> (a -> Either b c) -> ([b], [c])
forEither = flip mapEither

-- | Apply the given function to items in the list, returning the first @Just@
-- result, or @Nothing@ if all items return @Nothing@.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust = (msum .) . map

-- | Apply the given function to all items in the list, and group the items in
-- the list by comparing the results of the function.
groupKey :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupKey fn = map (map snd) . groupBy ((==) `on` fst) . map (fn &&& id)

-- | Apply the given function to all items in the list, and return the list
-- item that yields the minimum result.
minimumKey :: (Ord b) => (a -> b) -> [a] -> a
minimumKey fn = snd . minimumBy (comparing fst) . map (fn &&& id)

-- | Apply the given function to all items in the list, and return the list
-- item that yields the maximum result.
maximumKey :: (Ord b) => (a -> b) -> [a] -> a
maximumKey fn = snd . maximumBy (comparing fst) . map (fn &&& id)

-- | Apply the given function to all items in the list, and uniquify the items
-- in the list by comparing the results of the function.
nubKey :: (Eq b) => (a -> b) -> [a] -> [a]
nubKey fn = map snd . nubBy ((==) `on` fst) . map (fn &&& id)

-- | Apply the given function to all items in the list, and sort the items in
-- the list by comparing the results of the function.
sortKey :: (Ord b) => (a -> b) -> [a] -> [a]
sortKey fn = map snd . sortBy (comparing fst) . map (fn &&& id)

-------------------------------------------------------------------------------
-- Function combinators:

-- | Like 'flip', but moves the first argument to the third position instead of
-- | to the second position.
flip3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip3 f b c a = f a b c

-- | Like 'flip', but moves the first argument to the fourth position instead
-- | of to the second position.
flip4 :: (a -> b -> c -> d -> e) -> b -> c -> d -> a -> e
flip4 f b c d a = f a b c d

-------------------------------------------------------------------------------
-- Monadic functions:

-- | Perform the monadic actions one at a time; as soon as one returns 'True',
-- return 'True' without executing the rest.  If all of the actions return
-- 'False', then return 'False'.
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM fn (x : xs) = do
  bool <- fn x
  if bool then return True else anyM fn xs

-- | A combination of 'forM' and 'mapMaybe'.
forMaybeM :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM xs fn = liftM catMaybes $ mapM fn xs

-- | Performs an action on the value if present, otherwise does nothing.
maybeM :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
maybeM = flip $ maybe $ return ()

-- | Equivalent to (but more efficient than) @((liftM sum .) . mapM)@.
sumM :: (Monad m, Num b) => (a -> m b) -> [a] -> m b
sumM fn = foldM fn' 0 where
  fn' total item = do { value <- fn item; return $! total + value }

-- | Monadic version of @unfoldr@.
unfoldM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM fn start = do
  mbNext <- fn start
  case mbNext of
    Nothing -> return []
    Just (a, b) -> do { as <- unfoldM fn b; return (a : as) }

untilM :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
untilM value done update action =
  if done value then return ()
  else action value >> untilM (update value) done update action

-- | Like 'when', but allows the predicate to be a monadic action.
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predicate action = do
  bool <- predicate
  when bool action

-------------------------------------------------------------------------------
-- IO functions:

-- | Perform an IO action, and ensure that System.Mem.Weak finalizers do not
-- run on the given object before the action has completed.
delayFinalizers :: a -> IO b -> IO b
delayFinalizers value action = do
  result <- action
  touch value
  return result

-- | Force an object not to have been garbage collected before this point in
-- the computation.
touch :: a -> IO ()
touch x =  -- I _think_ this works, but I'm not totally sure.
  GHC.IO.IO (\s -> case GHC.Prim.touch# x s of { s' -> (# s', () #) })

-------------------------------------------------------------------------------
