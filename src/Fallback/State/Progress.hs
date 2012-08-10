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

module Fallback.State.Progress
  (-- * Progress
   Progress, emptyProgress, HasProgress(..),
   -- * VarSeeds
   VarSeed, HasVarSeeds(..), splitVarSeed, splitVarSeed3, splitVarSeed4,
   -- * Vars
   Var, newVar, VarType(..),
   -- * Other IDs
   DeviceId, newDeviceId,
   MonsterScriptId, newMonsterScriptId,
   TriggerId, newTriggerId)
where

import qualified Fallback.Data.Grid as Grid
import qualified Fallback.Data.SparseMap as SM

-------------------------------------------------------------------------------
-- Progress:

data Progress = Progress
  { progBoolVars :: SM.SparseMap (Var Bool) Bool,
    progIntVars :: SM.SparseMap (Var Int) Int,
    progKeyVars :: SM.SparseMap (Var (Grid.Key ())) (Grid.Key ()) }
  deriving (Read, Show)

emptyProgress :: Progress
emptyProgress = Progress
  { progBoolVars = SM.make varDefaultValue,
    progIntVars = SM.make varDefaultValue,
    progKeyVars = SM.make varDefaultValue }

class HasProgress a where
  getProgress :: a -> Progress

-------------------------------------------------------------------------------
-- VarSeeds:

-- | A seed used to create 'Var's and other kinds of unique IDs.  Each
-- 'VarSeed' must only be used once.  They can be created either from an
-- integer using 'fromInteger' (typically implicitly, using a integer literal),
-- or from another 'VarSeed' using 'splitVarSeed'.
data VarSeed = VarSeed Int Int
  deriving (Eq, Ord, Read, Show)

-- | Only 'fromInteger' is valid; all other methods raise an error.
instance Num VarSeed where
  fromInteger n = VarSeed (fromInteger n) 1
  (+) = error "VarSeed.+"
  (-) = error "VarSeed.-"
  (*) = error "VarSeed.*"
  abs = error "VarSeed.abs"
  negate = error "VarSeed.negate"
  signum = error "VarSeed.signum"

-- | Represents a monad that tracks which 'VarSeed's have been used, and
-- prevents the same one from being used twice.
class (Monad m) => HasVarSeeds m where
  -- | Use up the given 'VarSeed'.  If the 'VarSeed' has been used before, this
  -- should signal some kind of error in the monad; otherwise, it should
  -- succeed and remember that this 'VarSeed' has now been used.
  useVarSeed :: VarSeed -> m ()

-- | Use up the given 'VarSeed', and generate two fresh, unused ones.
splitVarSeed :: (HasVarSeeds m) => VarSeed -> m (VarSeed, VarSeed)
splitVarSeed vseed@(VarSeed n m) = do
  useVarSeed vseed
  return (VarSeed n (2 * m), VarSeed n (2 * m + 1))

splitVarSeed3 :: (HasVarSeeds m) => VarSeed -> m (VarSeed, VarSeed, VarSeed)
splitVarSeed3 vseed = do
  (vseed', vseed1) <- splitVarSeed vseed
  (vseed2, vseed3) <- splitVarSeed vseed'
  return (vseed1, vseed2, vseed3)

splitVarSeed4 :: (HasVarSeeds m) => VarSeed
              -> m (VarSeed, VarSeed, VarSeed, VarSeed)
splitVarSeed4 vseed = do
  (vseed', vseed'') <- splitVarSeed vseed
  (vseed1, vseed2) <- splitVarSeed vseed'
  (vseed3, vseed4) <- splitVarSeed vseed''
  return (vseed1, vseed2, vseed3, vseed4)

-------------------------------------------------------------------------------
-- Vars:

-- | Represents a storage key for a value in a 'Progress'.  Note that in order
-- to actually be stored in a 'Progress', the value type must be an instance
-- of 'VarType'.
newtype Var a = Var { fromVar :: VarSeed }
  deriving (Eq, Ord, Read, Show)

-- | Create a new 'Var' (of any desired type) from the given 'VarSeed'.  This
-- uses up the 'VarSeed'.
newVar :: (HasVarSeeds m) => VarSeed -> m (Var a)
newVar vseed = do
  useVarSeed vseed
  return (Var vseed)

-- | Represents a value that can be stored in a 'Progress'.
class VarType a where
  varDefaultValue :: a
  progressGetVar :: Var a -> Progress -> a
  progressSetVar :: Var a -> a -> Progress -> Progress

instance VarType Bool where
  varDefaultValue = False
  progressGetVar = getVar progBoolVars
  progressSetVar var value prog =
    prog { progBoolVars = SM.set var value (progBoolVars prog) }

instance VarType Int where
  varDefaultValue = 0
  progressGetVar = getVar progIntVars
  progressSetVar var value prog =
    prog { progIntVars = SM.set var value (progIntVars prog) }

instance VarType (Grid.Key a) where
  varDefaultValue = Grid.nilKey
  progressGetVar var = Grid.coerceKey . getVar progKeyVars (coerceVar var)
  progressSetVar var value prog =
    prog { progKeyVars = SM.set (coerceVar var) (Grid.coerceKey value)
                                (progKeyVars prog) }

-------------------------------------------------------------------------------
-- Other IDs:

newtype DeviceId = DeviceId VarSeed
  deriving (Eq, Ord, Read, Show)

-- | Create a new 'DeviceId' from the given 'VarSeed'.  This uses up the
-- 'VarSeed'.
newDeviceId :: (HasVarSeeds m) => VarSeed -> m DeviceId
newDeviceId vseed = do
  useVarSeed vseed
  return (DeviceId vseed)

newtype MonsterScriptId = MonsterScriptId VarSeed
  deriving (Eq, Ord, Read, Show)

-- | Create a new 'MonsterScriptId' from the given 'VarSeed'.  This uses up the
-- 'VarSeed'.
newMonsterScriptId :: (HasVarSeeds m) => VarSeed -> m MonsterScriptId
newMonsterScriptId vseed = do
  useVarSeed vseed
  return (MonsterScriptId vseed)

newtype TriggerId = TriggerId VarSeed
  deriving (Eq, Ord, Read, Show)

-- | Create a new 'TriggerId' from the given 'VarSeed'.  This uses up the
-- 'VarSeed'.
newTriggerId :: (HasVarSeeds m) => VarSeed -> m TriggerId
newTriggerId vseed = do
  useVarSeed vseed
  return (TriggerId vseed)

-------------------------------------------------------------------------------
-- Private functions:

coerceVar :: Var a -> Var b
coerceVar = Var . fromVar

getVar :: (Progress -> SM.SparseMap (Var a) a) -> Var a -> Progress -> a
getVar fn vid prog = SM.get vid $ fn prog

-------------------------------------------------------------------------------
