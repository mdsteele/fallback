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
   -- * Vars
   VarSeed, HasVarSeeds(..), splitVarSeed,
   Var, makeVar,
   DeviceId, makeDeviceId,
   MonsterScriptId, makeMonsterScriptId,
   TriggerId, makeTriggerId,
   VarType(..))
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------

-- TODO: Use SparseMaps here.
data Progress = Progress
  { progBoolVars :: Map.Map (Var Bool) Bool,
    progIntVars :: Map.Map (Var Int) Int }
  deriving (Read, Show)

emptyProgress :: Progress
emptyProgress = Progress
  { progBoolVars = Map.empty,
    progIntVars = Map.empty }

class HasProgress a where
  getProgress :: a -> Progress

-------------------------------------------------------------------------------

data VarSeed = VarSeed Int Int
  deriving (Eq, Ord, Read, Show)

instance Num VarSeed where
  fromInteger n = VarSeed (fromInteger n) 1
  (+) = error "VarSeed.+"
  (-) = error "VarSeed.-"
  (*) = error "VarSeed.*"
  abs = error "VarSeed.abs"
  negate = error "VarSeed.negate"
  signum = error "VarSeed.signum"

class (Monad m) => HasVarSeeds m where
  useVarSeed :: VarSeed -> m ()

splitVarSeed :: (HasVarSeeds m) => VarSeed -> m (VarSeed, VarSeed)
splitVarSeed vseed@(VarSeed n m) = do
  useVarSeed vseed
  return (VarSeed n (2 * m), VarSeed n (2 * m + 1))

-------------------------------------------------------------------------------

newtype Var a = Var VarSeed
  deriving (Eq, Ord, Read, Show)

makeVar :: VarSeed -> Var a
makeVar = Var

newtype DeviceId = DeviceId VarSeed
  deriving (Eq, Ord, Read, Show)

makeDeviceId :: VarSeed -> DeviceId
makeDeviceId = DeviceId

newtype MonsterScriptId = MonsterScriptId VarSeed
  deriving (Eq, Ord, Read, Show)

makeMonsterScriptId :: VarSeed -> MonsterScriptId
makeMonsterScriptId = MonsterScriptId

newtype TriggerId = TriggerId VarSeed
  deriving (Eq, Ord, Read, Show)

makeTriggerId :: VarSeed -> TriggerId
makeTriggerId = TriggerId

class VarType a where
  progressGetVar :: Var a -> Progress -> a
  progressSetVar :: Var a -> a -> Progress -> Progress
  progressAddVar :: Var a -> a -> Progress -> Maybe Progress

instance VarType Bool where
  progressGetVar = getVar progBoolVars
  progressSetVar vid value prog =
    prog { progBoolVars = Map.insert vid value (progBoolVars prog) }
  progressAddVar vid value prog =
    if Map.member vid (progBoolVars prog) then Nothing else
      Just (progressSetVar vid value prog)

instance VarType Int where
  progressGetVar = getVar progIntVars
  progressSetVar vid value prog =
    prog { progIntVars = Map.insert vid value (progIntVars prog) }
  progressAddVar vid value prog =
    if Map.member vid (progIntVars prog) then Nothing else
      Just (progressSetVar vid value prog)

-------------------------------------------------------------------------------
-- Private functions:

getVar :: (Progress -> Map.Map (Var a) a) -> Var a -> Progress -> a
getVar fn vid prog =
  fromMaybe (error $ "Var lookup failure: " ++ show vid) $
  Map.lookup vid $ fn prog

-------------------------------------------------------------------------------
