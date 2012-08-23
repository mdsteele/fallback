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

{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}

module Fallback.Control.Script
  (Script, Result(..), execScript, mapEffect, emitEffect)
where

import Control.Applicative ((<$>), (<*>), Applicative, pure)

-------------------------------------------------------------------------------

-- | A 'Script' represents a computation that produces a series of effects
-- (represented by type @f@) and eventually a final value of type @a@.
newtype Script f a = Script { execScript :: Result f a }

-- | A 'Result' is the result of executing a script.  When executing a script
-- of type @Script f a@, the result will either by the final value of type @a@,
-- or else an effect of type @f b@ (that is, an effect that yields back a value
-- of type @b@), and a function that, given the value of type @b@, produces the
-- rest of the script to be run.
data Result :: (* -> *) -> * -> * where
  ResultFinal :: a -> Result f a
  ResultEffect :: f b -> (b -> Script f a) -> Result f a
  ResultFailure :: String -> Result f a

instance Functor (Script f) where
  fmap fn script = Script $
    case execScript script of
      ResultFinal value -> ResultFinal (fn value)
      ResultEffect eff sfn -> ResultEffect eff (fmap fn . sfn)
      ResultFailure message -> ResultFailure message

instance Applicative (Script f) where
  pure = Script . ResultFinal
  script1 <*> script2 = Script $
    case execScript script1 of
      ResultFinal value -> execScript (value <$> script2)
      ResultEffect eff sfn -> ResultEffect eff ((<*> script2) . sfn)
      ResultFailure message -> ResultFailure message

instance Monad (Script f) where
  return = Script . ResultFinal
  script >>= bindFn = Script $
    case execScript script of
      ResultFinal value -> execScript (bindFn value)
      ResultEffect eff sfn -> ResultEffect eff ((>>= bindFn) . sfn)
      ResultFailure message -> ResultFailure message
  fail = Script . ResultFailure

-------------------------------------------------------------------------------

-- | Map the effect type of a script.
mapEffect :: (forall b. f b -> g b) -> Script f a -> Script g a
mapEffect fn script = Script $
  case execScript script of
    ResultFinal value -> ResultFinal value
    ResultEffect eff sfn -> ResultEffect (fn eff) (mapEffect fn . sfn)
    ResultFailure message -> ResultFailure message

-- | Create a script that emits the given effect, and then simply returns the
-- value produced by the effect.
emitEffect :: f a -> Script f a
emitEffect eff = Script (ResultEffect eff return)

-------------------------------------------------------------------------------
