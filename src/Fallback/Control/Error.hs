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

module Fallback.Control.Error
  (-- * The @EO@ monad
   EO, runEO,
   -- * The @IOEO@ monad
   IOEO, runIOEO, onlyIO, onlyEO)
where

import Control.Applicative ((<$>), Applicative(..), liftA)
import Control.Monad.Fix (MonadFix, mfix)

-------------------------------------------------------------------------------

-- | An object of type 'EO' @a@ (for \"Errors Or @a@\") represents a
--   computation that will produce either a value of type @a@ or a (non-empty)
--   list of error messages.  It is a convenient way to represent computations
--   that may fail, possibly in multiple ways at once.
newtype EO a = EO { runEO :: Either [String] a }

instance Functor EO where
  fmap = liftA

instance Applicative EO where
  pure = EO . Right
  eo1 <*> eo2 =
    case runEO eo1 of
      Left e1 -> case runEO eo2 of
                   Left e2 -> EO $ Left $ e1 ++ e2
                   Right _ -> EO $ Left e1
      Right f -> case runEO eo2 of
                   Left e2 -> EO $ Left e2
                   Right x -> pure (f x)

instance Monad EO where
  return = pure
  eo >>= f = case runEO eo of Left errs -> EO (Left errs)
                              Right x -> f x
  fail = EO . Left . (:[])

instance MonadFix EO where
  mfix fn = eo where eo = fn result
                     result = case runEO eo of
                                Left _ -> error "mfix EO"
                                Right value -> value

-------------------------------------------------------------------------------

-- | A monad combining the 'IO' and 'EO' monads.  Note that 'fail' in the
--   'IOEO' monad is equivalent to @onlyEO . fail@ (that is, it creates an 'EO'
--   error rather than crashing the program).
newtype IOEO a = IOEO { runIOEO :: IO (EO a) }

instance Functor IOEO where
  fmap = liftA

instance Applicative IOEO where
  pure = IOEO . pure . pure
  (IOEO ioeo1) <*> (IOEO ioeo2) = IOEO $ (<*>) <$> ioeo1 <*> ioeo2

instance Monad IOEO where
  return = pure
  (IOEO ioeo) >>= fn = IOEO $ do
    eo <- ioeo
    case runEO eo of
      Left e -> return $ EO $ Left e
      Right x -> runIOEO $ fn x
  fail = IOEO . return . fail

instance MonadFix IOEO where
  mfix fn = let fn' eo = runIOEO $ fn $
                         case runEO eo of
                           Left _ -> error "mfix IOEO"
                           Right value -> value
            in IOEO $ mfix fn'

-- | Run an arbitrary 'IO' action in the 'IOEO' monad.
onlyIO :: IO a -> IOEO a
onlyIO = IOEO . fmap pure

-- | Run an arbitrary 'EO' action in the 'IOEO' monad.
onlyEO :: EO a -> IOEO a
onlyEO = IOEO . return

-------------------------------------------------------------------------------
