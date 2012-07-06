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

-- | The 'Mesh' type allows one to build parsers (or other failable monads)
-- that parse subcomponents in arbitrary order.  The usual use case looks like:
--
-- > data D = Cons A [B] (Maybe C)
-- > parserD = weave $ Cons <$> meshOnce parserA
-- >                        <*> meshMany parserB
-- >                        <*> meshMaybe parserC
--
-- The above @parserD@ would parse a single A, zero or more B's, and an
-- optional C, with the A's, B's, and C's appearing in any order relative to
-- each other.
--
-- This module is essentially a slightly modified version of the @InterleaveT@
-- transformer presented in \"The InterleaveT Abstraction: Alternative with
-- Flexible Ordering\" by Neil Brown, found in issue 17 of The Monad Reader
-- (<http://themonadreader.files.wordpress.com/2011/01/issue17.pdf>).  Much
-- thanks to Neil for his excellent article.

module Fallback.Control.Mesh
  (Mesh, weave, weaveSep,
   meshOnce, meshMaybe, meshDefault, meshMany, meshMany1, meshSkipMany)
where

import Control.Applicative

-------------------------------------------------------------------------------

data Mesh m a = MeshStop a
              | MeshContinue !(Maybe a) (m (Mesh m a))

instance (Functor m) => Functor (Mesh m) where
  fmap f (MeshStop a) = MeshStop (f a)
  fmap f (MeshContinue ma p) = MeshContinue (fmap f ma) (fmap (fmap f) p)

instance (Alternative m) => Applicative (Mesh m) where
  pure = MeshStop
  ia@(MeshContinue ma pa) <*> ib@(MeshContinue mb pb) =
    MeshContinue (ma <*> mb) (((<*> ib) <$> pa) <|> ((ia <*>) <$> pb))
  MeshStop a <*> ib = a <$> ib
  ia <*> MeshStop b = ($ b) <$> ia

-------------------------------------------------------------------------------

-- | Turn a 'Mesh' back into a plain monad.
weave :: (Alternative m, Monad m) => Mesh m a -> m a
weave (MeshStop a) = return a
weave (MeshContinue Nothing p) = p >>= weave
weave (MeshContinue (Just a) p) = (p >>= weave) <|> return a

weaveSep :: (Alternative m, Monad m) => m b -> Mesh m a -> m a
weaveSep q i =
  case i of
    MeshStop a -> return a
    MeshContinue Nothing p -> p >>= weave'
    MeshContinue (Just a) p -> (p >>= weave') <|> return a
  where
    weave' (MeshStop a) = return a
    weave' (MeshContinue Nothing p) = q >> p >>= weave'
    weave' (MeshContinue (Just a) p) = (q >> p >>= weave') <|> return a

-------------------------------------------------------------------------------

-- | A parser that must succeed exactly once.
meshOnce :: (Functor m) => m a -> Mesh m a
meshOnce p = MeshContinue Nothing (fmap MeshStop p)

-- | A parser that must succeed at most once.
meshMaybe :: (Functor m) => m a -> Mesh m (Maybe a)
meshMaybe p = MeshContinue (Just Nothing) (fmap (MeshStop . Just) p)

-- | A parser that must succeed at most once, and which uses a default value if
-- it never succeeds.
meshDefault :: (Functor m) => a -> m a -> Mesh m a
meshDefault d p = MeshContinue (Just d) (fmap MeshStop p)

-- | A parser that can succeed any number of times (including zero).
meshMany :: (Functor m, Monad m) => m a -> Mesh m [a]
meshMany p = MeshContinue (Just []) $ do
  a <- p
  return $ fmap (a :) $ meshMany p

-- | A parser that must succeed at least once.
meshMany1 :: (Functor m, Monad m) => m a -> Mesh m [a]
meshMany1 p = MeshContinue Nothing $ do
  a <- p
  return $ fmap (a :) $ meshMany p

-- | A parser than can succeed any number of times, and the results be ignored.
meshSkipMany :: (Monad m) => m a -> Mesh m ()
meshSkipMany p = MeshContinue (Just ()) $ p >> return (meshSkipMany p)

-------------------------------------------------------------------------------
