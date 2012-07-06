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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fallback.Control.Parse
  (Parser, tryParse, parseRead, readPrecParser,
   meshKeyVal, meshKeyDefault, weaveBracesCommas,
   showKeyVal, showBracesCommas)
where

import Control.Applicative
import Control.Monad (ap, mzero)
import Data.List (intersperse)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, readPrec_to_P)
import Text.Read (readPrec)

import Fallback.Control.Mesh (Mesh, meshDefault, meshOnce, weaveSep)

-------------------------------------------------------------------------------

newtype Parser a = Parser { fromParser :: ReadP.ReadP a }
  deriving (Functor, Monad)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = Parser mzero
  Parser r1 <|> Parser r2 = Parser (r1 ReadP.<++ r2)

-------------------------------------------------------------------------------

tryParse :: Parser a -> String -> Maybe a
tryParse (Parser r) str =
  case ReadP.readP_to_S r str of
    [(value, "")] -> Just value
    _ -> Nothing

parseRead :: (Read a) => Parser a
parseRead = Parser (readPrec_to_P readPrec 11)

readPrecParser :: Parser a -> ReadPrec a
readPrecParser (Parser r) = lift r

-------------------------------------------------------------------------------

parseKeyVal :: (Read a) => String -> Parser a
parseKeyVal key =
  Parser (ReadP.string key >> ReadP.char '=' >> fromParser parseRead)

meshKeyVal :: (Read a) => String -> Mesh Parser a
meshKeyVal key = meshOnce $ parseKeyVal key

meshKeyDefault :: (Read a) => String -> a -> Mesh Parser a
meshKeyDefault key def = meshDefault def $ parseKeyVal key

weaveBracesCommas :: Mesh Parser a -> Parser a
weaveBracesCommas mesh =
  Parser $ ReadP.between (ReadP.char '{') (ReadP.char '}') $ fromParser $
  weaveSep (Parser (ReadP.char ',')) mesh

-------------------------------------------------------------------------------

showKeyVal :: (Show a) => String -> a -> ShowS
showKeyVal key a = showString key . showChar '=' . showsPrec 11 a

showBracesCommas :: [ShowS] -> ShowS
showBracesCommas ss =
  showChar '{' . foldr (.) id (intersperse (showChar ',') ss) . showChar '}'

-------------------------------------------------------------------------------
