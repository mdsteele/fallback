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
   showKeyVal, showKeyList, showBracesCommas)
where

import Control.Applicative
import Control.Monad (ap, mzero, void)
import Data.List (intersperse, unfoldr)
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
parseKeyVal key = Parser $ do
  void $ token $ ReadP.string key
  charToken '='
  token $ fromParser parseRead

meshKeyVal :: (Read a) => String -> Mesh Parser a
meshKeyVal key = meshOnce $ parseKeyVal key

meshKeyDefault :: (Read a) => String -> a -> Mesh Parser a
meshKeyDefault key def = meshDefault def $ parseKeyVal key

weaveBracesCommas :: Mesh Parser a -> Parser a
weaveBracesCommas mesh =
  Parser $ ReadP.between (charToken '{') (charToken '}') $ token $ fromParser $
  weaveSep (Parser (charToken ',')) mesh

-------------------------------------------------------------------------------

showKeyVal :: (Show a) => String -> a -> ShowS
showKeyVal key a = showString key . showChar '=' . showsPrec 11 a

showKeyList :: (Show a) => String -> Int -> [a] -> ShowS
showKeyList key n as = showString key . showString "=[" .
  (intercompose (showString ",\n") $ map (intercompose (showChar ',')) $
   groupsOf n $ map shows as) . showChar ']'

showBracesCommas :: [ShowS] -> ShowS
showBracesCommas ss =
  showChar '{' . intercompose (showString ",\n") ss . showString "}\n"

-------------------------------------------------------------------------------
-- Private:

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = unfoldr $ \as -> if null as then Nothing else Just (splitAt n as)

intercompose :: ShowS -> [ShowS] -> ShowS
intercompose sep = foldr (.) id . intersperse sep

token :: ReadP.ReadP a -> ReadP.ReadP a
token p = do { a <- p; ReadP.skipSpaces; return a }

charToken :: Char -> ReadP.ReadP ()
charToken = void . token . ReadP.char

-------------------------------------------------------------------------------
