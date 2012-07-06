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

module Fallback.Test.Parse (parseTests) where

import Control.Applicative ((<$>), (<*>))
import Test.HUnit ((~:), Test(TestList))
import Text.Read (readPrec)

import Fallback.Control.Parse
import Fallback.Test.Base (insist)

-------------------------------------------------------------------------------

parseTests :: Test
parseTests = "parse" ~: TestList [
  insist $ tryParse parser1 "garbage!" == Nothing,
  insist $ tryParse parser1 "{bool=True,int=7}" ==
           Just (True, 3.0, 7, "foobar"),
  insist $ tryParse parser1 "{double=2.5,int=2,bool=False,string=\"hi\"}" ==
           Just (False, 2.5, 2, "hi"),
  insist $ showBracesCommas [showKeyVal "b" True, showKeyVal "str" "foo"] "" ==
           "{b=True,str=\"foo\"}",
  insist $ reads "{c=3,a=1}" == [(Foo 1 2 3, "")],
  insist $ null $ (reads :: ReadS Foo) "{a=1}",
  insist $ reads "{c=3,a=1}blarg" == [(Foo 1 2 3, "blarg")],
  insist $ reads "(False,{a=6,c=8,b=5},True)" ==
           [((False, Foo 6 5 8, True), "")]]

-------------------------------------------------------------------------------

parser1 :: Parser (Bool, Double, Int, String)
parser1 = weaveBracesCommas $
  (,,,) <$> meshKeyVal "bool"
        <*> meshKeyDefault "double" 3.0
        <*> meshKeyVal "int"
        <*> meshKeyDefault "string" "foobar"

data Foo = Foo Int Int Int
  deriving (Eq)

instance Read Foo where
  readPrec = readPrecParser $ weaveBracesCommas $
    Foo <$> meshKeyVal "a"
        <*> meshKeyDefault "b" 2
        <*> meshKeyVal "c"

-------------------------------------------------------------------------------
