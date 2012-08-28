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

module Fallback.Data.QuadTree
  (QuadTree, empty, member, insert, valid)
where

import Control.Monad (liftM2, liftM4)
import Data.Bits (complement, shiftL, shiftR)
import Data.Char (isDigit)
import qualified Text.ParserCombinators.ReadP as ReadP (munch1)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (get, lift)
import Text.Read (readPrec)

import Fallback.Data.Point (Point(Point), Position)

-------------------------------------------------------------------------------

data QuadTree = QuadTree Quadrant Quadrant Quadrant Quadrant
  deriving (Eq)

instance Show QuadTree where
  showsPrec _ (QuadTree nw sw ne se) =
    shows nw . shows sw . shows ne . shows se

instance Read QuadTree where
  readPrec = liftM4 QuadTree readPrec readPrec readPrec readPrec

-------------------------------------------------------------------------------

data Quadrant = Quadrant Int Node
  deriving (Eq)

instance Show Quadrant where
  showsPrec _ (Quadrant depth node) = shows depth . shows node

instance Read Quadrant where
  readPrec = liftM2 Quadrant readDepth readPrec
    where readDepth = ReadPrec.lift $ fmap read $ ReadP.munch1 isDigit

-------------------------------------------------------------------------------

data Node = Empty | Full | Quad Node Node Node Node
  deriving (Eq)

instance Show Node where
  showsPrec _ Empty = showChar 'e'
  showsPrec _ Full = showChar 'f'
  showsPrec _ (Quad nw sw ne se) =
    showChar 'q' . shows nw . shows sw . shows ne . shows se

instance Read Node where
  readPrec = do
    ch <- ReadPrec.get
    case ch of
      'e' -> return Empty
      'f' -> return Full
      'q' -> liftM4 Quad readPrec readPrec readPrec readPrec
      _ -> fail ("Node.readPrec: invalid character: " ++ show ch)

-------------------------------------------------------------------------------

empty :: QuadTree
empty = QuadTree (Quadrant 0 Empty) (Quadrant 0 Empty)
                 (Quadrant 0 Empty) (Quadrant 0 Empty)

-------------------------------------------------------------------------------

member :: Position -> QuadTree -> Bool
member (Point x y) (QuadTree nw sw ne se) =
  if x < 0 then
    if y < 0 then
      quadrantMember (complement x) (complement y) nw
    else
      quadrantMember (complement x) y sw
  else
    if y < 0 then
      quadrantMember x (complement y) ne
    else
      quadrantMember x y se

quadrantMember :: Int -> Int -> Quadrant -> Bool
quadrantMember x y (Quadrant depth node) =
  if huge x || huge y then False else nodeMember x y (1 `shiftL` depth) node
  where huge n = n `shiftR` depth >= 2

nodeMember :: Int -> Int -> Int -> Node -> Bool
nodeMember _ _ _ Empty = False
nodeMember _ _ _ Full = True
nodeMember x y offset (Quad nw sw ne se) =
  if x < offset then
    if y < offset then
      nodeMember x y offset' nw
    else
      nodeMember x (y - offset) offset' sw
  else
    if y < offset then
      nodeMember (x - offset) y offset' ne
    else
      nodeMember (x - offset) (y - offset) offset' se
  where offset' = offset `shiftR` 1

-------------------------------------------------------------------------------

insert :: Position -> QuadTree -> QuadTree
insert (Point x y) (QuadTree nw sw ne se) =
  if x < 0 then
    if y < 0 then
      QuadTree (quadrantInsert (complement x) (complement y) nw) sw ne se
    else
      QuadTree nw (quadrantInsert (complement x) y sw) ne se
  else
    if y < 0 then
      QuadTree nw sw (quadrantInsert x (complement y) ne) se
    else
      QuadTree nw sw ne (quadrantInsert x y se)

quadrantInsert :: Int -> Int -> Quadrant -> Quadrant
quadrantInsert x y (Quadrant depth node) =
  if huge x || huge y
  then quadrantInsert x y $ Quadrant (depth + 1) $
       case node of { Empty -> Empty; _ -> Quad node Empty Empty Empty }
  else Quadrant depth (nodeInsert x y offset node)
  where
    offset = 1 `shiftL` depth
    huge n = n `shiftR` depth >= 2

nodeInsert :: Int -> Int -> Int -> Node -> Node
nodeInsert _ _ _ Full = Full
nodeInsert _ _ 0 _ = Full
nodeInsert x y offset Empty =
  nodeInsert x y offset (Quad Empty Empty Empty Empty)
nodeInsert x y offset (Quad nw sw ne se) =
  if x < offset then
    if y < offset then
      collapse (nodeInsert x y offset' nw) sw ne se
    else
      collapse nw (nodeInsert x (y - offset) offset' sw) ne se
  else
    if y < offset then
      collapse nw sw (nodeInsert (x - offset) y offset' ne) se
    else
      collapse nw sw ne (nodeInsert (x - offset) (y - offset) offset' se)
  where
    offset' = offset `shiftR` 1

collapse :: Node -> Node -> Node -> Node -> Node
collapse Full Full Full Full = Full
collapse nw sw ne se = Quad nw sw ne se

-------------------------------------------------------------------------------

valid :: QuadTree -> Bool
valid (QuadTree nw sw ne se) =
  quadrantValid nw && quadrantValid sw && quadrantValid ne && quadrantValid se

quadrantValid :: Quadrant -> Bool
quadrantValid (Quadrant depth node) = nodeValid (1 `shiftL` depth) node

nodeValid :: Int -> Node -> Bool
nodeValid offset _ | offset < 0 = False
nodeValid _ Empty = True
nodeValid _ Full = True
nodeValid 0 (Quad _ _ _ _) = False
nodeValid _ (Quad Empty Empty Empty Empty) = False
nodeValid _ (Quad Full Full Full Full) = False
nodeValid offset (Quad nw sw ne se) =
  nodeValid offset' nw && nodeValid offset' sw &&
  nodeValid offset' ne && nodeValid offset' se
  where offset' = offset `shiftR` 1

-------------------------------------------------------------------------------
