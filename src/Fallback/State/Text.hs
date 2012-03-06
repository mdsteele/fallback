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

module Fallback.State.Text
  (TextLine, wrapText, textLineLength, takeTextLine, textLineWidth,
   textLineSize, paintTextLine)
where

import Control.Applicative ((<$>))
import Control.Monad (foldM_)
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe (maybeToList)
import Text.ParserCombinators.ReadP ((<++), ReadP, readP_to_S)
import qualified Text.ParserCombinators.ReadP as ReadP

import Fallback.Data.Color (Color)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcFont, rsrcStatusIcons)

-------------------------------------------------------------------------------

newtype TextLine = TextLine [Piece]

data Piece = SpritePiece Sprite | StringPiece Font String

-------------------------------------------------------------------------------

data Token = NewlineToken | SpaceToken Font String | WordToken [Piece]

wrapText :: Resources -> Int -> String -> [TextLine]
wrapText resources width = map TextLine . reverse . fst .
                           foldl' wrap ([[]], []) . tokenize where
  wrap (tlines, space) token = case token of
    NewlineToken -> ([] : tlines, [])
    SpaceToken font str -> (tlines, space ++ [StringPiece font str])
    WordToken pieces ->
      let tline' = head tlines ++ space ++ pieces
          width' = textLineWidth (TextLine tline')
      in ((if width' <= width then tline' : tail tlines
           else pieces : tlines), [])
  tokenize string = case readP_to_S (tokensParser startFont) string of
                      [] -> error "wrapText: no parse"
                      [(tokens, "")] -> tokens
                      [(_, _:_)] -> error "wrapText: extra text left over"
                      _:_:_ -> error "wrapText: too many parses"
    where

      tokensParser :: Font -> ReadP [Token]
      tokensParser font = (ReadP.eof >> return []) <++ do
        (token, font') <- tokenParser font
        (token :) <$> tokensParser font'

      tokenParser :: Font -> ReadP (Token, Font)
      tokenParser font =
        newlineParser font <++ spaceParser font <++ wordParser font

      newlineParser :: Font -> ReadP (Token, Font)
      newlineParser font = ReadP.char '\n' >> return (NewlineToken, font)

      spaceParser :: Font -> ReadP (Token, Font)
      spaceParser font = do
        spaces <- ReadP.munch1 (== ' ')
        return (SpaceToken font spaces, font)

      wordParser :: Font -> ReadP (Token, Font)
      wordParser font = do
        (pieces, font') <- piecesParser font
        return (WordToken pieces, font')

      piecesParser :: Font -> ReadP ([Piece], Font)
      piecesParser font = return ([], font) ++> do
        (mbPiece, font', stop) <- pieceParser font
        if stop then return (maybeToList mbPiece, font') else do
          (pieces, font'') <- piecesParser font'
          return (maybe id (:) mbPiece pieces, font'')

      pieceParser :: Font -> ReadP (Maybe Piece, Font, Bool)
      pieceParser font = specialParser font <++ stringParser font

      stringParser :: Font -> ReadP (Maybe Piece, Font, Bool)
      stringParser font = do
        let tr c = if c == '~' then ' ' else c
        chars <- map tr <$> ReadP.munch1 (\c -> c `notElem` "{} \n-")
        hyphs <- ReadP.munch (== '-')
        return $ if null hyphs
                 then (Just (StringPiece font chars), font, False)
                 else (Just (StringPiece font (chars ++ hyphs)), font, True)

      specialParser :: Font -> ReadP (Maybe Piece, Font, Bool)
      specialParser font = ReadP.between (ReadP.char '{') (ReadP.char '}') $ do
        oper <- ReadP.get
        case oper of
          '_' -> return (Nothing, startFont, False)
          'b' -> return (Nothing, boldFont, False)
          'c' -> return (Nothing, chanceryFont, False)
          'i' -> return (Nothing, italicFont, False)
          's' -> do index <- intParser
                    return ((Just $ SpritePiece $
                             rsrcStatusIcons resources ! index), font, False)
          _ -> ReadP.pfail

      intParser :: ReadP Int
      intParser = read <$> ReadP.munch1 isDigit

      startFont = rsrcFont resources FontGeorgia11
      boldFont = rsrcFont resources FontGeorgiaBold11
      chanceryFont = rsrcFont resources FontChancery14
      italicFont = rsrcFont resources FontGeorgiaItalic11

      (++>) = flip (<++)

-------------------------------------------------------------------------------

textLineLength :: TextLine -> Int
textLineLength (TextLine pieces) = sum $ map pieceSize pieces where
  pieceSize (SpritePiece _) = 1
  pieceSize (StringPiece _ string) = length string

takeTextLine :: Int -> TextLine -> TextLine
takeTextLine _ (TextLine []) = TextLine []
takeTextLine n (TextLine (p : ps)) =
  if n <= 0 then TextLine [] else
    case p of SpritePiece _ -> continue (n - 1)
              StringPiece font string ->
                let n' = n - length string
                in if n' < 0 then TextLine [StringPiece font (take n string)]
                   else continue n'
  where continue n' = let TextLine ps' = takeTextLine n' (TextLine ps)
                      in TextLine (p : ps')

textLineWidth :: TextLine -> Int
textLineWidth (TextLine pieces) = sum $ map pieceWidth pieces where
  pieceWidth (SpritePiece sprite) = spriteWidth sprite
  pieceWidth (StringPiece font str) = textRenderWidth font str

textLineSize :: TextLine -> (Int, Int)
textLineSize (TextLine pieces) = foldl' fn (0, 0) $ map pieceSize pieces where
  fn (w1, h1) (w2, h2) = ((,) $! (w1 + w2)) $! (max h1 h2)
  pieceSize (SpritePiece sprite) = spriteSize sprite
  pieceSize (StringPiece font str) = textRenderSize font str

paintTextLine :: Color -> LocSpec Int -> TextLine -> Paint ()
paintTextLine color loc (TextLine pieces) = do
  let (w, h) = textLineSize (TextLine pieces)
  let Point x0 y0 = locTopleft loc (w, h)
  let paintNext x (SpritePiece sprite) = do
        blitTopleft sprite (Point x y0)
        return (x + spriteWidth sprite)
      paintNext x (StringPiece font string) = do
        drawText font color (LocTopleft $ Point x y0) string
        return (x + textRenderWidth font string)
  foldM_ paintNext x0 pieces

-------------------------------------------------------------------------------
