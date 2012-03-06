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

module Fallback.Data.Color where

import Data.Word (Word8)

-------------------------------------------------------------------------------

data Color = Color { colorRed :: !Word8,
                     colorGreen :: !Word8,
                     colorBlue :: !Word8 }

blackColor :: Color
blackColor = Color 0 0 0

whiteColor :: Color
whiteColor = Color 255 255 255

applyAlpha :: Word8 -> Color -> Tint
applyAlpha a (Color r g b) = Tint r g b a

-------------------------------------------------------------------------------

data Tint = Tint { tintRed :: !Word8,
                   tintGreen :: !Word8,
                   tintBlue :: !Word8,
                   tintAlpha :: !Word8 }

whiteTint :: Tint
whiteTint = Tint 255 255 255 255

blackTint :: Tint
blackTint = Tint 0 0 0 255

toColor :: Tint -> Color
toColor (Tint r g b _) = Color r g b

-------------------------------------------------------------------------------
