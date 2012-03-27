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

module Fallback.Event
  (Event(..), Key(..), KeyMod(..),
   fromSDLKey, toSDLKey, letterKeys,
   getKeyStateIO, getMouseButtonStateIO, getAbsoluteMousePosition)
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Extras as SDLx

import Fallback.Data.Point (IPoint, Point(Point))

-------------------------------------------------------------------------------

data Event = EvTick
           | EvQuit
           | EvKeyDown !Key [KeyMod] Char   -- ^ key, modifiers, character
           | EvKeyUp !Key                   -- ^ key
           | EvMouseMotion !IPoint !IPoint  -- ^ location, delta
           | EvMouseDown !IPoint            -- ^ location
           | EvMouseUp !IPoint              -- ^ location
           | EvScrollDownwards !IPoint      -- ^ location
           | EvScrollUpwards !IPoint        -- ^ location
           | EvFocus !IPoint                -- ^ mouse location
           | EvBlur
  deriving (Eq, Show)

data KeyMod = KeyModCmd | KeyModShift
  deriving (Eq, Ord, Show)

data Key = KeyUnknown
         | KeyTab | KeyReturn | KeyEscape | KeySpace | KeyMinus | KeyPlus
         | Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
         | KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
         | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
         | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
         | KeyUpArrow | KeyDownArrow | KeyRightArrow | KeyLeftArrow
  deriving (Eq, Show)

toSDLKey :: Key -> SDL.SDLKey
toSDLKey KeyUnknown = SDL.SDLK_UNKNOWN
toSDLKey KeyTab = SDL.SDLK_TAB
toSDLKey KeyReturn = SDL.SDLK_RETURN
toSDLKey KeyEscape = SDL.SDLK_ESCAPE
toSDLKey KeySpace = SDL.SDLK_SPACE
toSDLKey KeyMinus = SDL.SDLK_MINUS
toSDLKey KeyPlus = SDL.SDLK_EQUALS -- sic
toSDLKey Key0 = SDL.SDLK_0
toSDLKey Key1 = SDL.SDLK_1
toSDLKey Key2 = SDL.SDLK_2
toSDLKey Key3 = SDL.SDLK_3
toSDLKey Key4 = SDL.SDLK_4
toSDLKey Key5 = SDL.SDLK_5
toSDLKey Key6 = SDL.SDLK_6
toSDLKey Key7 = SDL.SDLK_7
toSDLKey Key8 = SDL.SDLK_8
toSDLKey Key9 = SDL.SDLK_9
toSDLKey KeyA = SDL.SDLK_a
toSDLKey KeyB = SDL.SDLK_b
toSDLKey KeyC = SDL.SDLK_c
toSDLKey KeyD = SDL.SDLK_d
toSDLKey KeyE = SDL.SDLK_e
toSDLKey KeyF = SDL.SDLK_f
toSDLKey KeyG = SDL.SDLK_g
toSDLKey KeyH = SDL.SDLK_h
toSDLKey KeyI = SDL.SDLK_i
toSDLKey KeyJ = SDL.SDLK_j
toSDLKey KeyK = SDL.SDLK_k
toSDLKey KeyL = SDL.SDLK_l
toSDLKey KeyM = SDL.SDLK_m
toSDLKey KeyN = SDL.SDLK_n
toSDLKey KeyO = SDL.SDLK_o
toSDLKey KeyP = SDL.SDLK_p
toSDLKey KeyQ = SDL.SDLK_q
toSDLKey KeyR = SDL.SDLK_r
toSDLKey KeyS = SDL.SDLK_s
toSDLKey KeyT = SDL.SDLK_t
toSDLKey KeyU = SDL.SDLK_u
toSDLKey KeyV = SDL.SDLK_v
toSDLKey KeyW = SDL.SDLK_w
toSDLKey KeyX = SDL.SDLK_x
toSDLKey KeyY = SDL.SDLK_y
toSDLKey KeyZ = SDL.SDLK_z
toSDLKey KeyUpArrow = SDL.SDLK_UP
toSDLKey KeyDownArrow = SDL.SDLK_DOWN
toSDLKey KeyRightArrow = SDL.SDLK_RIGHT
toSDLKey KeyLeftArrow = SDL.SDLK_LEFT

fromSDLKey :: SDL.SDLKey -> Key
fromSDLKey SDL.SDLK_TAB = KeyTab
fromSDLKey SDL.SDLK_RETURN = KeyReturn
fromSDLKey SDL.SDLK_ESCAPE = KeyEscape
fromSDLKey SDL.SDLK_SPACE = KeySpace
fromSDLKey SDL.SDLK_MINUS = KeyMinus
fromSDLKey SDL.SDLK_0 = Key0
fromSDLKey SDL.SDLK_1 = Key1
fromSDLKey SDL.SDLK_2 = Key2
fromSDLKey SDL.SDLK_3 = Key3
fromSDLKey SDL.SDLK_4 = Key4
fromSDLKey SDL.SDLK_5 = Key5
fromSDLKey SDL.SDLK_6 = Key6
fromSDLKey SDL.SDLK_7 = Key7
fromSDLKey SDL.SDLK_8 = Key8
fromSDLKey SDL.SDLK_9 = Key9
fromSDLKey SDL.SDLK_EQUALS = KeyPlus -- sic
fromSDLKey SDL.SDLK_a = KeyA
fromSDLKey SDL.SDLK_b = KeyB
fromSDLKey SDL.SDLK_c = KeyC
fromSDLKey SDL.SDLK_d = KeyD
fromSDLKey SDL.SDLK_e = KeyE
fromSDLKey SDL.SDLK_f = KeyF
fromSDLKey SDL.SDLK_g = KeyG
fromSDLKey SDL.SDLK_h = KeyH
fromSDLKey SDL.SDLK_i = KeyI
fromSDLKey SDL.SDLK_j = KeyJ
fromSDLKey SDL.SDLK_k = KeyK
fromSDLKey SDL.SDLK_l = KeyL
fromSDLKey SDL.SDLK_m = KeyM
fromSDLKey SDL.SDLK_n = KeyN
fromSDLKey SDL.SDLK_o = KeyO
fromSDLKey SDL.SDLK_p = KeyP
fromSDLKey SDL.SDLK_q = KeyQ
fromSDLKey SDL.SDLK_r = KeyR
fromSDLKey SDL.SDLK_s = KeyS
fromSDLKey SDL.SDLK_t = KeyT
fromSDLKey SDL.SDLK_u = KeyU
fromSDLKey SDL.SDLK_v = KeyV
fromSDLKey SDL.SDLK_w = KeyW
fromSDLKey SDL.SDLK_x = KeyX
fromSDLKey SDL.SDLK_y = KeyY
fromSDLKey SDL.SDLK_z = KeyZ
fromSDLKey SDL.SDLK_UP = KeyUpArrow
fromSDLKey SDL.SDLK_DOWN = KeyDownArrow
fromSDLKey SDL.SDLK_RIGHT = KeyRightArrow
fromSDLKey SDL.SDLK_LEFT = KeyLeftArrow
fromSDLKey _ = KeyUnknown

-- | A list of the letter keys, in order from A to Z.
letterKeys :: [Key]
letterKeys = [KeyA, KeyB, KeyC, KeyD, KeyE, KeyF, KeyG, KeyH, KeyI, KeyJ,
              KeyK, KeyL, KeyM, KeyN, KeyO, KeyP, KeyQ, KeyR, KeyS, KeyT,
              KeyU, KeyV, KeyW, KeyX, KeyY, KeyZ]

-------------------------------------------------------------------------------

getKeyStateIO :: Key -> IO Bool
getKeyStateIO k = SDLx.getKeyState (toSDLKey k)

getMouseButtonStateIO :: IO Bool
getMouseButtonStateIO = do
  (_, _, buttons) <- SDL.getMouseState
  return (SDL.ButtonLeft `elem` buttons)

getAbsoluteMousePosition :: IO IPoint
getAbsoluteMousePosition = do
  (x, y, _) <- SDL.getMouseState
  return (Point x y)

-------------------------------------------------------------------------------
