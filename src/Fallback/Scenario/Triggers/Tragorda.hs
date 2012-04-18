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

module Fallback.Scenario.Triggers.Tragorda
  (compileTragorda)
where

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals (Globals(..), signRadius)
import Fallback.Scenario.Triggers.Script (setAreaCleared)
import Fallback.State.Tags (AreaTag(..))

-------------------------------------------------------------------------------

compileTragorda :: Globals -> CompileScenario ()
compileTragorda globals = compileArea Tragorda Nothing $ do

  makeExit StoneBridge [Rect 0 2 2 40] (Point 3 22)
  makeExit WhistlingWoods [Rect 2 0 51 2] (Point 19 3)
  makeExit Duskwood [Rect 53 2 2 40] (Point 51 20)

  onStartDaily 269446 $ do
    setAreaCleared Tragorda True
    addDevice_ (gAdobeDoor globals) (Point 8 34)
    addDevice_ (gAdobeDoor globals) (Point 8 36)
    addDevice_ (gAdobeDoor globals) (Point 42 7)
    addDevice_ (gAdobeDoor globals) (Point 47 5)
    addDevice_ (gAdobeDoor globals) (Point 48 38)
    addDevice_ (gStoneDoor globals) (Point 6 20)
    addDevice_ (gStoneDoor globals) (Point 8 24)
    addDevice_ (gStoneDoor globals) (Point 12 20)
    addDevice_ (gStoneDoor globals) (Point 13 11)
    addDevice_ (gStoneDoor globals) (Point 16 36)
    addDevice_ (gStoneDoor globals) (Point 19 36)
    addDevice_ (gStoneDoor globals) (Point 24 7)
    addDevice_ (gStoneDoor globals) (Point 24 11)
    addDevice_ (gStoneDoor globals) (Point 25 32)
    addDevice_ (gStoneDoor globals) (Point 25 37)
    addDevice_ (gStoneDoor globals) (Point 27 30)
    addDevice_ (gStoneDoor globals) (Point 28 13)
    addDevice_ (gStoneDoor globals) (Point 29 32)
    addDevice_ (gStoneDoor globals) (Point 29 37)
    addDevice_ (gStoneDoor globals) (Point 32 7)
    addDevice_ (gStoneDoor globals) (Point 32 11)
    addDevice_ (gStoneDoor globals) (Point 39 37)
    addDevice_ (gStoneDoor globals) (Point 40 30)
    addDevice_ (gStoneDoor globals) (Point 42 16)
    addDevice_ (gStoneDoor globals) (Point 44 29)
    addDevice_ (gStoneDoor globals) (Point 48 23)

  uniqueDevice 277292 (Point 9 24) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}THE WELCOME TRAVELER TAVERN{_}"
  uniqueDevice 915293 (Point 13 20) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}APOTHECARY{_}"
  uniqueDevice 884670 (Point 26 30) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}TRAGORDA MARKETPLACE{_}"
  uniqueDevice 285872 (Point 29 13) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}TRAGORDA CITY HALL{_}"
  uniqueDevice 642104 (Point 29 36) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \          {b}Bisla Gruer Fine Jewelry{_}\n\
      \      {i}Ornaments magical and mundane.{_}\n\
      \                           {i}Est. 1136{_}\n\
      \                   {i}A family business.{_}"
  uniqueDevice 320769 (Point 32 12) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}OFFICE OF THE CLERK{_}"
  uniqueDevice 157924 (Point 39 36) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}SVENGAARD MINERS' UNION{_}"
  uniqueDevice 802766 (Point 40 31) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {i}CHURCH OF THE RESCUED SOUL{_}"
  uniqueDevice 705513 (Point 42 8) signRadius $ \_ _ -> do
    narrate "The sign tacked to the wall reads:\n\n\
      \      {i}Sage Bora: Scholar and Artificer{_}"
  uniqueDevice 912708 (Point 42 15) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}VECHAR MEMORIAL LIBRARY{_}"

-------------------------------------------------------------------------------
