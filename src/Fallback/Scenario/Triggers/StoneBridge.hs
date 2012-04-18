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

module Fallback.Scenario.Triggers.StoneBridge
  (compileStoneBridge)
where

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals (Globals(..), signRadius)
--import Fallback.Scenario.Triggers.Script (setAreaCleared)
import Fallback.State.Creature (MonsterTownAI(GuardAI))
import Fallback.State.Tags (AreaTag(..), MonsterTag(..))

-------------------------------------------------------------------------------

compileStoneBridge :: Globals -> CompileScenario ()
compileStoneBridge globals = compileArea StoneBridge Nothing $ do

  makeExit PerilousRoad [Rect 0 0 2 10, Rect 2 0 6 2] (Point 3 4)
  makeExit Tragorda [Rect 53 0 2 44] (Point 51 22)

  onStartDaily 423026 $ do
    addDevice_ (gStoneDoor globals) (Point 19 12)
    addDevice_ (gStoneDoor globals) (Point 20 8)

  uniqueDevice 180091 (Point 12 18) signRadius $ \_ _ -> do
    narrate "A sign has been posted along the road here:\n\n\
      \      {i}Travellers arriving from outside Svengaard{_}\n\
      \           {i}must report to the customs office{_}\n\
      \              {i}before crossing into Tragorda.{_}"
  uniqueDevice 800253 (Point 20 12) signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}TRAGORDA CUSTOMS OFFICE{_}"

  once 025516 (walkIn (Rect 17 9 5 3)) $ do
    narrate "This office looks like it has seen better days.  According to the\
      \ sign by the door, this is the customs office--presumably, travellers\
      \ coming from over the Kovola mountain range to the northwest are\
      \ supposed to check in here before entering the city.  But since the\
      \ only pass through the mountains--the one you just came from--is\
      \ currently blocked by a giant magical wall of ice, you don't imagine\
      \ there have been any such travellers lately.\n\n\
      \Anyway, there's no one here to check you in, and the office is in\
      \ disarray.  Papers are scattered all over the floor, and the desk here\
      \ seems to be covered in scratches--claw marks?  The far wall looks to\
      \ be in poor shape, and you're pretty sure you can hear some kind of\
      \ noises coming from behind it."

  let archerGuard vseed pos sfn =
        simpleTownsperson vseed GuardArcher pos (GuardAI pos) sfn
  archerGuard 129578 (Point 26 19) $ \_ -> do return () -- TODO
  archerGuard 712197 (Point 25 18) $ \_ -> do return () -- TODO
  archerGuard 510619 (Point 23 25) $ \_ -> do return () -- TODO

-------------------------------------------------------------------------------
