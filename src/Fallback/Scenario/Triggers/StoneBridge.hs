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
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Creature (MonsterTownAI(GuardAI, MindlessAI))
import Fallback.State.Tags (AreaTag(..), MonsterTag(..))
import Fallback.State.Tileset (TileTag(StoneGateClosedTile, StoneGateOpenTile))

-------------------------------------------------------------------------------

compileStoneBridge :: Globals -> CompileScenario ()
compileStoneBridge globals = compileArea StoneBridge Nothing $ do

  makeExit PerilousRoad [Rect 0 0 2 10, Rect 2 0 6 2] (Point 3 4)
  makeExit Tragorda [Rect 53 0 2 44] (Point 51 22)

  onStartDaily 423026 $ do
    addUnlockedDoors globals

  uniqueDevice 180091 "Signpost" signRadius $ \_ _ -> do
    narrate "A sign has been posted along the road here:\n\n\
      \      {i}Travellers arriving from outside Svengaard{_}\n\
      \           {i}must report to the customs office{_}\n\
      \              {i}before crossing into Tragorda.{_}"

  -- North part of forest:
  simpleMonster 749087 Ghast "GhastA" MindlessAI
  simpleMonster 420424 Skeleton "SkelA" MindlessAI
  simpleMonster 108402 Zombie "ZomA1" MindlessAI
  simpleMonster 482902 Zombie "ZomA2" MindlessAI
  simpleMonster 779872 Zombie "ZomA3" MindlessAI
  simpleMonster 398795 Zombie "ZomA4" MindlessAI

  -- South part of forest:
  simpleMonster 998491 Ghoul "GhoulB1" MindlessAI
  simpleMonster 479829 Ghoul "GhoulB2" MindlessAI
  simpleMonster 498721 Ghoul "GhoulB3" MindlessAI
  simpleMonster 874987 Skeleton "SkelB" MindlessAI
  simpleMonster 749209 Wraith "WraithB" MindlessAI
  simpleMonster 740021 Zombie "ZomB1" MindlessAI
  simpleMonster 799211 Zombie "ZomB2" MindlessAI

  -- Customs office:
  uniqueDevice 800253 "CustomsSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}TRAGORDA CUSTOMS OFFICE{_}"
  once 025516 (walkIn "CustomsFrontRoom") $ do
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
  simpleMonster 834714 Ghast "GhastC" MindlessAI
  simpleMonster 471492 Ghoul "GhoulC1" MindlessAI
  simpleMonster 947193 Ghoul "GhoulC2" MindlessAI
  simpleMonster 978248 Skeleton "SkelC1" MindlessAI
  simpleMonster 477421 Skeleton "SkelC2" MindlessAI
  simpleMonster 819889 Wraith "WraithC" MindlessAI

  -- West forest:
  simpleMonster 108042 Wolf "WolfD1" MindlessAI
  simpleMonster 580912 Wolf "WolfD2" MindlessAI

  -- North shore:
  simpleMonster 740281 Wolf "WolfE1" MindlessAI
  simpleMonster 480981 Wolf "WolfE2" MindlessAI

  -- Bridge gates:
  gateTemporarilyOpen <- newTransientVar 019112 $ return False
  gatePermenantlyOpen <- newPersistentVar 848929 False
  do let frontGateOpen = varTrue gateTemporarilyOpen `orP`
                         varTrue gatePermenantlyOpen
     let rearGateOpen = varTrue gatePermenantlyOpen
     trigger 018489 (frontGateOpen) $ do
       setTerrain StoneGateOpenTile =<< lookupTerrainMark "FrontGate"
     trigger 498239 (notP frontGateOpen) $ do
       setTerrain StoneGateClosedTile =<< lookupTerrainMark "FrontGate"
     trigger 298323 (rearGateOpen) $ do
       setTerrain StoneGateOpenTile =<< lookupTerrainMark "RearGate"
     trigger 102982 (notP rearGateOpen) $ do
       setTerrain StoneGateClosedTile =<< lookupTerrainMark "RearGate"

  let archerGuard vseed key sfn =
        simpleTownsperson vseed GuardArcher key (GuardAI key) sfn

  archerGuard 129578 "Archer1" $ \_ -> conversation $ do
    convText "\"Ho, there, travellers.  The bridge is closed.\"  FIXME"
    let
      initialChoices = convNode $ do
        convChoice done "\"I guess we'll be going, then.\"  (Leave.)"
        convChoice whyClosed "\"Why is the bridge closed?\""
        convChoice whereGoes "\"Where does the bridge lead to?\""
      whereGoes = convNode $ do
        convText "\"It leads to Tragorda.\"  FIXME"
      whyClosed = convNode $ do
        convText "\"There's a...a beast on the bridge,\" the guard stammers. \
          \ \"Some kind of wolf-thing.\"  Huh.  that doesn't sound so bad.\n\
          \\n\"Some kind of enormous twelve-foot fire-breathing purple daemon\
          \ wolf straight from the pits of hell.\"  Okay, that could be more\
          \ of a problem.\n\n\
          \The other guard cuts in, \"That thing, it just showed up one day. \
          \ Nearly killed all of us.  But then it just wandered onto the\
          \ bridge and stood itself there.  We were lucky we could close the\
          \ gates on it and trap it there, else it might've gone into the\
          \ city.  But it's been there for days and hasn't even needed water;\
          \ if it really is a daemon, me might never be able to starve it\
          \ out.\""
        convChoice offerHelp "\"Maybe we could get rid of it for you.\""
      offerHelp = convNode $ do
        convText "\"You crazy?  That thing'll tear you all limb from limb.  It\
          \ can breathe fire!  It can spit acid!  You'll never survive!\"\n\n\
          \The first guard chimes in again, \"We can't risk letting that thing\
          \ out.  We've just got to keep the gates closed until we can think\
          \ of something.\""
        convChoice noReally "\"No really, we can do it.  We're pros.  Just\
          \ open the gate on this side, and we'll go in and kill it.\""
        convChoice neverMind "\"Yeah, on second thought, we'd better leave\
          \ that thing alone for now.\""
      neverMind = convNode $ do
        convReset
        fail "FIXME"
      noReally = convNode $ do
        convReset
        convText "The two guards look at each other.  They clearly think\
          \ you're nuts.\n\
          \\n\"All right,\" says the first guard, finally.  \"I'm not sure you\
          \ know what you're getting yourselves into, but...all right.  I'll\
          \ open the gate on this side, but I'm going to close it again once\
          \ you're on the bridge.  We can't let that thing out now that we've\
          \ got it caught.\""
        convChoice done "\"Just leave it to us.\""
        writeVar gateTemporarilyOpen True
      done = return ()
    initialChoices

  archerGuard 712197 "Archer2" $ \_ -> do return () -- TODO
  archerGuard 510619 "Archer3" $ \_ -> do return () -- TODO

  simpleMonster 283948 DemonWolf "DemonWolf" MindlessAI -- FIXME

-------------------------------------------------------------------------------
