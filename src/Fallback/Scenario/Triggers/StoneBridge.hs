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

import Data.Char (toLower)

import qualified Fallback.Data.Grid as Grid
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Area (arsParty)
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Item (itemName)
import Fallback.State.Party (partyFindItem)
import Fallback.State.Resources (SoundTag(SndLever))
import Fallback.State.Simple (FaceDir(..))
import Fallback.State.Tags (AreaTag(..), MonsterTag(..), isFoodItem)
import Fallback.State.Tileset (TileTag(StoneGateClosedTile, StoneGateOpenTile))

-------------------------------------------------------------------------------

compileStoneBridge :: Globals -> CompileScenario ()
compileStoneBridge globals = compileArea StoneBridge Nothing $ do

  makeExit PerilousRoad ["ToPerilousRoad1", "ToPerilousRoad2"]
           "FromPerilousRoad"
  makeExit Tragorda ["ToTragorda"] "FromTragorda"

  onStartDaily 423026 $ do
    addUnlockedDoors globals

  uniqueDevice 180091 "Signpost" signRadius $ \_ _ -> do
    narrate "A sign has been posted along the road here:\n\n\
      \      {i}Travellers arriving from outside Svengaard{_}\n\
      \           {i}must report to the customs office{_}\n\
      \              {i}before crossing into Tragorda.{_}"

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
  simpleEnemy_ 834714 "GhastC" Ghast MindlessAI
  simpleEnemy_ 471492 "GhoulC1" Ghoul MindlessAI
  simpleEnemy_ 947193 "GhoulC2" Ghoul MindlessAI
  simpleEnemy_ 978248 "SkelC1" Skeleton MindlessAI
  simpleEnemy_ 477421 "SkelC2" Skeleton MindlessAI
  simpleEnemy_ 819889 "WraithC" Wraith MindlessAI

  -- Western edge:
  simpleEnemy_ 108042 "WolfD1" Wolf MindlessAI
  simpleEnemy_ 580912 "WolfD2" Wolf MindlessAI

  -- North shore:
  simpleEnemy_ 740281 "WolfE1" Wolf MindlessAI
  simpleEnemy_ 480981 "WolfE2" Wolf MindlessAI

  -- On bridge:
  daemonWolfDead <- simpleEnemy 283948 "DaemonWolf" DaemonWolf MindlessAI

  -- North part of forest:
  simpleEnemy_ 749087 "GhastA" Ghast MindlessAI
  simpleEnemy_ 420424 "SkelA" Skeleton MindlessAI
  simpleEnemy_ 108402 "ZomA1" Zombie MindlessAI
  simpleEnemy_ 482902 "ZomA2" Zombie MindlessAI
  simpleEnemy_ 779872 "ZomA3" Zombie MindlessAI
  simpleEnemy_ 398795 "ZomA4" Zombie MindlessAI

  -- South part of forest:
  simpleEnemy_ 998491 "GhoulB1" Ghoul MindlessAI
  simpleEnemy_ 479829 "GhoulB2" Ghoul MindlessAI
  simpleEnemy_ 498721 "GhoulB3" Ghoul MindlessAI
  simpleEnemy_ 874987 "SkelB" Skeleton MindlessAI
  simpleEnemy_ 749209 "WraithB" Wraith MindlessAI
  simpleEnemy_ 740021 "ZomB1" Zombie MindlessAI
  simpleEnemy_ 799211 "ZomB2" Zombie MindlessAI

  -- Corner of forest:
  mutantWolfBefriended <- newPersistentVar 789294 False
  mutantWolfFollowing <- newTransientVar 498104 $ return False
  (mutantWolfKey, mutantWolfDead) <-
    scriptedTownsperson 233894 "MutantWolf" MutantWolf
                        (GuardAI 5 "MutantWolf" FaceRight) $
                        \ge -> conversation $ do
      let
        initialChoices = do
          convChoice backAway "Back away.  Slowly."
          convChoice attackIt "Attack."
          convChoice giveItFood "Give it something to eat."
          convChoice tryToTalk "Try to communicate with it."
        giveItFood = convNode $ do
          mbFood <- areaGet (partyFindItem isFoodItem . arsParty)
          case mbFood of
            Nothing -> do
              convText "It occurs to you to offer this creature some food. \
                \ Unfortunately, you don't have any right now."
            Just (itemTag, itemSlot) -> do
              removeItem itemSlot
              convReset
              let foodName = map toLower (itemName itemTag)
              convText $ "Without taking your eyes off the beast, you feel\
                \ around inside your pack, and then cautiously you take out\
                \ some " ++ foodName ++ ".  As soon as it sees the food in\
                \ your hands, the wolf-thing stops growling, and looks\
                \ straight at it.  Gently, you underhand toss it towards the\
                \ creature.  It keeps its head completely still as the food\
                \ arcs towards it, following it only with its eyes, until at\
                \ the last moment it darts its head out with incredicble\
                \ speed, snatches the " ++ foodName ++ " out of the air in its\
                \ teeth, and proceeds to devour the morsel ravenously."
              writeVar mutantWolfBefriended True
              setMonsterTownAI ChaseAI (Grid.geKey ge)
              daemonDead <- readVar daemonWolfDead
              if daemonDead then do
                convText "\n\n\
                  \After it finishes, it stands still for a moment, and then\
                  \ sniffs the air.  Apparently satisfied with whatever it\
                  \ noticed, it gives a howl of satisfaction, then lies down\
                  \ and promptly goes to sleep.\n\n\
                  \It's not entirely clear where this bizarre mutant came\
                  \ from, but you seem to have pacified it."
              else do
                convText "\n\n\
                  \After it finishes, it shakes off the snowflakes from its\
                  \ fur, lumbers over to you, and proceeds licking you\
                  \ affectionately.  You pull away, as the spittle from its\
                  \ bright green tongue feels like it's melting your flesh\
                  \ slightly.  Unoffended, the wolf-thing looks up at you, its\
                  \ tail wagging.\n\n\
                  \It's not entirely clear where this bizarre mutant with\
                  \ acidic spit came from, exactly; but it appears that you've\
                  \ made a new friend."
                writeVar mutantWolfFollowing True
        tryToTalk = convNode $ do
          convReset
          convText "You crouch down slightly, so your face is level with the\
            \ wolf's.  The beast is so large that you don't have to bend down\
            \ very far, which makes you reconsider whether you really want to\
            \ be sitting this close.\n\n\
            \It stops growling for a moment, and looks at you intently.  It's\
            \ as if it is waiting for you to say something."
          convChoice backAway "Back away.  Slowly."
          convChoice tryBarking "\"Grrr.  Bow-wow!  Ruff!\""
          convChoice trySpeakingEnglish "\"Are you hungry?\""
          convChoice trySpeakingEnglish "\"Can you understand us?\""
        tryBarking = convNode $ do
          convReset
          convText "You do your best impression of some wolf-sounds, which\
            \ earns you a few seconds of blank stare from the wolf.  Then it\
            \ resumes growling at you.  Apparently, you don't know how to\
            \ speak Wolfish.  You shouldn't feel bad, though--it's really a\
            \ very tricky language."
          initialChoices
        trySpeakingEnglish = convNode $ do
          convReset
          convText "Oddly enough, like most wild animals, the wolf doesn't\
            \ seem to understand a word you're saying.  It resumes growling at\
            \ you.\n\n\
            \It may be intelligent, but evidentally it doesn't understand your\
            \ language.  Maybe you can find some other way to make it like\
            \ you."
          initialChoices
        attackIt = do
          narrate "Better to get this thing before it gets you.  You draw your\
            \ weapons, and rush towards the beast.  It snarls, and then leaps\
            \ at you."
          setMonsterIsAlly False (Grid.geKey ge)
        backAway = do
          narrate "You back away from the mutant wolf-thing, never taking your\
            \ eyes off it.  It continues to growl and watch you until you get\
            \ sufficiently far away, and then it lowers its head."
      daemonDead <- readVar daemonWolfDead
      friend <- readVar mutantWolfBefriended
      following <- readVar mutantWolfFollowing
      if daemonDead && friend then do
        -- TODO: Check if wolf is at its home.  If so, show this message;
        -- otherwise, show a different message saying that the wolf is walking
        -- back to its den.
        narrate "The mutated wolf is lying asleep next to its tree.  As you\
          \ approach, it opens one eye and sleepily lifts its head to look at\
          \ you.  After a moment, though, it puts its head back onto its paws\
          \ and goes back to sleep."
      else if following then do
        narrate "The mutated wolf looks up at you, its tail wagging.  It\
          \ continues to trot along at your side, following you around this\
          \ area.  It looks just like a little puppy--at least, it would, if\
          \ it weren't for the six legs, the absurdly large muscles, the acid\
          \ dripping from its mouth, or the fact that it's about five feet\
          \ tall."
      else if friend then do
        narrate "The mutated wolf perks its head up as you approach it.  Its\
          \ tail wagging, it bounds up over the snow towards you, then looks\
          \ up at you, ready to follow.  Obviously, it still remembers that\
          \ you fed it earlier, and you have earned its loyalty."
        setMonsterTownAI ChaseAI (Grid.geKey ge)
        writeVar mutantWolfFollowing True
      else convNode $ do
        convText "You approach the mutant wolf-thing, slowly.  It looks\
          \ intelligent.  TODO"
        initialChoices

  once 984358 (walkIn "WolfDen" `andP` varFalse mutantWolfDead) $ do
    narrate "Walking through this small, undead-infested forest, you find\
      \ yourselves in a clearing between the gnarled trees and the sheer\
      \ mountain face.  Oddly, there's no sign of the undead back here;\
      \ instead, bits of fur and old droppings are scattered around the snow. \
      \ It smells like animals back here.  {i}Living{_} animals.\n\n\
      \That's about the point a which you notice the very-much-living animal\
      \ standing by a tree at the back of the clearing.  It looks like some\
      \ kind of huge mutated wolf, with six legs, powerful muscles, and huge\
      \ teeth.  It must be fierce enough to be able keep the undead out of\
      \ here; this must be its den.  It growls at you, but does not attack\
      \ yet.\n\n\
      \If you intend to approach it, you had better do it carefully."

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
        simpleTownsperson vseed GuardArcher key (GuardAI 5 key FaceRight) sfn

  archerGuard 129578 "Archer1" $ \_ -> conversation $ do
    convText "\"Ho, there, travellers.  The bridge is closed.\"  FIXME" -- TODO
    let
      initialChoices = convNode $ do
        convChoice done "\"I guess we'll be going, then.\"  (Leave.)"
        convChoice whyClosed "\"Why is the bridge closed?\""
        convChoice whereGoes "\"Where does the bridge lead to?\""
      whereGoes = convNode $ do
        convText "\"It leads to Tragorda.\"  FIXME" -- TODO
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
        convChoice openGate "\"Just leave it to us.\""
      openGate = do
        playSound SndLever
        writeVar gateTemporarilyOpen True
      done = return ()
    initialChoices

  archerGuard 712197 "Archer2" $ \_ -> do return () -- TODO
  archerGuard 510619 "Archer3" $ \_ -> do return () -- TODO

  once 890472 (varTrue gateTemporarilyOpen `andP` walkIn "Bridge" `andP`
               varFalse daemonWolfDead) $ do
    playSound SndLever
    writeVar gateTemporarilyOpen False
    conversation $ convNode $ do
      convText "The gates close behind you.  One of the soldiers leans out the\
        \ window and shouts \"Good luck!\" before retreating back into the\
        \ guardhouse.\n\n\
        \It's just you and that daemon thing now."
      whenP (varTrue mutantWolfFollowing `andP` varFalse mutantWolfDead) $ do
        convText "  At least you've got your new pet mutant-thing with you."

  once 759825 (varTrue daemonWolfDead) $ do
    playSound SndLever
    writeVar gatePermenantlyOpen True
    narrate "Yay the gates are open now." -- TODO
    setAreaCleared StoneBridge True

  once 029841 (varTrue daemonWolfDead `andP` varFalse mutantWolfDead `andP`
               varTrue mutantWolfFollowing) $ do
    narrate "With the daemonic wolfbeast destroyed, the mutated wolf you\
      \ befriended gives a howl of victory, followed by exhausted panting.  It\
      \ is tired, and it is injured, but it seems like somehow, to it, this\
      \ was an important victory.   You wonder why.\n\n\
      \With its enemy gone, the mutant wolf trots off back towards its den to\
      \ rest."
    writeVar mutantWolfFollowing False
    setMonsterTownAI (GuardAI 5 "MutantWolf" FaceRight) =<<
      readVar mutantWolfKey

-------------------------------------------------------------------------------
