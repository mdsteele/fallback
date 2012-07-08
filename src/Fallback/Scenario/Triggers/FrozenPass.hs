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

module Fallback.Scenario.Triggers.FrozenPass
  (compileFrozenPass)
where

import Control.Applicative ((<$>))

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Area (arsCharacterPosition)
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Resources (SoundTag(SndFreeze), StripTag(IceBoom))
import Fallback.State.Simple (DamageType(ColdDamage))
import Fallback.State.Tags
  (AreaTag(..), InertItemTag(IronKey), ItemTag(InertItemTag), MonsterTag(..))

-------------------------------------------------------------------------------

compileFrozenPass :: Globals -> CompileScenario ()
compileFrozenPass globals = compileArea FrozenPass Nothing $ do

  makeExit Holmgare [Rect 53 36 2 7] (Point 51 38)

  onStartDaily 028371 $ do
    addUnlockedDoors globals

  uniqueDevice 981323 "Signpost" signRadius $ \_ _ -> do
    narrate "This signpost looks like it has seen better days, but you can\
      \ still read it clearly.  It says:\n\n\
      \      {c}Holmgare: 2 mi. E{_}"

  onStartOnce 113461 $ do
    wait 40
    narrate "Ough, ow, you're going to feel that one in the morning.  Except,\
      \ it looks like it already is morning.  Wherever you are.\n\n\
      \You look around, trying to ignore your newly acquired splitting\
      \ headache.  The ground is covered in snow.  You appear to be in some\
      \ kind of mountain pass, but looking behind you, you see that the way\
      \ through the pass has been blocked by a solid wall of ice.  It doesn't\
      \ look natural--it must have been magically created.  There is no way to\
      \ go but forward.\n\n\
      \Where is this?  You've never been here before.  This can't be your\
      \ past, can it?"
    -- TODO add Jessica ghost
    wait 20
    forcedConversationLoop "Jessica's ghostly form appears in front of you. \
      \ \"Hello there,\" she chimes, \"and welcome back to your past!  This is\
      \ the pass through the Kovola mountain range that leads into Svengaard. \
      \ That is where your last and greatest quest took place: to find the\
      \ Sunrod, and to destroy the Great Ur-Lich Vhaegyst.\"\n\n\
      \You check your persons.  Three of the four Astral Weapons are still\
      \ with you: the Lifeblade, the Moonbow, and the Starspear.  Sure enough,\
      \ the Sunrod is nowhere to be found.  You've never been anywhere called\
      \ \"Svengaard\" before, but it sounds vaguely like something you\
      \ might've seen marked on a map once, and you've certainly heard of the\
      \ Kovola mountains--they're in the northern reaches of the continent. \
      \ So at least you're in a real place."
      [("\"How are we supposed to find the Sunrod?\"",
        ContinueConv "\"Figuring that out is part of the quest, silly!\" she\
          \ answers.  \"What do you think 'find' means?  You're supposed to\
          \ {i}look{_} for it.\"\n\
          \\n\"I'll give you a big hint, though.  Vhaegyst has it.\"\n\n\
          \Great." []),
       ("\"Vhaegyst?  We've never even heard of him.\"",
        ContinueConv "Jessica looks mildly uncomfortable, as though you had\
          \ just made a minor faux pas.  \"I...wouldn't call it a 'him,'\
          \ exactly.  Anyway, don't worry about having forgotten all about\
          \ Vhaegyst.  I'm sure you'll learn more, very soon.\"\n\n\
          \Somehow, that doesn't feel very reassuring." []),
       ("\"And just how do you expect us to defeat some kind of super-lich? \
        \ We are going to get stomped, hard.\"",
        ContinueConv "\"With the other three Astral weapons that you've\
          \ already gathered, of course.\" she replies, like it was obvious. \
          \ \"There's a reason you did this quest last, you know.  It was the\
          \ hardest one, and certainly the most dangerous.  Vhaegyst is a\
          \ serious force to be reckoned with.  I mean, yikes.\"\n\
          \\n\"I'll think you'll be fine, though.  You're all pros by now,\
          \ right?\"" []),
       ("\"But we never went on this quest!  We've never even been here\
        \ before!\"", StopConv ())]
    -- TODO fade away Jessica
    narrate "\"Nonsense!\" she cheerfully declares.  \"You came straight to\
      \ Corenglen from here after defeating Vhaegyst.  Now run along, and go\
      \ do it again.  I'll meet up with you after you are victorious.\"  And\
      \ with that, she fades away.\n\n\
      \Well, this does {i}not{_} look good for the good guys.  But you seem to\
      \ be sealed in this region by that giant ice wall behind you, so you\
      \ have little choice but to press on forward.  Maybe, just maybe, you\
      \ really can defeat this Vhaegyst lich, and maybe then you'll get some\
      \ answers."
  simpleMonster 398273 Wolf "WolfA1" ChaseAI
  simpleMonster 142118 Wolf "WolfA2" ChaseAI
  simpleMonster 293554 Wolf "WolfA3" ChaseAI
  simpleMonster 828135 Wolf "WolfA4" MindlessAI

  once 830701 (walkIn (Rect 40 20 10 1)) $ do
    narrate "{b}The undead!{_}\n\n\
      \Well, you're apparently on a quest to defeat an evil lich, so you\
      \ probably shouldn't be surprised to run into a group of zombies\
      \ shambling around.  Get used to it--there's likely to be more in the\
      \ near future.\n\n\
      \Zombies are pretty stupid, but they're dangerous.  All undead are. \
      \ They don't feel fear, they don't feel pain, and they {i}always{_} feel\
      \ hunger for mortal flesh.  Even a smallish pack of zombies would be\
      \ perilous foes for warriors as inexperienced as yourselves, but with\
      \ three out of the four Astral Weapons still in your hands, you should\
      \ be able to get through this."
  simpleMonster 547952 Zombie "ZomB1" MindlessAI
  simpleMonster 453147 Zombie "ZomB2" MindlessAI
  simpleMonster 448374 Zombie "ZomB3" MindlessAI
  simpleMonster 475373 Zombie "ZomB4" MindlessAI

  simpleMonster 972911 Zombie "ZomC1" MindlessAI
  simpleMonster 080213 Zombie "ZomC2" MindlessAI
  simpleMonster 420917 Zombie "ZomC3" MindlessAI

  once 385861 (walkIn (Rect 3 34 2 2)) $ do
    narrate "This old, cracked building looks like it has seen better days. \
      \ Well, assuming this area has {i}ever{_} had better days; so far this\
      \ place is depressing and cold and not somewhere you'd ever much want to\
      \ go on vacation.  But the flat roof seems to be sagging under the\
      \ weight of the snow piled up, and the look of the walls doesn't give\
      \ you a lot of confidence.  If you're planning to step inside, you think\
      \ you might not want to linger very long."
  simpleMonster 233177 Skeleton "SkelD1" MindlessAI
  simpleMonster 571346 Skeleton "SkelD2" MindlessAI
  simpleMonster 035978 Skeleton "SkelD3" MindlessAI
  simpleMonster 697145 Ghoul "GhoulD1" MindlessAI
  simpleMonster 296464 Ghoul "GhoulD2" MindlessAI
  simpleMonster 080509 Wraith "WraithD" MindlessAI

  uniqueDevice 044279 "TornBook" 1 $ \_ _ -> do
    narrate "Aha!  This book probably holds all sorts of interesting\
      \ information, clues about what's going on around here, and powerful\
      \ magical arcana that will be a great boon to you in your\
      \ adventures.\n\n\
      \At least, it probably {i}did{_}.  But it looks like the ghouls in here\
      \ went and chewed it to pieces long before you arrived.  Not a single\
      \ legible page remains."

  once 232166 (walkOn (Point 12 39)) $ do
    narrate "Now that's interesting...what first looked like another crack in\
      \ the wall turns out to be a secret panel leading to a narrow passage\
      \ behind the building.  Why would someone hide a secret passage behind a\
      \ building that's probably going to fall over any day now?"

  once 434118 (walkOn (Point 18 40)) $ do
    narrate "Brrrrr...it is {i}freezing{_} back here.  Evidentally not a\
      \ lot of sunlight makes it down into this tiny opening.\n\n\
      \You take a moment to examine the walls of the passage.  Now that you\
      \ look more closely, the rock walls don't look natural, which means\
      \ that someone must have dug it out on purpose.  But they don't really\
      \ look like they were dug out with tools, or animal claws, or anything\
      \ else you can think of either.  Which means...magic?  Pretty powerful\
      \ magic, if whoever it was was boring through solid rock.\n\n\
      \Odd.  Powerful magic tends to be in short supply, and is not usually\
      \ wasted on digging pointless tunnels."

  ironDoorAttempted <- newPersistentVar 126701 False
  ironDoorUnlocked <- newPersistentVar 477117 False
  let tryOpenIronDoor _ _ = do
        unlocked <- readVar ironDoorUnlocked
        if unlocked then return True else do
        firstAttempt <- not <$> readVar ironDoorAttempted
        writeVar ironDoorAttempted True
        hasKey <- doesPartyHaveItem (InertItemTag IronKey)
        if firstAttempt || hasKey then do
          narrate ((if firstAttempt then "There's a heavy wrought iron door\
            \ mounted in a wall at the end of the tunnel here.  What with the\
            \ winter weather here, the metal is very cold to the touch; you\
            \ find you have to cover your hands before gripping the door\
            \ handle.  Carefully, you pull on the handle, but the door is\
            \ locked.\n\n" else "") ++
            (if hasKey then "The metal of the door looks very similar to the\
               \ metal of the iron key that you won from that Dactylid demon. \
               \ You try the key in the keyhole, and it fits.  You have to\
               \ struggle a bit to move the frozen lock, but with a creaking\
               \ noise the latch finally gives way, and the door swings open."
             else "Lacking any key to open it, you examine the door more\
               \ carefully, trying to get any clue of what might lie behind\
               \ it, or who made this passage and put a door there.  You find\
               \ nothing that provides any answers."))
        else do
          setMessage "The iron door is still locked.  You still don't have the\
                     \ key."
        if not hasKey then return False else do
        playDoorUnlockSound
        writeVar ironDoorUnlocked True
        return True
  ironDoorDevice <-
    newDoorDevice 202933 tryOpenIronDoor (const $ const $ return True)
  onStartDaily 093423 $ do
    addDevice_ ironDoorDevice =<< demandOneTerrainMark "IronDoor"

  once 328351 (walkOn (Point 26 40)) $ do
    narrate "A huge, solid crystal of ice dominates the center of this tiny\
      \ room.  You can almost feel it sucking the heat out of your bodies,\
      \ even from here.  It is almost certainly magical, whatever it's for."

  uniqueDevice 623179 "IceCrystal" 1 $ \_ charNum -> do
    mbTouch <- forcedChoice "The faces of the giant ice crystal are almost\
      \ flawless.  From up close, you can definitely feel the aura of magic\
      \ around this thing; if nothing else, it's somehow making the room a\
      \ whole lot colder than it would be naturally.  It sure isn't just for\
      \ decoration, or it wouldn't be stowed away back here, behind a door\
      \ locked by a key that was held by an infernal daemon.  Logically, this\
      \ block of ice must {i}do{_} something.  And you can't help feeling that\
      \ it's probably something bad.\n\n\
      \Okay.  So that means you're standing in front of an evil magical ice\
      \ crystal.  What's the next step?"
      [("Touch it.", Just True), ("Smash it.", Just False),
       ("Leave it alone.", Nothing)]
    case mbTouch of
      Just True -> do
        narrate "You reach out a hand hesitantly, and then, slowly, place your\
          \ fingertips on the ice.  It is colder than you thought possible. \
          \ For a fraction of a moment, nothing happens.  And then..."
        wait 6
        pos <- areaGet (arsCharacterPosition charNum)
        playSound SndFreeze
        addBoomDoodadAtPosition IceBoom 1 pos
        dealDamage [(HitCharacter charNum, ColdDamage, 150)]
        wait 10
        narrate "Augh, bloody hell, that hurt!  Okay, new rule: touching an\
          \ evil magical ice crystal is a Bad Plan."
      Just False -> narrate "Have you ever tried to shatter a giant, solid\
        \ block of ice?  Even normal ice?  It doesn't work very well; ice is\
        \ actually pretty strong.  Your weapon just bounces off, doing hardly\
        \ any damage at all beyond loosing a few chips of frost."
      Nothing -> return ()

  uniqueDevice 477627 "Runes" signRadius $ \_ _ -> do
    narrate "There is some kind of inscription on the wall.  Peering closely\
      \ at it, it appears to be written in a language you can't understand,\
      \ using symbols you don't even recognize.  A foreign tongue?  Magical\
      \ runes?  Meaningless shapes?  You can't tell for sure."

-------------------------------------------------------------------------------
