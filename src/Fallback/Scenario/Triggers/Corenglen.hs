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

module Fallback.Scenario.Triggers.Corenglen
  (compileCorenglen)
where

import Control.Monad (join)

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.MountainPath (startingPosition)
import Fallback.Scenario.Triggers.Script
import Fallback.State.Tags

-------------------------------------------------------------------------------

compileCorenglen :: Globals -> CompileScenario ()
compileCorenglen globals = compileArea Corenglen Nothing $ do

  makeExit MountainPath ["ToMountainPath"] (Point 2 16)

  alwaysLockedDoor <- newDevice 963970 1 $ \_ _ -> do
    setMessage "The door is locked."

  onStartDaily 820304 $ do
    addUnlockedDoors globals
    addDeviceOnMarks alwaysLockedDoor "Locked"

  let isFirstTimeThroughLongvale = varEq (gTimesThroughLongvale globals) 0
  let isSecondTimeThroughLongvale = varEq (gTimesThroughLongvale globals) 1
  let svengaardEntryPosition = Point 5 9

  once 340838 isFirstTimeThroughLongvale $ do
    wait 40
    narrate "You step down through the valley into Corenglen.  The villagers\
      \ could probably see and hear your fight with the revenant up on the\
      \ valley path that descends into this village, so you were prepared to\
      \ expect some thanks or even applause; but what you now find catches\
      \ you completely off guard.\n\n\
      \The entire town is out lining the streets.  All of them are cheering\
      \ for you.  Children are running towards you with bunches of flowers\
      \ in their arms.  Fathers are still shaking from fear only recently\
      \ abated.  Mothers are holding their babies, tears of joy and thanks\
      \ and relief streaming down their faces.  Everyone is shouting, and\
      \ cheering, and looking at you as though you are their heroes and\
      \ saviors.\n\n\
      \...because you killed what, one revenant?  What on earth is going on?"
    ok <- forcedChoice "A man wearing some kind of sash steps forward to\
      \ speak to you, water in his eyes.  You can barely hear him over the\
      \ din.  \"I welcome you, heroes, to Corenglen; I am Mayor Faz.  We are\
      \ forever in your debt!  We...we don't even know who you are yet, but\
      \ if you had arrived even a few minutes later, why...\"  He gets\
      \ choked up at this point, and takes a moment to recover before\
      \ continuing.  No, really, what is going on here, and why is everyone\
      \ acting like you just saved them from total destruction or\
      \ something?  You can plainly see that a number of the villagers are\
      \ wearing swords or even armor.  There's {i}no{_} way they could have\
      \ thought that that one monster posed them enough threat to explain\
      \ how they're acting.\n\
      \\n\"Please,\" the mayor continues, \"come join us at the meeting\
      \ hall.  We are putting together a feast in your honor even as we\
      \ speak.  It's the least we can do.  I beg you to do us the honor of\
      \ staying for a bit.  I'll lead you there now?\""
      [("\"Of course.  We'd be happy to stay for dinner.\"", True),
       ("\"There's, uh, no need to thank us.  It was nothing.\"", False)]
    narrate ((if ok then "\"Thank you, thank you!  Please.  This way.\"  He\
                \ bows and gestures to follow him down the road."
              else "Your show of humility is almost too much for the poor\
                \ man.  \"Nothing!\" he chokes.  \"Perhaps to you, but to\
                \ us, why, we would all be...\"  You hurredly assure him\
                \ that that you will stay after all, if only to keep him\
                \ from crying again, and after wiping his eyes, he smiles\
                \ and moves to lead you down the road. ") ++
      "\n\n\
      \You fall in line behind the mayor and follow him toward the village\
      \ meeting hall.  As you proceed through the town, the villagers form a\
      \ sort of parade behind you.  They continue to cry and cheer.  Some of\
      \ the children who managed to find some confetti are now throwing\
      \ it.\n\n\
      \This is going to be a very awkward dinner.")
    setPartyPosition (Point 23 34) -- TODO cutscene of parade
    narrate "You sit down to dinner, surrounded by grateful townspeople. \
      \ The settings aren't fancy, even for a backwater village like this\
      \ one, and preparations were obviously rushed--they didn't know until\
      \ about fifteen minutes ago that they would be preparing a heroes'\
      \ feast--but the food is delicious.  You would be enjoying it\
      \ immensely if you weren't intensely embarrassed for being celebrated\
      \ by these people for no reason that you can understand.\n\n\
      \Nobody does much to explain anything to you--the only thing anyone\
      \ seems to say the whole time is profuse thanks for your help, and\
      \ everybody says it constantly.  You can't get a word in edgewise the\
      \ entire dinner, for which you are grateful--you would have no idea\
      \ what to say to these people who keep insisting that you have saved\
      \ them."
    -- TODO set lighting for evening
    narrate "The feast drags on into the evening, and it grows dark\
      \ outside.  As the older children clean up from dinner, apparently\
      \ cheerfully, a man by the name of David who seems to be the local\
      \ innkeeper insists that you stay the night for free in his best\
      \ room.  It's late, you're tired, and most importantly, it'll give\
      \ you a chance to get away from all these people for a while; so\
      \ you gratefully accept his offer."
    setPartyPosition (Point 22 7) -- TODO cutscene of walking to inn
    narrate "You lock your door for the night, drop your bags on the floor,\
      \ and collapse into bed.  Maybe in the morning this will all turn out\
      \ to be a really weird dream."
    -- TODO set lighting for night; maybe some cricket noises?
    wait 40
    -- TODO add Jessica-ghost townsperson
    let choices = [("\"Why are these people treating us like heroes?\"",
                    return True),
                   ("\"Um, no, we're not confused about anything.  But\
                    \ thanks anyways.\"  (Lie.)", return False)]
    let page1 = join $ forcedChoice "\"My name is Jessica,\" she cheerfully\
          \ repeats.  You wait for her to say more, but she seems to be\
          \ happy to simply stand and smile and wait for you to realize\
          \ that she isn't {i}going{_} to say any more about who--or\
          \ what--she is." choices
    ask <- join $ forcedChoice "You are awoken a few hours past midnight by\
      \ a soft light.  You jump out of bed to see who has come into the\
      \ room, but see that the door is still closed and locked.  Only then\
      \ do you notice the translucent, ghostly figure who has appeared in\
      \ the room with you.  The figure is of a long-haired woman, probably\
      \ in her late thirties.\n\n\
      \She looks at you and smiles.  \"Don't be alarmed,\" she says.  \"My\
      \ name is Jessica.  You did very well yesterday!  Ah, but you seem a\
      \ bit confused by all that has happened; perhaps I can help you?\""
      (("\"Who are you?  {i}What{_} are you?\"", page1) : choices)
    deny <- forcedChoice
      ((if ask then "\"That is an easy question,\" Jessica responds.  "
        else "Jessica gives a delighted, friendly laugh. \
             \ \"Oh, I rather doubt that,\" she says.\n\n") ++
       "\"They're treating you like heroes because you saved their lives. \
       \ If you hadn't been there yesterday, on the path down to this\
       \ village, every man, woman, and child here would now be dead.\"\n\
       \\n\"You do know that much, right?\"")
      [("\"No, we did nothing of the sort.  What are you talking about?\"",
        True),
       ("\"Well, we did kill a revenant that was on its way to attack the\
        \ village, but surely these people could have fought off one\
        \ revenant on their own?\"", False)]
    forcedChoice
      ((if deny then ""
        else "\"Well, of course the town could have fought off {i}one{_}\
             \ revenant,\" Jessica allows, \"a couple of them, even, if\
             \ they were small.\"  She shakes her head and chuckles.  \"But\
             \ that's not what you did!\"  You're, um, pretty sure that\
             \ {i}is{_} what you did, actually.\n\n") ++
       "\"You're just not remembering, are you?\" she sighs.  \"You saved a\
       \ whole village and you don't even remember it.  Tell you what.  I'll\
       \ send you back in time.\"\n\
       \\n\"I'll take you back to yesterday, to when you were walking down\
       \ the path into the village.  Then you can see, again, exactly what\
       \ happened.\"")
      [("\"Wait, what?\"", ()),
       ("\"Is that even possible?  I'm pretty sure it's not.\"", ()),
       ("\"Sounds great.  We'd love to have our memories refreshed.\"", ()),
       ("\"What if we don't {i}want{_} to travel back in time?\"", ())]
    narrate "Jessica completely ignores you, but seems instead to be\
      \ concentrating on something.  She makes a slight hand gesture, and\
      \ then her ghostly figure begins to become more substantial.\n\n\
      \No, wait.  She's not becoming more substantial--the whole rest of the\
      \ room is becoming {i}less{_} substantial.  The walls and furniture\
      \ become translucent like her, and then fade into mist.  The light\
      \ dims until there is nothing but total blackness.  The floor seems to\
      \ drop out from under you; you are in freefall.  You feel cold, and\
      \ hot, at the same time.  Somehow.  And then..."
    writeVar (gTimesThroughLongvale globals) 1
    teleport MountainPath startingPosition

  once 832345 isSecondTimeThroughLongvale $ do
    wait 40
    narrate "Exhausted, you step down through the valley into Corenglen for\
      \ the second time--or maybe it really is the first time.\n\n\
      \The entire town is out lining the streets.  All of them are cheering\
      \ for you.  Children are running towards you with bunches of flowers\
      \ in their arms.  Fathers are still shaking from fear only recently\
      \ abated.  Mothers are holding their babies, tears of joy and thanks\
      \ and relief streaming down their faces.  Everyone is shouting, and\
      \ cheering, and looking at you as though you are their heroes and\
      \ saviors.\n\n\
      \And this time, their treatment of you makes perfect sense--without\
      \ your timely intervention, every one of these people would be\
      \ suffering and dying right now instead of cheering.  What you\
      \ {i}don't{_} understand is, well, {i}anything{_} else about what has\
      \ happened in the past day."
    ok <- forcedChoice "Mayor Faz steps forward to speak to you, water in\
      \ his eyes.  You can barely hear him over the din.  \"I welcome you,\
      \ heroes, to Corenglen; I am Mayor Faz.  We are forever in your debt! \
      \ We...we don't even know who you are yet, but if you had arrived even\
      \ a few minutes later, why...\"  He gets choked up at this point, and\
      \ takes a moment to recover before continuing.\n\n\
      \You feel almost as awkward as you did the last time you met Mayor Faz\
      \ for the first time.  You may have saved this village, but you can't\
      \ help feeling like you don't really deserve his praise.  It's not as\
      \ though you set out to save the town--as far as you can tell, all you\
      \ did was be in the right place, at the right time, at the moment when\
      \ some kind of crazy ghost-woman decided she would send a few idiots\
      \ back in time armed only with epicly magical weapons of unspeakable\
      \ power.\n\
      \\n\"Please,\" the mayor continues, \"come join us at the meeting\
      \ hall.  We are putting together a feast in your honor even as we\
      \ speak.  It's the least we can do.  I beg you to do us the honor of\
      \ staying for a bit.  I'll lead you there now?\""
      [("\"Of course.  We'd be happy to stay for dinner.\"", True),
       ("\"There's, uh, no need to thank us.  It was nothing.\"", False)]
    narrate ((if ok then "\"Thank you, thank you!  Please.  This way.\"  He\
                \ bows and gestures to follow him down the road."
              else "Your show of humility is almost too much for the poor\
                \ man.  \"Nothing!\" he chokes.  \"Perhaps to you, but to\
                \ us, why, we would all be...\"  You hurredly assure him\
                \ that that you will stay after all; you know as well as he\
                \ does what would have happened to this village without your\
                \ help.  After wiping his eyes, he smiles\
                \ and moves to lead you down the road. ") ++
      "\n\n\
      \You fall in line behind the mayor and follow him toward the village\
      \ meeting hall.  As you proceed through the town, the villagers form a\
      \ sort of parade behind you.  They continue to cry and cheer.  Some of\
      \ the children who managed to find some confetti are now throwing\
      \ it.\n\n\
      \You're not really sure what to do now.  Maybe if you sit through\
      \ dinner and stay at the inn again, this Jessica person will appear\
      \ again and explain what is going on.")
    -- TODO parade and dinner scene
    setPartyPosition (Point 22 7) -- TODO cutscene of walking to inn
    narrate "You lock your door for the night, place your bags on the floor,\
      \ and get carefully into bed.  Despite your fatigue, you try to keep\
      \ yousevles awake, watching for the Jessica-ghost to appear again. \
      \ You lie there, and you wait.\n\n\
      \And you wait."
    -- TODO set lighting for night; maybe some cricket noises?
    wait 40
    -- TODO add Jessica-ghost townsperson
    snark <- forcedChoice "...and the next thing you know, you are awoken\
      \ from your inevitable slumber by a soft light.  You sit up and look\
      \ to see the translucent figure of Jessica standing in the middle of\
      \ the room.\n\
      \\n\"Greetings!\" she says. \"I am Jessica.  You remember me, I\
      \ hope?\""
      [("\"How could we forget?\"", False),
       ("\"Well...I don't think we've met you on any day before this,\
        \ right?  So no, I suppose we don't remember you.\"", True)]
    forcedConversationLoop
      ((if snark then "Jessica rolls her eyes, but manages (with effort) to\
                      \ maintain her smile.  \"Yes yes, how very clever of\
                      \ you.\""
        else "Jessica closes her eyes briefly and grins.  \"Splendid!  Glad\
             \ to hear there was no, er, permenant damage to you.\"") ++
       "\n\n\"You did very well yesterday!  The townspeople and I are both\
       \ impressed and grateful.  And I hope you are feeling less confused\
       \ this time around?\"  Your looks of total exasperation quickly\
       \ disabuse her of this notion.  \"Oh dear.  Well, what's still\
       \ confusing you?\"")
      [("\"How could we possibly have gone back in time?\"",
        ContinueConv "\"Oh, because I sent you back!  Last time we met here\
          \ today.  It's a trick I learned, once.\"  Wonderful.  A complete\
          \ non-answer." []),
       ("\"Are you some kind of ghost?\"",
        ContinueConv "Jessica just shrugs." []),
       ("\"Why were there more revenants this time?\"",
        ContinueConv "\"I'm sure I have no idea what you're talking about. \
          \ You went back in time and saw the exact same events unfold\
          \ twice.\"  She leans in, and says in a slightly condescending\
          \ voice,  \"That is what happens when you go back in time,\
          \ dears.\"\n\n\
          \You're pretty sure that she knows perfectly well that the battle\
          \ was different the second time around, but evidentally she's not\
          \ going to answer your question." []),
       ("\"Where did all those revenants come from?\"",
        ContinueConv "\"An excellent question.  I do hope you'll find out\
          \ for me.\"  Interesting." []),
       ("\"Where did these magical weapons come from?\"",
        ContinueConv "\"Why, don't you remember?\"  Jessica looks mildly\
          \ shocked.  \"Those are the Astral Weapons, which you gathered,\
          \ painstakingly, on your previous quests!  The four great quests\
          \ you embarked on before coming here.\"\n\n\
          \Huh?  You've never even been on any quests--you've only just\
          \ started adventuring.  Even if you had, no quest leading to an\
          \ artifact like one of these would be achievable by novices like\
          \ you.  It's unthinkable."
          [("\"That's crazy talk.  We've never been on any quests, and\
            \ we've certainly never seen these weapons before.\"",
            StopConv ())]),
       ("\"Where did {i}you{_} come from?\"",
        ContinueConv "\"I'm from here, actually.  I grew up in this village,\
          \ although I've travelled quite a bit since then.\"  Jessica\
          \ folds her hands primly in front of her and continues to smile. \
          \ Apparently that's all the answer you're getting." []),
       ("\"Pretty much everything, actually.\"",
        ContinueConv "\"Ah.  Could you be a little more specific?  I'm not\
          \ sure I have time to explain 'everything.'  Like how to walk, or\
          \ bathe.\"" [])]
    forcedChoice "\"You're still not remembering, are you?  Not weeks ago,\
      \ you defeated the most powerful lich in the world, and you're telling\
      \ me you don't remember it?\"  Say {i}what{_} now?  Lich?  You've\
      \ {i}definitely{_} never fought a lich.  You know this, because you're\
      \ still alive.\n\n\
      \She thinks for a bit.  \"Tell you what.\" she says.  \"Why don't I\
      \ send you back in time again, to the beginning of that most recent\
      \ quest?  I'm sure that will jog your memories.\""
      [("\"Er, I don't think that will be necessary.\"", ()),
       ("\"Sure, why not?  A quest sounds like fun.  Or, wait, does that\
        \ mean we have to get killed by a lich?\"", ()),
       ("\"Stay away from us, you crazy ghost lady!\"", ())]
    narrate "You're pretty sure that Jessica didn't hear you--or even wait\
      \ for you to answer--because the room around you is already fading\
      \ away to blackness.  There is a deafening silence, so loud you almost\
      \ can't stand it, but you find yourselves unable to speak.  You\
      \ realize you have been flipped upside down--or maybe the rest of the\
      \ world has been--because you are falling headlong, somewhere.  And\
      \ then..."
    teleport FrozenPass svengaardEntryPosition

-------------------------------------------------------------------------------
