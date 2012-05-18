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

module Fallback.Scenario.Triggers
  (startingArea, startingPosition, scenarioTriggers, initialProgress,
   getAreaDevice, getAreaEntrance, getAreaExits, getAreaLinks, getAreaTerrain,
   getAreaTriggers, getRegionBackground)
where

import Control.Applicative ((<$>))
import Control.Monad (forM, join, zipWithM_)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.FrozenPass (compileFrozenPass)
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Icehold (compileIcehold)
import Fallback.Scenario.Triggers.IronMine (compileIronMine)
import Fallback.Scenario.Triggers.Script
import Fallback.Scenario.Triggers.SewerCaves (compileSewerCaves)
import Fallback.Scenario.Triggers.StoneBridge (compileStoneBridge)
import Fallback.Scenario.Triggers.Tragorda (compileTragorda)
import Fallback.State.Area --(arsGetCharacter)
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Party (chrClass)
import Fallback.State.Progress (Progress)
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Tags
import Fallback.Utility (firstJust, flip3, maybeM)

-------------------------------------------------------------------------------

startingArea :: AreaTag
startingArea = MountainPath

startingPosition :: Position
startingPosition = Point 14 6

initialProgress :: Progress
initialProgress = scenarioInitialProgress scenarioTriggers

-------------------------------------------------------------------------------

scenarioTriggers :: ScenarioTriggers
scenarioTriggers = compileScenario $ do

  globals <- compileGlobals

  ----------------------------- Global Variables ------------------------------

  -- Longvale:
  timewarpToBeginningFirstTime <- newGlobalVar 970844 False
  timewarpToBeginningLastTime <- newGlobalVar 561796 False
  let isFirstTimeThroughLongvale =
        varFalse timewarpToBeginningFirstTime `andP`
        varFalse timewarpToBeginningLastTime
  let isSecondTimeThroughLongvale =
        varTrue timewarpToBeginningFirstTime `andP`
        varFalse timewarpToBeginningLastTime
  let isLastTimeThroughLongvale = varTrue timewarpToBeginningLastTime

  -- Svengaard:
  let svengaardEntryPosition = Point 5 9
  --rescuedSophia <- newGlobalVar 953743 False

  -- Tahariam:
  demonPitOpen <- newGlobalVar 928347 False

  ---------------------------------- Regions ----------------------------------

  compileRegion Longvale (const "regions/Longvale.png")
  compileRegion Svengaard (const "regions/Svengaard.png")
  compileRegion Tahariam $ \party -> if getVar demonPitOpen party
                                     then "regions/Tahariam2.png"
                                     else "regions/Tahariam1.png"
  compileRegion Bailagua (const "regions/Bailagua.png")
  compileRegion Emitsuibom (const "regions/Emitsuibom.png")
  -- OtherFloors is a dummy region containing e.g. 2nd floors of areas from
  -- other regions.  The background image doesn't really matter.
  compileRegion OtherFloors (const $ "regions/Longvale.png")

  ----------------------------------- Areas -----------------------------------

  compileArea Valhalla Nothing $ do

    onStartDaily 409487 $ do
      addDevice_ (gStoneDoor globals) (Point 8 2)
      addDevice_ (gStoneDoor globals) (Point 12 4)
      addDevice_ (gStoneDoor globals) (Point 3 6)
      addDevice_ (gStoneDoor globals) (Point 4 6)
      addDevice_ (gStoneDoor globals) (Point 10 6)
      addDevice_ (gStoneDoor globals) (Point 2 9)
      addDevice_ (gStoneDoor globals) (Point 10 9)
      addDevice_ (gStoneDoor globals) (Point 4 11)
      addDevice_ (gStoneDoor globals) (Point 7 11)
      addDevice_ (gStoneDoor globals) (Point 5 13)
      addDevice_ (gStoneDoor globals) (Point 10 13)
      addDevice_ (gBasaltDoor globals) (Point 32 17)

    simpleMonster 660632 DemonWolf (Point 29 2) MindlessAI
    simpleMonster 660633 DemonWolf (Point 6 20) ChaseAI
    simpleMonster 660634 Wolf (Point 27 16)
                  (PatrolAI (Point 27 16) (Point 36 16))

  compileArea MountainPath Nothing $ do

    makeExit Corenglen [Rect 38 38 2 8, Rect 31 44 7 2] (Point 36 42)

    trigger 908784 (walkIn (Rect 0 0 9 2) `orP` walkIn (Rect 0 0 3 9)) $ do
      whenP isFirstTimeThroughLongvale $ do
        narrate "You look back up the mountain, towards the way you came\
          \ from.  Then, you turn around and head back down.  There's nothing\
          \ of interest on the path that led you here, but you feel sure,\
          \ somehow, that adventure awaits you down in Coringlen."
      whenP isSecondTimeThroughLongvale $ do
        narrate "You're still not sure what's going on, but you doubt you're\
          \ going to find any answers by heading back the way you came.  You'd\
          \ better continue on into Corenglen.  Again."
      whenP isLastTimeThroughLongvale $ do
        narrate "Just when you thought everything made sense, nothing makes\
          \ sense anymore.  You have to end this, and that means returning to\
          \ Corenglen one more time.  Squaring your shoulders, you turn to\
          \ descend through the valley."
      pos <- getPartyPosition
      partyWalkTo (pos `plusDir` DirSE)

    once 085585 (isFirstTimeThroughLongvale `andP`
                 walkOff startingPosition) $ do
      narrate "You're finally here!  A brand new band of adventures, on a\
        \ journey to seek out your very first quest.  Without a doubt, the\
        \ days ahead will be filled with countless monsters, and hopefully,\
        \ even more countless treasures.\n\n\
        \After buying supplies and several days of hiking over the mountains,\
        \ you find yourselves descending through a narrow valley towards the\
        \ village of Corenglen.  The air smells fresh and the sun is just\
        \ beginning to set over the western peaks.\n\n\
        \You know very little about this village, but it's a remote town in\
        \ dangerous territory; surely {i}they'll{_} have some quests for you!"

    once 741589 (isSecondTimeThroughLongvale `andP`
                 walkOff startingPosition) $ do
      narrate "You blink.  Your head feels fuzzy.  Looking around, you find\
        \ yourselves back on the mountain path leading down into Corenglen. \
        \ The sun is just beginning to set over the peaks to the west, just as\
        \ it was when you were hiking into Corenglen before.\n\n\
        \This is bizarre.  Could you really have gone back in time by half a\
        \ day?  Surely that's impossible--you've never heard of such magic. \
        \ But the only way you can think of to find out for sure is to head\
        \ down towards Corenglen again and see if that revenant shows up.  And\
        \ if the town throws another party for you, you're going to be asking\
        \ a lot more questions this time."

    loneRevenantDead <- newPersistentVar 960522 False
    once 582362 (isFirstTimeThroughLongvale `andP`
                 (walkIn (Rect 19 25 15 1) `orP`
                  walkIn (Rect 19 14 1 11))) $ do
      narrate "As you are walking down the valley, you suddenly hear a noise\
        \ and turn to see a small rock bouncing down the path from above. \
        \ Skip, skip, skip.  Someone or something else is up there, behind\
        \ you.  Then you hear an unearthly shriek.\n\n\
        \You freeze.  You know what that shriek is; you've heard it before. \
        \ That's the sound of a revenant: a dangerous undead creature of pure\
        \ magic, usually made by a wizard.  Usually an evil wizard.  They are\
        \ quite nasty.\n\n\
        \Presently you see it hurtle around the bend above you, making a\
        \ beeline down to the town below and screaming all the way.  It\
        \ doesn't look like a terribly powerful specimen, but still big enough\
        \ that it would probably kill a few townspeople if you didn't stop it,\
        \ even given that they can probably already hear it coming.  You ready\
        \ your weapons and move to attack it.  You just hope it won't kill a\
        \ few of {i}you{_}..."
      addBasicEnemyMonster (Point 33 13) Revenant (Just loneRevenantDead)
                           ChaseAI
    once 520543 (isFirstTimeThroughLongvale `andP`
                 varTrue loneRevenantDead) $ do
      wait 40
      narrate "Whew...one less horror in the world.  Looks like the village\
        \ below is safe for now.  Not that they wouldn't have been mostly fine\
        \ without you--their militia, unlike you, have probably actually\
        \ fought off monsters before--but still, you may have saved a life or\
        \ two.  Maybe they'll give you free drinks at the pub or\
        \ something.\n\n\
        \Now, time to get back to that adventure you've been meaning to get\
        \ started on."

    once 309037 (isSecondTimeThroughLongvale `andP`
                 (walkIn (Rect 19 25 15 1) `orP`
                  walkIn (Rect 19 14 1 11))) $ do
      narrate "As you are walking down the valley, you suddenly hear a noise\
        \ and turn to see a small rock bouncing down the path from above. \
        \ Skip, skip, skip.  You frown.  You could have sworn that that exact\
        \ same rock bounced in the {i}exact{_} same way when the revenant\
        \ attacked you yesterday.  In fact, everything here been the same as\
        \ it was yesterday, from the look of the sunset to the smell of the\
        \ air.  This can't all be a coincidence.  It seems that this really\
        \ {i}is{_} yesterday?\n\n\
        \You wince preemptively for the shrill cry of the revenant that you\
        \ know is about to appear.  But suddenly, you are nearly deafened by\
        \ what sounds like hundreds of unearthly screams, as though the gates\
        \ of hell themselves had been thrown open on the path above you."
      narrate "The blood drains from your faces as a huge group of revenants\
        \ stampede around the bend above.  There must be dozens of them, the\
        \ weakest of which puts the one you fought before to shame.  The\
        \ leader of the pack is twenty feet tall and blazing with sickly-blue\
        \ light; it looks like it could kill a dozen soldiers just by\
        \ coughing.\n\n\
        \You've heard tales of this before--packs of revenants attacking\
        \ villages, wreaking horrible deaths upon them and leaving no\
        \ survivors--though you've never heard of any so large as this one,\
        \ and anyway you didn't think the tales were true.  You feel sick as\
        \ you realize that the town below can almost certainly already see and\
        \ hear these revenants coming from afar, but will be unable to escape\
        \ in time, walled in as they are by the valley.  They will know that\
        \ they are doomed, but will be able to do little more than cower in\
        \ absolute terror for the next ten minutes or so, waiting for their\
        \ inevitable deaths.\n\n\
        \Then you feel even sicker as you realize, also, that you are right in\
        \ between the pack and the village, and the valley walls offer you no\
        \ escape."
      -- TODO create revenant pack, give astral weapons to party
      numsAndClasses <- forM [minBound .. maxBound] $ \charNum -> do
        (,) charNum . chrClass <$> areaGet (arsGetCharacter charNum)
      flip State.evalStateT (Map.fromList numsAndClasses,
                             Set.fromList [Sunrod, Starspear, Moonbow,
                                           Lifeblade]) $ do
        let tryAssign (item, cls) = do
              (chars, items) <- State.get
              if Set.notMember item items then return () else do
              let match (n, c) = if c == cls then Just n else Nothing
              maybeM (firstJust match $ Map.assocs chars) $ \charNum -> do
                State.lift $ grantAndEquipWeapon item charNum
                State.put (Map.delete charNum chars, Set.delete item items)
        mapM_ tryAssign [(Sunrod, MagusClass),
                         (Moonbow, HunterClass),
                         (Lifeblade, WarriorClass),
                         (Starspear, AlchemistClass),
                         (Sunrod, ClericClass),
                         (Sunrod, AlchemistClass),
                         (Moonbow, RogueClass),
                         (Moonbow, MagusClass),
                         (Moonbow, ClericClass),
                         (Sunrod, HunterClass),
                         (Lifeblade, RogueClass),
                         (Sunrod, RogueClass),
                         (Starspear, WarriorClass),
                         (Starspear, RogueClass),
                         (Lifeblade, ClericClass),
                         (Moonbow, AlchemistClass),
                         (Starspear, MagusClass)]
        (chars, items) <- State.get
        flip3 zipWithM_ (Map.keys chars) (Set.elems items) $ \charNum tag -> do
          State.lift $ grantAndEquipWeapon tag charNum
      narrate "You reach for your weapons, knowing that you don't stand a\
        \ snowball's chance in hell against these monsters.  Maybe you can at\
        \ least slow them down enough for some of the villagers to escape? \
        \ No, that's a joke.  You're all going to be dead in about thirty\
        \ seconds, tops.  Still, you have no choice but to try."
      -- TODO start combat (makes "schwing!" noise) before showing this next
      -- message.
      narrate "But when you pull out your weapons, you are shocked to find not\
        \ the meager armaments that you were barely able to afford when\
        \ packing for your journey out here, but four powerful magical\
        \ artifacts--a sword, a bow, a spear, and a wand--the likes of which\
        \ you have never seen, or even {i}heard{_} of.  These are no ordinary\
        \ enchanted weapons, like the sort that veteran adventurers are always\
        \ bragging about winning from daring quests.  You can actually\
        \ {i}feel{_} the power flowing through you the moment you lay your\
        \ hands on them.\n\n\
        \Each of the four bears a different inscription:\n\n\
        \    {c}Lifeblade, the fount of strength and light.{_}\n\n\
        \    {c}Moonbow, the master of the night.{_}\n\n\
        \    {c}Starspear, the guard that hell hath dread.{_}\n\n\
        \    {c}Sunrod, the bane of all undead.{_}\n\n\
        \Whoa.  With weapons like these...you might actually survive this.\n\n\
        \But where could they have come from?"
      startMusic MusicMovementProposition

  compileArea Corenglen Nothing $ do

    makeExit MountainPath [Rect 0 10 2 12] (Point 2 16)

    alwaysLockedDoor <- newDevice 963970 1 $ \_ _ -> do
      setMessage "The door is locked."

    onStartDaily 820304 $ do
      -- Doors in inn:
      addDevice_ (gAdobeDoor globals) (Point 19 10)
      addDevice_ (gAdobeDoor globals) (Point 19 12)
      addDevice_ (gAdobeDoor globals) (Point 23 10)
      addDevice_ alwaysLockedDoor (Point 23 12)
      addDevice_ (gAdobeDoor globals) (Point 26 11)
      addDevice_ (gAdobeDoor globals) (Point 30 13)
      -- Doors on houses:
      addDevice_ alwaysLockedDoor (Point 12 13)
      addDevice_ alwaysLockedDoor (Point 38 9)
      addDevice_ alwaysLockedDoor (Point 38 15)
      addDevice_ alwaysLockedDoor (Point 42 12)
      addDevice_ alwaysLockedDoor (Point 44 18)
      -- Doors on other buildings:
      addDevice_ alwaysLockedDoor (Point 14 26)
      addDevice_ alwaysLockedDoor (Point 38 29)

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
      writeVar timewarpToBeginningFirstTime True
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

  compileFrozenPass globals

  compileArea Holmgare Nothing $ do

    makeExit FrozenPass [Rect 0 0 2 13, Rect 2 0 5 2] (Point 3 7)
    makeExit SewerCaves [Rect 33 0 6 2] (Point 36 3)
    makeExit PerilousRoad [Rect 53 17 2 17] (Point 51 25)

    onStartDaily 472927 $ do
      setAreaCleared Holmgare True
      addDevice_ (gAdobeDoor globals) (Point 16 13)
      addDevice_ (gAdobeDoor globals) (Point 17 23)
      addDevice_ (gAdobeDoor globals) (Point 19 14)
      addDevice_ (gAdobeDoor globals) (Point 22 21)
      addDevice_ (gAdobeDoor globals) (Point 36 23)
      addDevice_ (gAdobeDoor globals) (Point 38 20)
      addDevice_ (gStoneDoor globals) (Point 24 26)
      addDevice_ (gStoneDoor globals) (Point 26 11)
      addDevice_ (gStoneDoor globals) (Point 28  9)
      addDevice_ (gStoneDoor globals) (Point 28 24)
      addDevice_ (gStoneDoor globals) (Point 31 14)
      addDevice_ (gStoneDoor globals) (Point 34 13)
      addDevice_ (gStoneDoor globals) (Point 36  8)
    simpleTownsperson 217809 TownManApron (Point 28 27) ImmobileAI $ \_ -> do
      let page1 = multiChoice
            "The blacksmith wipes his brow and sets down his tongs.  \"The\
            \ name's Gregor.  What can I do for you?\""
            [("\"Tell us about your smithy.\"", join page2),
             ("\"What have you got for sale?\"", join page3),
             ("\"What's been going on in this village?\"", join page4),
             ("\"I think we're all set.\"  (Leave.)", return ())]
            (return ())
          page2 = multiChoice
            "Otay.\""
            [("\"Whatever wrod!\"", return ())]
            (return ())
          page3 = multiChoice
            "He frowns.  \"Not a lot right now, to be honest,\" he says, with\
            \ apparent regret.  \"I can do repair work, and I can show you\
            \ what little I've got in stock.  But I'm short on raw materials,\
            \ and until I can get more I'm not going to be able to do any\
            \ commission work."
            [("\"Well, let's see what you have on hand.\"",
              return ()),
             ("\"You're short on raw materials?  Is that something we could\
              \ help with?\"", return ()),
             ("\"I wanted to ask you about something else.\"", join page1)]
            (return ())
          page4 = multiChoice
            "\"Otay.\""
            [("\"Whatever wrod!\"", return ())]
            (return ())
      join page1
    simpleTownsperson 092833 TownWomanBlue (Point 15 15)
                      ImmobileAI $ \_ge -> do
      narrate "Oh hai."
    simpleTownsperson 711833 TownManRed (Point 19 11) ImmobileAI $ \_ge -> do
      narrate "Hi, folks!"
      number <- forcedChoice "Please pick a number."
                  [("\"One!\"", 1), ("\"Two?\"", 2), ("\"Wait, I'm confused.  Is this a trick question?  Can I get a hint?  I don't know what to do!  I want my mommy!  Um, sorry...I meant 'three.'\"", 3),
                  ("(Just walk away.)", 4), ("\"Oh, yeah, we totally picked five.\"  (Lie.)", 5)]
      narrate $ "You chose " ++ show (number :: Int) ++ ", I guess."
    return ()

  compileSewerCaves globals

  compileArea PerilousRoad Nothing $ do
    makeExit Holmgare [Rect 0 4 2 5] (Point 3 6)
    makeExit StoneBridge [Rect 13 4 2 5] (Point 11 6)
    makeExit IcyConfluence [Rect 8 0 5 2] (Point 10 3)

  compileStoneBridge globals
  compileTragorda globals

  compileArea WhistlingWoods Nothing $ do
    makeExit IcyConfluence [Rect 0 3 2 5] (Point 3 5)
    makeExit Tragorda [Rect 5 10 5 2] (Point 7 8)
    makeExit Duskwood [Rect 13 3 2 5] (Point 11 5)

  compileArea IcyConfluence Nothing $ do
    makeExit PerilousRoad [Rect 0 3 2 6] (Point 3 6)
    makeExit Marata [Rect 4 0 7 2] (Point 7 3)
    makeExit WhistlingWoods [Rect 13 3 2 6] (Point 11 6)

  compileArea Marata Nothing $ do
    makeExit IcyConfluence [Rect 9 42 8 2] (Point 11 40)
    makeExit IronMine [Rect 37 0 9 2] (Point 41 3)
    makeExit NorthernTundra [Rect 53 11 2 12] (Point 51 16)

  compileIronMine globals

  compileArea NorthernTundra Nothing $ do
    makeExit Marata [Rect 0 3 2 5] (Point 3 5)
    makeExit Duskwood [Rect 13 3 2 5] (Point 11 5)

  compileArea Duskwood Nothing $ do
    makeExit WhistlingWoods [Rect 0 3 2 5] (Point 3 5)
    makeExit Icehold [Rect 13 3 2 5] (Point 11 5)
    makeExit NorthernTundra [Rect 5 0 5 2] (Point 7 3)
    makeExit Tragorda [Rect 5 10 5 2] (Point 7 8)

  compileIcehold globals

  compileArea BurningMaze Nothing $ return ()

  compileArea Gazerpit Nothing $ return ()

  compileArea ArcaneLab Nothing $ return ()
  compileArea InnerLab Nothing $ return ()
{-
    fooCount <- newPersistentVar 238238 0
    trigger 119323 (walkIn (Rect 0 0 5 5)) $ do
      foo <- readVar fooCount
      when (foo < 5) $ do
        modifyVar fooCount (+ 1)
        narrate "You can't go there."
      backUp

    boss <- monster' 729382 MasterRevenant (Point 4 6)

    daily 203942 (walkIn (Rect)) $ do
      monsterWalk boss (Point)

    bossFight <- scriptedCombat 827349 $ do
      once 193822 ((<= 500) . monstHealth <$> getMonster boss) $ do
        pos <- monsterPosition boss
        monologue pos "You know, there's really no reason for us to fight\
                      \ like this.  We should be able to work this out\
                      \ reasonably, like adults, don't you think?"
        wait 30
        monologue pos "...after you're dead, of course."
        orderMonsterUseAttack 4

    once 203942 (walkIn (Rect)) $ do
      narrate "Boss time!"
      startCombat bossFight
-}

-------------------------------------------------------------------------------

-- 400278, 372710, 262175, 115489, 648882, 642527, 643253, 035698, 904223,
-- 915362, 041045, 514224, 762406, 999849, 390882, 028595, 542093

-------------------------------------------------------------------------------
