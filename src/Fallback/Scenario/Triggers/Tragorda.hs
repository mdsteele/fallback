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

import Control.Monad (when)

import qualified Fallback.Data.TotalMap as TM (get)
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Area (arsParty)
import Fallback.State.Creature (MonsterTownAI(DrunkAI))
import Fallback.State.Party (partyIngredients)
import Fallback.State.Simple (Ingredient(..), QuestStatus(..))
import Fallback.State.Tags

-------------------------------------------------------------------------------

compileTragorda :: Globals -> CompileScenario ()
compileTragorda globals = compileArea Tragorda Nothing $ do

  makeExit StoneBridge ["ToStoneBridge"] "FromStoneBridge"
  makeExit WhistlingWoods ["ToWhistlingWoods"] "FromWhistlingWoods"
  makeExit Duskwood ["ToDuskwood"] "FromDuskwood"

  onStartDaily 269446 $ do
    addUnlockedDoors globals
    setAreaCleared Tragorda True

  uniqueDevice 277292 "TavernSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}THE WELCOME TRAVELER{_}"

  uniqueDevice 915293 "ApothecarySign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}APOTHECARY{_}"
  simpleTownsperson 109230 TownWomanApron "Lucca"
                    (DrunkAI "LuccaZone") $ \_ -> conversation $ do
    convText "Well, hello!"  -- TODO
    let
      initialChoices = convNode $ do
        convChoice (return ()) "\"We're all set, thank you.\"  (Leave.)"
        whenP (questActive DryIceForLucca) $ do
          ings <- areaGet (partyIngredients . arsParty)
          when (TM.get DryIce ings > 0) $ do
            convChoice giveIce "\"We found some dry ice for you.\"  (Give.)"
        convChoice whereFrom "\"Where do your ingredients come from?\""
        convChoice doShop "\"Let's see what you've got for sale.  (Shop.)\""
      doShop = convNode $ do
        startShopping $
          map (Right . PotionItemTag)
              [HealingTincture, HealingPotion, ManaPhilter, Antidote] ++
          map Left [AquaVitae .. Brimstone]
        convText "You conclude your business.  \"Always glad to be of help to\
          \ an adventuring group,\" Lucca says with a little smile.  \"I used\
          \ to be an adventurer myself, until I took...well, that's a story\
          \ for another time, perhaps.\""
      whereFrom = convNode $ do
        convText "\"Hah, nice try!\" she laughs.  \"If I told you that, you'd\
          \ just go get them yourselves instead of coming here to buy them\
          \ from little old me!  What would I do then?\"  She gives you a\
          \ little wink."
        whenP (questActive DryIceForLucca) $ do
          convText "\n\n\"I'll admit that I'm still hoping you'll bring me\
            \ some dry ice, though.  At least, if you ever find any.\""
        whenP (questUntaken DryIceForLucca) $ do
          convText "\n\n\"Although you know, now that I say that, I wonder if\
            \ maybe you could help me out.  I've got this recipe I've been\
            \ working on for months, but I need some dry ice to finish it, and\
            \ I haven't got a bit of it.  Why don't you go fetch some for me,\
            \ and if the recipe works out you can have some of the results?\""
          setQuestStatus DryIceForLucca QuestActive
        whenP (questActive DryIceForLucca) $ do
          convChoice whereIsIce "\"Where can we find dry ice?\""
          convChoice howMuchIce "\"How much dry ice do you need?\""
      howMuchIce = convNode $ do
        convText "\"Oh, I hardly need any, that's the most frustrating part. \
          \ A single piece would do.  But I haven't got a single bit of it\
          \ right now, not even a speck.  It's difficult stuff to come by.\""
      whereIsIce = convNode $ do
        convText "\"I have no idea, dearies.  If I knew, I would go get it\
          \ myself and spare myself from having to share the potion with\
          \ you.\"  She brightens a bit.  \"Then I could sell it to you\
          \ instead!  Then everyone would be happy!\"\n\
          \\n\"But no, no idea where to find any around here; just let me know\
          \ if you do.\""
      giveIce = convNode $ do
        convText "\"Thankee!\"" -- TODO
        -- TODO take one unit DryIce; give XP/reward
        setQuestStatus DryIceForLucca QuestSucceeded
    initialChoices

  uniqueDevice 642104 "JewelerSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \          {b}Bisla Gruer Fine Jewelry{_}\n\
      \      {i}Ornaments magical and mundane.{_}\n\
      \                           {i}Est. 1136{_}\n\
      \                   {i}A family business.{_}"
  simpleTownsperson 020394 TownManBlue "Bisla"
                    (DrunkAI "BislaZone") $ \_ -> conversation $ do
    narrate "Oh hai." -- FIXME
    startShopping $ map (Right . AccessoryItemTag) $
      [GroundedCharm, IcyNecklace, JeweledPin, ShieldRing, WizardsRing]

  uniqueDevice 884670 "MarketSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}TRAGORDA MARKETPLACE{_}"
  uniqueDevice 285872 "CityHallSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}TRAGORDA CITY HALL{_}"
  uniqueDevice 320769 "ClerkSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}OFFICE OF THE CLERK{_}"
  uniqueDevice 157924 "MinersSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}SVENGAARD MINERS' UNION{_}"
  uniqueDevice 802766 "ChurchSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {i}CHURCH OF THE RESCUED SOUL{_}"
  uniqueDevice 705513 "SageSign" signRadius $ \_ _ -> do
    narrate "The sign tacked to the wall reads:\n\n\
      \      {i}Sage Bora: Scholar and Artificer{_}"
  uniqueDevice 912708 "LibrarySign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}VECHAR MEMORIAL LIBRARY{_}"

-------------------------------------------------------------------------------
