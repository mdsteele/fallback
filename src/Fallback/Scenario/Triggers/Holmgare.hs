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

module Fallback.Scenario.Triggers.Holmgare
  (compileHolmgare)
where

import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Tags

-------------------------------------------------------------------------------

compileHolmgare :: Globals -> CompileScenario ()
compileHolmgare globals = compileArea Holmgare Nothing $ do

  makeExit FrozenPass ["ToFrozenPass1", "ToFrozenPass2"] "FromFrozenPass"
  makeExit SewerCaves ["ToSewerCaves"] "FromSewerCaves"
  makeExit PerilousRoad ["ToPerilousRoad"] "FromPerilousRoad"

  onStartDaily 472927 $ do
    addUnlockedDoors globals
    setAreaCleared Holmgare True

  simpleTownsperson 217809 TownManApron "Gregor"
                    ImmobileAI $ \_ -> conversation $ do
    let
      aboutSmithy = convNode $ do convText "FIXME"
      whatIsNews = convNode $ do convText "FIXME"
      whatForSale = convNode $ do
        convText "He frowns.  \"Not a lot right now, to be honest,\" he says,\
          \ with apparent regret.  \"I can do repair work, and I can show you\
          \ what little I've got in stock.  But I'm short on raw materials,\
          \ and until I can get more I'm not going to be able to do any\
          \ commission work."
        convChoice canWeHelp "\"You're short on raw materials?  Is that\
          \ something we could help with?\""
        convChoice doShop "\"Well, let's see what you have on hand.\""
      doShop = convNode $ do
        startShopping $ map Right $
          [WeaponItemTag Dagger, WeaponItemTag Shortsword,
           WeaponItemTag Voulge, ArmorItemTag IronMail]
        convText "You conclude your business.  Gregor grunts and turns back to\
          \ his anvil."
      canWeHelp = convNode $ do convText "FIXME"
    convNode $ do
      convText "The blacksmith wipes his brow and sets down his tongs.  \"The\
        \ name's Gregor.  What can I do for you?\""
      convChoice (return ()) "\"I think we're all set.\"  (Leave.)"
      convChoice whatIsNews "\"What's been going on in this village?\""
      convChoice whatForSale "\"What have you got for sale?\""
      convChoice aboutSmithy "\"Tell us about your smithy.\""

  simpleTownsperson 720981 TownWomanRed "Laci"
                    (DrunkAI "LaciZone") $ \_ -> conversation $ do
    let
      whatsWrong = convNode $ convText "FIXME"
      waitForHer = convNode $ convText "FIXME"
--       whereParents = convNode $ convText "FIXME"
--       whatYouDoing = convNode $ convText "FIXME"
--       buyMushrooms = convNode $ do
--         startShopping [Right (PotionItemTag Mushroom)]
--         convText "Laci packs the rest of the mushrooms she's collected back\
--           \ into her bag, and then wipes a stray tear from her check.  She\
--           \ seems to be doing a little better now, at least for the moment."
    convNode $ do
      convText ""
      convChoice (return ()) "\"We need to be going.\"  (Leave.)"
      convChoice waitForHer "(Wait for her to be ready.)"
      convChoice whatsWrong "\"Why are you crying?\""

  simpleTownsperson 711833 TownManRed "Dorvan" ImmobileAI $ \_ge -> do
    narrate "TODO"

  simpleTownsperson 092833 TownWomanBlue "Eithne"
                    ImmobileAI $ \_ -> conversation $ do
    narrate "TODO"

  simpleTownsperson 209831 GuardWoman "Reta"
                    ImmobileAI $ \_ -> conversation $ do
    narrate "TODO"

  simpleTownsperson 502809 GuardSmallShield "Pavel"
                    (PatrolAI "Pavel" "PavelPatrol") $ \_ -> conversation $ do
    narrate "TODO"

  simpleTownsperson 470982 TownWomanApron "Marunda"
                    (DrunkAI "MarundaZone") $ \_ -> conversation $ do
    narrate "TODO"

  simpleTownsperson 309810 TownManYellow "Zivon"
                    ImmobileAI $ \_ -> conversation $ do
    narrate "TODO"

  simpleTownsperson 509834 TownChildBlue "Tistra"
                    (DrunkAI "TistraZone") $ \_ -> conversation $ do
    narrate "TODO"

-------------------------------------------------------------------------------
