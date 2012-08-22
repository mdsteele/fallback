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
  (startingArea, startingMark, scenarioTriggers, initialProgress,
   getAreaDevice, getAreaEntrance, getAreaExits, getAreaLinks, getAreaTerrain,
   getAreaTriggers, getMonsterScript, getRegionBackground,
   ScriptedBattle(..), getScriptedBattle)
where

import Control.Monad (join)

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Corenglen (compileCorenglen)
import Fallback.Scenario.Triggers.FrozenPass (compileFrozenPass)
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Icehold (compileIcehold)
import Fallback.Scenario.Triggers.IcyConfluence (compileIcyConfluence)
import Fallback.Scenario.Triggers.IronMine (compileIronMine)
import Fallback.Scenario.Triggers.MountainPath (compileMountainPath)
import Fallback.Scenario.Triggers.Script
import Fallback.Scenario.Triggers.SewerCaves (compileSewerCaves)
import Fallback.Scenario.Triggers.StoneBridge (compileStoneBridge)
import Fallback.Scenario.Triggers.Tragorda (compileTragorda)
import Fallback.Scenario.Triggers.WhistlingWoods (compileWhistlingWoods)
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Progress (Progress)
import Fallback.State.Tags

-------------------------------------------------------------------------------

startingArea :: AreaTag
startingArea = MountainPath

startingMark :: String
startingMark = "Start"

initialProgress :: Progress
initialProgress = scenarioInitialProgress scenarioTriggers

-------------------------------------------------------------------------------

scenarioTriggers :: ScenarioTriggers
scenarioTriggers = compileScenario $ do

  globals <- compileGlobals

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
      addUnlockedDoors globals
    simpleEnemy_ 660632 "DemonWolf1" DaemonWolf MindlessAI
    simpleEnemy_ 660633 "DemonWolf2" DaemonWolf ChaseAI
    simpleEnemy_ 660634 "Wolf" Wolf (PatrolAI (Point 27 16) (Point 36 16))
    simpleEnemy_ 978292 "Bat" CaveBat (GuardAI 4 "Bat")

  compileMountainPath globals
  compileCorenglen globals

  compileFrozenPass globals

  compileArea Holmgare Nothing $ do

    makeExit FrozenPass ["ToFrozenPass1", "ToFrozenPass2"] "FromFrozenPass"
    makeExit SewerCaves ["ToSewerCaves"] "FromSewerCaves"
    makeExit PerilousRoad ["ToPerilousRoad"] "FromPerilousRoad"

    onStartDaily 472927 $ do
      addUnlockedDoors globals
      setAreaCleared Holmgare True

    simpleTownsperson 217809 TownManApron "Gregor" ImmobileAI $ \_ -> do
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
    simpleTownsperson 092833 TownWomanBlue "SophiaMom"
                      ImmobileAI $ \_ge -> do
      narrate "Oh hai."
    simpleTownsperson 711833 TownManRed "SophiaDad" ImmobileAI $ \_ge -> do
      narrate "Hi, folks!"
      number <- forcedChoice "Please pick a number."
                  [("\"One!\"", 1), ("\"Two?\"", 2), ("\"Wait, I'm confused.  Is this a trick question?  Can I get a hint?  I don't know what to do!  I want my mommy!  Um, sorry...I meant 'three.'\"", 3),
                  ("(Just walk away.)", 4), ("\"Oh, yeah, we totally picked five.\"  (Lie.)", 5)]
      narrate $ "You chose " ++ show (number :: Int) ++ ", I guess."
    return ()

  compileSewerCaves globals

  compileArea PerilousRoad Nothing $ do
    makeExit Holmgare ["ToHolmgare"] "FromHolmgare"
    makeExit StoneBridge ["ToStoneBridge"] "FromStoneBridge"
    makeExit IcyConfluence ["ToIcyConfluence"] "FromIcyConfluence"

  compileStoneBridge globals
  compileTragorda globals
  compileWhistlingWoods globals
  compileIcyConfluence globals

  compileArea Marata Nothing $ do

    makeExit IcyConfluence ["ToIcyConfluence"] "FromIcyConfluence"
    makeExit IronMine ["ToIronMine"] "FromIronMine"
    makeExit NorthernTundra ["ToNorthernTundra"] "FromNorthernTundra"

    onStartDaily 109833 $ do
      addUnlockedDoors globals

  compileIronMine globals

  compileArea NorthernTundra Nothing $ do
    makeExit Marata ["ToMarata"] "FromMarata"
    makeExit Duskwood ["ToDuskwood"] "FromDuskwood"

  compileArea Duskwood Nothing $ do
    makeExit WhistlingWoods ["ToWhistlingWoods"] "FromWhistlingWoods"
    makeExit Icehold ["ToIcehold"] "FromIcehold"
    makeExit NorthernTundra ["ToNorthernTundra"] "FromNorthernTundra"
    makeExit Tragorda ["ToTragorda"] "FromTragorda"

  compileIcehold globals

  compileArea BurningMaze Nothing $ return ()

  compileArea Gazerpit Nothing $ return ()

  compileArea ArcaneLab Nothing $ return ()
  compileArea InnerLab Nothing $ return ()

-------------------------------------------------------------------------------
