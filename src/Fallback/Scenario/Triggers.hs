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
   getAreaTriggers, getMonsterScript, getRegionBackground)
where

import Control.Monad (join)

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Corenglen (compileCorenglen)
import Fallback.Scenario.Triggers.FrozenPass (compileFrozenPass)
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Icehold (compileIcehold)
import Fallback.Scenario.Triggers.IronMine (compileIronMine)
import Fallback.Scenario.Triggers.MountainPath
  (compileMountainPath, startingPosition)
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

    makeExit FrozenPass ["ToFrozenPass1", "ToFrozenPass2"] (Point 3 7)
    makeExit SewerCaves ["ToSewerCaves"] (Point 36 3)
    makeExit PerilousRoad ["ToPerilousRoad"] (Point 51 25)

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
    makeExit Holmgare ["ToHolmgare"] (Point 3 6)
    makeExit StoneBridge ["ToStoneBridge"] (Point 11 6)
    makeExit IcyConfluence ["ToIcyConfluence"] (Point 10 3)

  compileStoneBridge globals
  compileTragorda globals
  compileWhistlingWoods globals

  compileArea IcyConfluence Nothing $ do
    makeExit PerilousRoad ["ToPerilousRoad"] (Point 3 6)
    makeExit Marata ["ToMarata"] (Point 7 3)
    makeExit WhistlingWoods ["ToWhistlingWoods"] (Point 11 6)

  compileArea Marata Nothing $ do

    makeExit IcyConfluence ["ToIcyConfluence"] (Point 11 40)
    makeExit IronMine ["ToIronMine"] (Point 41 3)
    makeExit NorthernTundra ["ToNorthernTundra"] (Point 51 16)

    onStartDaily 109833 $ do
      addUnlockedDoors globals

  compileIronMine globals

  compileArea NorthernTundra Nothing $ do
    makeExit Marata ["ToMarata"] (Point 3 5)
    makeExit Duskwood ["ToDuskwood"] (Point 11 5)

  compileArea Duskwood Nothing $ do
    makeExit WhistlingWoods ["ToWhistlingWoods"] (Point 3 5)
    makeExit Icehold ["ToIcehold"] (Point 11 5)
    makeExit NorthernTundra ["ToNorthernTundra"] (Point 7 3)
    makeExit Tragorda ["ToTragorda"] (Point 7 8)

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
