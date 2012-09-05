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

import Fallback.Scenario.Compile
import Fallback.Scenario.Triggers.Corenglen (compileCorenglen)
import Fallback.Scenario.Triggers.FrozenPass (compileFrozenPass)
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Holmgare (compileHolmgare)
import Fallback.Scenario.Triggers.Icehold (compileIcehold)
import Fallback.Scenario.Triggers.IcyConfluence (compileIcyConfluence)
import Fallback.Scenario.Triggers.IronMine (compileIronMine)
import Fallback.Scenario.Triggers.MountainPath (compileMountainPath)
import Fallback.Scenario.Triggers.PerilousRoad (compilePerilousRoad)
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
    simpleEnemy_ 660634 "Wolf" Wolf (PatrolAI "Wolf" "WolfPatrol")
    simpleEnemy_ 978292 "Bat" CaveBat MindlessAI

  compileMountainPath globals
  compileCorenglen globals

  compileFrozenPass globals
  compileHolmgare globals
  compileSewerCaves globals
  compilePerilousRoad globals
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
