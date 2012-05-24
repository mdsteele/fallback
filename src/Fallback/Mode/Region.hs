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

module Fallback.Mode.Region
  (newRegionMode)
where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.IORef
import qualified Data.Set as Set

import Fallback.Constants (screenRect)
import Fallback.Draw (paintScreen, handleScreen, takeScreenshot)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Error (popupIfErrors)
import Fallback.Mode.GameMenu (GameMenuState(RegionMenuState), newGameMenuMode)
import Fallback.Mode.LoadGame (newLoadGameMode)
import Fallback.Mode.SaveGame (newSaveBeforeQuittingMode, newSaveGameMode)
import Fallback.Scenario.Areas (areaEntrance, areaLinks, enterPartyIntoArea)
import Fallback.Scenario.Regions (regionBackground)
import Fallback.Scenario.Save (SavedGame(SavedRegionState))
import Fallback.State.Party (partyClearedArea, partyFoundAreas)
import Fallback.State.Region
import Fallback.State.Resources (Resources)
import Fallback.State.Tags (AreaTag)
import Fallback.View
import Fallback.View.Region

-------------------------------------------------------------------------------

newRegionMode :: Resources -> Modes -> RegionState -> IO Mode
newRegionMode resources modes initState = do
  stateRef <- newIORef initState { rsParty =
    (rsParty initState) { partyFoundAreas =
      Set.insert (rsSelectedArea initState) $
      Set.insert (rsPreviousArea initState) $
      partyFoundAreas (rsParty initState) } }
  view <- do
    state <- readIORef stateRef
    newRegionView resources $ regionBackground (rsParty state) (rsRegion state)
  let

    mode EvQuit = do
      rs <- readIORef stateRef
      if not (rsUnsaved rs) then return DoQuit else do
        screenshot <- takeScreenshot screenRect
        ChangeMode <$> newSaveBeforeQuittingMode resources screenshot
                         (SavedRegionState rs) mode view rs
    mode (EvKeyDown KeyO [KeyModCmd] _) = do
      rs <- readIORef stateRef
      ChangeMode <$> newLoadGameMode resources modes mode view rs
    mode (EvKeyDown KeyS [KeyModCmd, KeyModShift] _) = do
      screenshot <- takeScreenshot screenRect
      rs <- readIORef stateRef
      let onSave _ = return (ChangeMode mode) -- TODO display some message
      ChangeMode <$> newSaveGameMode resources onSave screenshot
                                     (SavedRegionState rs) mode view rs
    mode event = do
      when (event == EvTick) $ modifyIORef stateRef tickRegionState
      rs <- readIORef stateRef
      action <- handleScreen $ viewHandler view rs event
      when (event == EvTick) $ paintScreen (viewPaint view rs)
      case fromAction action of
        Nothing -> return SameMode
        Just (SelectAreaNode node) -> do
          case trySelectAreaNode rs node of
            Nothing -> return ()
            Just prev -> writeIORef stateRef rs { rsSelectedArea = node,
                                                  rsPreviousArea = prev }
          return SameMode
        Just TravelToSelectedArea -> do
          let tag = rsSelectedArea rs
          popupIfErrors resources view rs (return mode)
                        (enterPartyIntoArea resources (rsParty rs) tag $
                         areaEntrance tag (rsPreviousArea rs)) $ \ts -> do
            ChangeMode <$> newTownMode' modes ts
        Just ShowMenu -> do
          screenshot <- takeScreenshot screenRect
          let onDone rs' = do
                writeIORef stateRef rs'
                return mode
          ChangeMode <$> newGameMenuMode resources modes screenshot
                                         (RegionMenuState rs) onDone view

  return mode

-------------------------------------------------------------------------------

trySelectAreaNode :: RegionState -> AreaTag -> Maybe AreaTag
trySelectAreaNode state dest =
  let found = rsFoundAreas state
      party = rsParty state
      -- TODO: Use Queue type instead of list here
      bfs visited ((prev, curr) : queue) =
        if curr == dest then Just prev else
          let visited' = Set.insert curr visited
              queue' =
                if not (partyClearedArea party curr) then queue
                else (queue ++) $ map ((,) curr) $
                     filter (flip Set.member found) $
                     filter (flip Set.notMember visited') $ areaLinks curr
          in bfs visited' queue'
      bfs _ [] = Nothing
  in bfs Set.empty [(rsSelectedArea state, rsPreviousArea state),
                    (rsPreviousArea state, rsSelectedArea state)]

-------------------------------------------------------------------------------
