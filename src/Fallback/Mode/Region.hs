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
import Fallback.Data.Couple (fromCouple, makeCouple)
import qualified Fallback.Data.Queue as Queue
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
import Fallback.State.Party (partyClearedArea, partyFoundAreaLinks)
import Fallback.State.Region
import Fallback.State.Resources (Resources)
import Fallback.State.Tags (AreaTag)
import Fallback.View
import Fallback.View.Region

-------------------------------------------------------------------------------

newRegionMode :: Resources -> Modes -> RegionState -> IO Mode
newRegionMode resources modes initState = do
  stateRef <- newIORef initState { rsParty =
    (rsParty initState) { partyFoundAreaLinks =
      Set.insert (makeCouple (rsPreviousArea initState)
                             (rsSelectedArea initState)) $
      partyFoundAreaLinks (rsParty initState) } }
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

-- | If the party can region the given area from where they are, return the
-- directly linked area from which they would enter the selected area; if the
-- desired area is unreachable, return 'Nothing'.
trySelectAreaNode :: RegionState -> AreaTag -> Maybe AreaTag
trySelectAreaNode rs dest = bfs initVisited initQueue where
  bfs visited queue =
    case Queue.pop queue of
      Just ((prev, curr), queue') ->
        if curr == dest then Just prev else
          if notCleared curr then bfs visited queue' else
            let children = filter (linkIsFound curr) $
                           filter (flip Set.notMember visited) $
                           Set.toList $ areaLinks curr
            in bfs (foldr Set.insert visited children)
                   (foldr Queue.insert queue' $ map ((,) curr) children)
      Nothing -> Nothing
  notCleared = not . partyClearedArea (rsParty rs)
  linkIsFound tag1 tag2 = Set.member (makeCouple tag1 tag2) foundLinks
  foundLinks = Set.filter (isRealLink . fromCouple) $ rsFoundAreaLinks rs
  isRealLink (tag1, tag2) = Set.member tag1 $ areaLinks tag2
  initQueue = Queue.fromList [(rsSelectedArea rs, rsPreviousArea rs),
                              (rsPreviousArea rs, rsSelectedArea rs)]
  initVisited = Set.fromList [rsPreviousArea rs, rsSelectedArea rs]

-------------------------------------------------------------------------------
