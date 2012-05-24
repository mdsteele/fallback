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

{-# LANGUAGE GADTs, KindSignatures #-}

module Fallback.View.GameMenu
  (GameMenuState(..), gmsSubState, gmsSavedGame, gmsUnsaved, gmsModifyParty,
   GameMenuAction(..), newGameMenuView,
   newPreferencesView, newChangeDifficultyView)
where

import Control.Applicative ((<$), (<$>))
import Data.Maybe (isJust)

import Fallback.Data.Color (blackColor)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.Preferences (Preferences(..))
import Fallback.Scenario.Save (SavedGame(..))
import Fallback.State.Area (acsParty, arsParty)
import Fallback.State.Combat (CombatState, csCommon)
import Fallback.State.Resources (FontTag(..), Resources, rsrcFont)
import Fallback.State.Party (Party, partyDifficulty)
import Fallback.State.Region (RegionState, rsParty, rsUnsaved)
import Fallback.State.Simple (Difficulty)
import Fallback.State.Town (TownState(tsCommon))
import Fallback.View.Base
import Fallback.View.Dialog (newDialogView, newDialogView')
import Fallback.View.NewGame (newDifficultyView)
import Fallback.View.Widget
  (enabledIf, makeLabel, makeLabel_, newCheckbox, newSimpleTextButton,
   newTextButton)

-------------------------------------------------------------------------------

data GameMenuState :: * -> * where
  CombatMenuState :: CombatState -> GameMenuState CombatState
  RegionMenuState :: RegionState -> GameMenuState RegionState
  TownMenuState :: TownState -> GameMenuState TownState

gmsSubState :: GameMenuState a -> a
gmsSubState (CombatMenuState cs) = cs
gmsSubState (RegionMenuState rs) = rs
gmsSubState (TownMenuState ts) = ts

gmsAllowChangeDifficulty :: GameMenuState a -> Bool
gmsAllowChangeDifficulty (RegionMenuState _) = True
gmsAllowChangeDifficulty _ = False

gmsModifyParty :: (Party -> Party) -> GameMenuState a -> GameMenuState a
gmsModifyParty fn (CombatMenuState cs) =
  let acs = csCommon cs
      party = acsParty acs
  in CombatMenuState (cs { csCommon = acs { acsParty = fn party } })
gmsModifyParty fn (RegionMenuState rs) =
  RegionMenuState (rs { rsParty = fn (rsParty rs) })
gmsModifyParty fn (TownMenuState ts) =
  let acs = tsCommon ts
      party = acsParty acs
  in TownMenuState (ts { tsCommon = acs { acsParty = fn party } })

gmsParty :: GameMenuState a -> Party
gmsParty (CombatMenuState cs) = arsParty cs
gmsParty (RegionMenuState rs) = rsParty rs
gmsParty (TownMenuState ts) = arsParty ts

gmsSavedGame :: GameMenuState a -> Maybe SavedGame
gmsSavedGame (CombatMenuState _) = Nothing
gmsSavedGame (RegionMenuState rs) = Just (SavedRegionState rs)
gmsSavedGame (TownMenuState ts) = Just (SavedTownState ts)

gmsUnsaved :: GameMenuState a -> Bool
gmsUnsaved (CombatMenuState _) = True
gmsUnsaved (RegionMenuState rs) = rsUnsaved rs
gmsUnsaved (TownMenuState _) = True -- TODO: Should this always be True?

data GameMenuAction = GameMenuChangeDifficulty
                    | GameMenuEditParty
                    | GameMenuLoad
                    | GameMenuPreferences
                    | GameMenuQuit
                    | GameMenuReturnToGame
                    | GameMenuReturnToMainMenu
                    | GameMenuSave

-------------------------------------------------------------------------------

newGameMenuView :: (MonadDraw m) => Resources -> View a b
                -> m (View (GameMenuState a) GameMenuAction)
newGameMenuView resources bgView = do
  dialog <- newGameMenuDialog resources
  newDialogView' bgView gmsSubState dialog $ Rect 70 25 500 440

newGameMenuDialog :: (MonadDraw m) => Resources
                  -> m (View (GameMenuState a) GameMenuAction)
newGameMenuDialog resources = do
  let { buttonMargin = 20; buttonSpacing = 8; buttonHeight = 24 }
  let buttonSub col row = subView $ \_ (w, h) ->
        let buttonWidth = half (w - 2 * buttonMargin - buttonSpacing)
        in Rect (buttonMargin + col * (buttonWidth + buttonSpacing))
                (h - buttonMargin - buttonHeight -
                 row * (buttonSpacing + buttonHeight))
                buttonWidth buttonHeight
  compoundViewM [
    -- TODO quests view
    (buttonSub 0 3 .
     vmap (\gms -> ("Save Game", enabledIf $ isJust $ gmsSavedGame gms)) <$>
     newTextButton resources [KeyS] GameMenuSave),
    (buttonSub 0 2 <$>
     newSimpleTextButton resources "Load Game" [KeyO] GameMenuLoad),
    (buttonSub 0 1 <$>
     newSimpleTextButton resources "Return to Title Screen" [KeyT]
                         GameMenuReturnToMainMenu),
    (buttonSub 0 0 <$>
     newSimpleTextButton resources "Quit" [KeyQ] GameMenuQuit),
    (buttonSub 1 3 <$>
     newSimpleTextButton resources "Preferences" [KeyP] GameMenuPreferences),
    (buttonSub 1 2 .
     vmap (\gms -> ("Edit Party", enabledIf $ isJust $ gmsSavedGame gms)) <$>
     newTextButton resources [KeyE] GameMenuEditParty),
    (buttonSub 1 1 .
     vmap (\gms -> ("Change Difficulty",
                    enabledIf (gmsAllowChangeDifficulty gms))) <$>
     newTextButton resources [KeyD] GameMenuChangeDifficulty),
    (buttonSub 1 0 <$>
     newSimpleTextButton resources "Return to Game"
                         [KeyEscape, KeyReturn] GameMenuReturnToGame)]

-------------------------------------------------------------------------------

newPreferencesView :: (MonadDraw m) => Resources -> Preferences
                   -> View a b -> a -> m (View () (Maybe Preferences))
newPreferencesView resources initPrefs bgView bgInput = do
  dialog <- do
    let labelFont = rsrcFont resources FontGeorgia11
    let newCheckboxAndLabel label top getter setter = compoundViewM [
          (viewMap getter setter <$>
           newCheckbox resources [] (LocTopleft $ Point 24 top)),
          (return $ vmap (const label) $ makeLabel_ labelFont blackColor $
           LocTopleft $ Point 45 (top + 1))]
    stateRef <- newDrawRef initPrefs
    compoundViewM [
      (newTopLabel resources "Preferences"),
      (viewMapM (const $ readDrawRef stateRef)
                ((Suppress <$) . modifyDrawRef stateRef) <$>
       compoundViewM [
         (newCheckboxAndLabel "Enable sound effects" 50 prefEnableSound
                              (\b p -> p { prefEnableSound = b })),
         (newCheckboxAndLabel "Enable background music" 80 prefEnableMusic
                              (\b p -> p { prefEnableMusic = b })),
         (newCheckboxAndLabel "Fullscreen on startup" 110 prefFullscreen
                              (\b p -> p { prefFullscreen = b }))]),
      (viewMapM return (const (Action . Just <$> readDrawRef stateRef)) .
       subView (\_ (w, h) -> Rect (w - 100) (h - 44) 80 24) <$>
       newSimpleTextButton resources "OK" [KeyReturn] ()),
      (subView (\_ (_, h) -> Rect 20 (h - 44) 80 24) <$>
       newSimpleTextButton resources "Cancel" [KeyEscape] Nothing)]
  newDialogView bgView bgInput dialog $ Rect 170 145 300 190

-------------------------------------------------------------------------------

newChangeDifficultyView :: (MonadDraw m) => Resources
                        -> View (GameMenuState a) b -> (GameMenuState a)
                        -> m (View () Difficulty)
newChangeDifficultyView resources bgView gms = do
  dialog <- do
    let initDiff = partyDifficulty $ gmsParty gms
    stateRef <- newDrawRef initDiff
    compoundViewM [
      (viewMapM (const $ readDrawRef stateRef)
                ((Suppress <$) . writeDrawRef stateRef) .
       subView (\_ (w, h) -> Rect 20 20 (w - 40) (h - 75)) <$>
       newDifficultyView resources),
      (viewMapM return (const (Action <$> readDrawRef stateRef)) .
       subView (\_ (w, h) -> Rect (w - 100) (h - 44) 80 24) <$>
       newSimpleTextButton resources "OK" [KeyReturn] ()),
      (subView (\_ (_, h) -> Rect 20 (h - 44) 80 24) <$>
       newSimpleTextButton resources "Cancel" [KeyEscape] initDiff)]
  newDialogView bgView gms dialog $ Rect 214 90 220 305

-------------------------------------------------------------------------------

newTopLabel :: (MonadDraw m) => Resources -> String -> m (View a b)
newTopLabel resources string = do
  let headingFont = rsrcFont resources FontGeorgiaBold14
  return $ vmap (const string) $ makeLabel headingFont blackColor $ \(w, _) ->
    LocMidtop $ Point (w `div` 2) 16

-------------------------------------------------------------------------------
