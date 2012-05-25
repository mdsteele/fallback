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
import Data.Array (listArray)
import Data.List (sort)
import Data.Maybe (isJust)

import Fallback.Data.Color (Color(Color), Tint(Tint), blackColor, blackTint)
import Fallback.Data.Point
import qualified Fallback.Data.SparseMap as SM (sparseAssocs)
import Fallback.Draw
import Fallback.Event
import Fallback.Preferences (Preferences(..))
import Fallback.Scenario.Save (SavedGame(..))
import Fallback.State.Area (acsParty, arsParty)
import Fallback.State.Combat (CombatState, csCommon)
import Fallback.State.Resources (FontTag(..), Resources, rsrcFont)
import Fallback.State.Party (Party, partyDifficulty, partyQuests)
import Fallback.State.Region (RegionState, rsParty, rsUnsaved)
import Fallback.State.Simple (Difficulty, QuestStatus(QuestActive))
import Fallback.State.Tags (QuestTag, questDescription, questName)
import Fallback.State.Town (TownState(tsCommon))
import Fallback.View.Base
import Fallback.View.Dialog (newDialogView, newDialogView')
import Fallback.View.NewGame (newDifficultyView)
import Fallback.View.Widget
  (enabledIf, makeLabel, makeLabel_, newCheckbox, newDynamicTextWrapView,
   newSimpleTextButton, newTextButton)

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
  let { buttonMargin = 20; buttonSpacing = 6; buttonHeight = 24 }
  let buttonSub col row = subView $ \_ (w, h) ->
        let buttonWidth = half (w - 2 * buttonMargin - buttonSpacing)
        in Rect (buttonMargin + col * (buttonWidth + buttonSpacing))
                (h - buttonMargin - buttonHeight -
                 row * (buttonSpacing + buttonHeight))
                buttonWidth buttonHeight
  compoundViewM [
    (newTopLabel resources "Party Quest Log"),
    (subView (\_ (w, h) -> Rect 16 38 (w - 32)
                                (h - 38 - buttonMargin -
                                 4 * (buttonHeight + buttonSpacing))) .
     vmap gmsParty <$> newQuestsView resources),
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

newQuestsView :: (MonadDraw m) => Resources -> m (View Party a)
newQuestsView resources = do
  let { numButtons = 10; buttonWidth = 200; buttonHeight = 22 }
  indexRef <- newDrawRef 0
  let questArrayFn party =
        listArray (0, numButtons - 1) $ (++ repeat Nothing) $ map Just $ sort $
        map fst $ filter ((QuestActive ==) . snd) $ SM.sparseAssocs $
        partyQuests party
  let nameInputFn idx arr = do
        chosen <- readDrawRef indexRef
        return $ fmap (flip (,) chosen) (arr ! idx)
  let makeNameWidget idx =
        viewMapM (nameInputFn idx) ((Suppress <$) . writeDrawRef indexRef) .
        maybeView id .
        subView_ (Rect 0 (6 + (buttonHeight + 1) * idx)
                       buttonWidth buttonHeight) <$>
        newQuestNameWidget resources idx
  let descInputFn arr = do
        chosen <- readDrawRef indexRef
        return $ fmap (flip (,) chosen) $ (arr ! chosen)
  vmap questArrayFn <$> compoundViewM [
    (compoundViewM $ map makeNameWidget [0 .. numButtons - 1]),
    (vmapM descInputFn . maybeView id .
     subView (\_ (w, h) -> Rect buttonWidth 0 (w - buttonWidth) h) <$>
     newQuestDescriptionView resources)]

newQuestNameWidget :: (MonadDraw m) => Resources -> Int
                   -> m (View (QuestTag, Int) Int)
newQuestNameWidget resources idx = do
  let font1 = rsrcFont resources FontGeorgia11
  let font2 = rsrcFont resources FontGeorgiaBold11
  let
    paint (tag, chosen) = do
      let selected = chosen == idx
      (w, h) <- canvasSize
      drawText (if selected then font2 else font1)
               (if selected then Color 192 0 0 else blackColor)
               (LocMidleft $ Point 5 $ half h) (questName tag)
      drawLineChain (if selected then blackTint else Tint 0 0 0 40)
                    [Point w 0, Point 3 0, Point 0 3,
                     Point 0 (h - 4), Point 3 (h - 1), Point w (h - 1)]
    handler _ (EvMouseDown pt) = do
      whenWithinCanvas pt $ return $ Action idx
    handler _ _ = return Ignore
  return $ View paint handler

newQuestDescriptionView :: (MonadDraw m) => Resources
                        -> m (View (QuestTag, Int) a)
newQuestDescriptionView resources = do
  let paintBackground (_, idx) = do
        (w, h) <- canvasSize
        let startY = 6 + 23 * idx
        drawLineChain blackTint [
          Point 0 startY, Point 0 5, Point 5 0, Point (w - 6) 0,
          Point (w - 1) 5, Point (w - 1) (h - 6), Point (w - 6) (h - 1),
          Point 5 (h - 1), Point 0 (h - 6), Point 0 (startY + 21)]
  compoundViewM [
    (return $ inertView paintBackground),
    (subView (\_ (w, h) -> Rect 8 8 (w - 16) (h - 16)) .
     vmap (questDescription . fst) <$> newDynamicTextWrapView resources)]

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
