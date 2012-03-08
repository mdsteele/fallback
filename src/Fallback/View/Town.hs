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

{-# LANGUAGE GADTs #-}

module Fallback.View.Town
  (TownAction(..), newTownView)
where

import Control.Applicative ((<$), (<$>))
import Control.Monad (guard, when)
import qualified Data.Set as Set

import Fallback.Constants (sidebarWidth, talkRangeSquared)
import Fallback.Control.Script (Script, mapEffect)
import Fallback.Data.Grid (GridEntry, geValue, gridEntries, gridSearch)
import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event
import Fallback.State.Area
import Fallback.State.Camera (camTopleft)
import Fallback.State.Creature (animOffset, ciStand)
import Fallback.State.Party
import Fallback.State.Resources (Resources, rsrcCharacterImages)
import Fallback.State.Simple (CharacterNumber)
import Fallback.State.Terrain
import Fallback.State.Town
import Fallback.Utility (flip3, maybeM)
import Fallback.View.Abilities
import Fallback.View.Base
import Fallback.View.Camera
  (paintDevices, paintFields, paintMessage, paintMonsters, paintTargeting,
   paintTerrain, tintNonVisibleTiles)
import Fallback.View.Hover
import Fallback.View.Inventory
import Fallback.View.Quiver
import Fallback.View.Sidebar
import Fallback.View.Upgrade

-------------------------------------------------------------------------------

data TownAction = TownSidebar SidebarAction
                | TownAbilities AbilitiesAction
                | TownInventory InventoryAction
                | TownUpgrade UpgradeAction
                --- | TownInteract (GridEntry Device)
                | TownMove Direction
                | TownScript (Script TownEffect ())
                --- | TownTalk (GridEntry Monster)
                | TownTargetPosition Position
                | TownTargetCharacter CharacterNumber
                | TownCancelTargeting

newTownView :: Resources -> Draw z (View TownState TownAction)
newTownView resources = newCursorView resources $ \cursorSink -> do
  let mapRect _ (w, h) = Rect sidebarWidth 0 (w - sidebarWidth) h
  let sidebarRect _ (_, h) = Rect 0 0 sidebarWidth h
  let abilitiesFn ts =
        case tsPhase ts of
          ChooseAbilityPhase ->
            Just AbilitiesState { abilsActiveCharacter = tsActiveCharacter ts,
                                  abilsInCombat = False,
                                  abilsMetaAbilityTag = Nothing,
                                  abilsParty = arsParty ts }
          _ -> Nothing
  let inventoryFn ts =
        case tsPhase ts of
          InventoryPhase _ ->
            Just InventoryState { ivsActiveCharacter = tsActiveCharacter ts,
                                  ivsClock = arsClock ts,
                                  ivsParty = arsParty ts }
          _ -> Nothing
  let upgradeFn ts =
        case tsPhase ts of
          UpgradePhase st sk ->
            Just UpgradeState { upsActiveCharacter = tsActiveCharacter ts,
                                upsSpentSkills = sk,
                                upsSpentStats = st, upsParty = arsParty ts }
          _ -> Nothing
  hoverView cursorSink DefaultCursor <$> compoundViewM [
    (subView sidebarRect . viewMap SidebarTown TownSidebar <$>
     newSidebarView resources),
    (subView mapRect <$> compoundViewM [
       (newTownMapView resources cursorSink),
       (newMaybeView inventoryFn =<< fmap TownInventory <$>
        newInventoryView resources cursorSink),
       (newMaybeView abilitiesFn =<< fmap TownAbilities <$>
        newAbilitiesView resources cursorSink),
       (newMaybeView upgradeFn =<< fmap TownUpgrade <$>
        newUpgradeView resources cursorSink)])]

newTownMapView :: Resources -> HoverSink Cursor
               -> Draw z (View TownState TownAction)
newTownMapView resources cursorSink = do
  quiver <- newQuiver
  let

    paint (ts, mbMousePt) = do
      let acs = tsCommon ts
      let cameraTopleft = camTopleft $ acsCamera acs
      let explored = arsExploredMap ts
      -- TODO factor out duplicated code from here and Fallback.View.Combat
      paintTerrain acs
      paintDevices resources cameraTopleft explored (acsClock acs)
                   (gridEntries $ acsDevices acs)
      paintDoodads cameraTopleft LowDood (acsDoodads acs)
      paintFields resources cameraTopleft (acsVisible acs) (acsClock acs)
                  (acsFields acs)
      paintMonsters resources cameraTopleft (acsClock acs) (acsVisible acs)
                    [tsPartyPosition ts] (gridEntries $ acsMonsters acs)
      paintParty resources cameraTopleft ts
      paintDoodads cameraTopleft MidDood (acsDoodads acs)
      tintNonVisibleTiles cameraTopleft explored (acsVisible acs)
      paintDoodads cameraTopleft HighDood (acsDoodads acs)
      -- Paint the targeting display, if any:
      case tsPhase ts of
        TargetingPhase (TownTargeting { ttTargeting = targeting }) ->
          paintTargeting cameraTopleft mbMousePt ts
                         (tsActiveCharacter ts) targeting
        _ -> return ()
      maybeM (acsMessage acs) (paintMessage resources)

    handler _ _ EvTick =
      maybe Ignore (Action . TownMove) <$> quiverDirection quiver
    handler (ts, _) rect (EvMouseMotion pt _) = Ignore <$ setCursor ts rect pt
    handler (ts, _) rect (EvMouseDown pt) =
      if not (rectContains rect pt) then return Ignore else do
        setCursor ts rect pt
        case tsPhase ts of
          WalkingPhase -> do
            let townScript = maybe Suppress (Action . TownScript)
            return $ mouseCase townScript townScript (Action . TownMove)
                               ts rect pt
          ChooseAbilityPhase -> return Suppress
          InventoryPhase _ -> return Suppress
          UpgradePhase _ _ -> return Suppress
          TargetingPhase _ -> return $ Action $ TownTargetPosition pos
          ScriptPhase _ -> return Suppress
      where pos = pointPosition (pt `pAdd` (camTopleft $ arsCamera ts))
    handler (ts, _) _ (EvKeyDown KeyEscape _ _) = do
      case tsPhase ts of
        TargetingPhase _ -> return (Action TownCancelTargeting)
        _ -> return Ignore
    handler (ts, _) _ (EvKeyDown key _ _) = do
      quiverKeyDown quiver key
      case keyCharacterNumber key of
        Nothing -> return Ignore
        Just charNum -> return $
          case tsPhase ts of
            TargetingPhase targeting ->
              (case targeting of
                 TownTargeting { ttTargeting = TargetingAlly _ } ->
                   Action $ TownTargetCharacter charNum
                 _ -> Suppress) :: Action TownAction
            _ -> Ignore
    handler _ _ (EvKeyUp key) = Ignore <$ quiverKeyUp quiver key
    handler (ts, _) rect (EvFocus pt) = Ignore <$ setCursor ts rect pt
    handler _ _ EvBlur = Ignore <$ resetQuiver quiver
    handler _ _ _ = return Ignore

    mouseCase :: (Maybe (Script TownEffect ()) -> a)
              -> (Maybe (Script TownEffect ()) -> a)
              -> (Direction -> a) -> TownState -> IRect -> IPoint -> a
    mouseCase monFn devFn dirFn ts rect pt =
      let acs = tsCommon ts
          pos = pointPosition (pt `pAdd` camTopleft (acsCamera acs))
          checkRange r s = guard (pos `pSqDist` tsPartyPosition ts <= r) >> s
          search grid = do guard $ Set.member pos $ acsVisible acs
                           gridSearch grid pos
          monFn' script = monFn $ checkRange talkRangeSquared $ Just script
          devFn' ge = devFn $ checkRange (devRange $ geValue ge) $ Just $
                      mapEffect EffTownArea $ devInteract (geValue ge) ge $
                      tsActiveCharacter ts
          getScript ge = do mscript <- monstScript (geValue ge)
                            Just (mscriptScriptFn mscript ge)
      in flip3 maybe monFn' (search (acsMonsters acs) >>= getScript) $
         flip3 maybe devFn' (search (acsDevices acs)) $
         dirFn $ ipointDir $ pt `pSub` rectCenter rect

    setCursor :: TownState -> IRect -> IPoint -> Draw z ()
    setCursor ts rect pt =
      when (rectContains rect pt) $ writeHoverSink cursorSink $
      mouseCase (const TalkCursor) (const HandCursor) WalkCursor ts rect pt

  newMouseView $ View paint handler

-------------------------------------------------------------------------------

-- paintTown :: TownState -> Paint ()
-- paintTown ts = do
--   rect <- canvasRect
--   let cameraTopleft = (round <$> tsCameraCenter ts) `pSub` rectCenter rect
--   let explored = tsExploredMap ts
--   paintTerrain cameraTopleft (tsTerrain ts) explored (tsClock ts)
--   paintMonsters cameraTopleft (tsVisible ts) [tsPartyPosition ts]
--                 (gridElems $ tsMonsters ts)
--   paintParty cameraTopleft ts
--   tintNonVisibleTiles cameraTopleft explored (tsVisible ts)
--   maybeM (tsMessage ts) paintMessage

paintParty :: Resources -> IPoint -> TownState -> Paint ()
paintParty resources cameraTopleft ts = do
  let rect = positionRect (tsPartyPosition ts) `rectPlus`
             (animOffset (tsPartyAnim ts) (tsPartyPosition ts) `pSub`
              cameraTopleft)
  let char = arsParty ts `partyGetCharacter` tsActiveCharacter ts
  let sprite = ciStand (tsPartyFaceDir ts) $
               rsrcCharacterImages resources (chrClass char)
                                   (chrAppearance char)
  blitStretch sprite rect

-------------------------------------------------------------------------------
