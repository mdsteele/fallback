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

module Fallback.View.Combat
  (CombatAction(..), newCombatView)
where

import Control.Applicative ((<$>))

import Fallback.Constants (sidebarWidth)
import Fallback.Data.Color (Tint(Tint))
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM (get)
import Fallback.Draw
import Fallback.Event
import Fallback.State.Area
import Fallback.State.Camera (camTopleft)
import Fallback.State.Combat
import Fallback.State.Creature
import Fallback.State.Doodad (DoodadHeight(..), paintDoodads)
import Fallback.State.Item (wdRange)
import Fallback.State.Party
import Fallback.State.Resources (Resources, rsrcCharacterImages)
import Fallback.State.Simple (CharacterNumber)
import Fallback.State.Terrain
import Fallback.Utility (maybeM)
import Fallback.View.Abilities
import Fallback.View.Base
import Fallback.View.Camera
import Fallback.View.Hover
import Fallback.View.Inventory
import Fallback.View.Sidebar

-------------------------------------------------------------------------------

data CombatAction = CombatSidebar SidebarAction
                  | CombatAbilities AbilitiesAction
                  | CombatInventory InventoryAction
                  | CombatMove Direction
                  | CombatAttack Position
                  | CombatEndTurnEarly
                  | CombatTargetPosition Position
                  | CombatTargetCharacter CharacterNumber
                  | CombatEndTargeting

newCombatView :: (MonadDraw m) => Resources
              -> m (View CombatState CombatAction)
newCombatView resources = newCursorView resources $ \cursorSink -> do
  let mapRect _ (w, h) = Rect sidebarWidth 0 (w - sidebarWidth) h
  let sidebarRect _ (_, h) = Rect 0 0 sidebarWidth h
  let abilitiesFn cs =
        case csPhase cs of
          ChooseAbilityPhase cc -> Just AbilitiesState
            { abilsActiveCharacter = ccCharacterNumber cc,
              abilsInCombat = True,
              abilsMetaAbilityTag = Nothing,
              abilsParty = arsParty cs }
          MetaAbilityPhase cm -> Just AbilitiesState
            { abilsActiveCharacter = ccCharacterNumber (cmCommander cm),
              abilsInCombat = True,
              abilsMetaAbilityTag = Just (cmFeatTag cm),
              abilsParty = arsParty cs }
          _ -> Nothing
  let inventoryFn cs =
        case csPhase cs of
          InventoryPhase cc mbItem ->
            Just $ makeInventoryState cs (ccCharacterNumber cc) mbItem
          _ -> Nothing
  compoundViewM [
    (subView sidebarRect . viewMap SidebarCombat CombatSidebar <$>
     newSidebarView resources),
    (subView mapRect <$> compoundViewM [
       (newCombatMapView resources),
       (newMaybeView inventoryFn =<< fmap CombatInventory <$>
        newInventoryView resources cursorSink),
       (newMaybeView abilitiesFn =<< fmap CombatAbilities <$>
        newAbilitiesView resources cursorSink)])]

newCombatMapView :: (MonadDraw m) => Resources
                 -> m (View CombatState CombatAction)
newCombatMapView resources = do
  let
    paint cs = do
      let acs = csCommon cs
      let cameraTopleft = camTopleft (acsCamera acs)
      paintTerrain acs
      paintDoodads cameraTopleft LowDood (acsDoodads acs)
      paintFields resources cameraTopleft (acsVisible acs)
                  (acsClock acs) (acsFields acs)
      paintMonsters acs True
      paintCharacters resources cameraTopleft cs
      paintDoodads cameraTopleft MidDood (acsDoodads acs)
      tintNonVisibleTiles acs
      paintDoodads cameraTopleft HighDood (acsDoodads acs)
      let paintRange cc = do
            let charNum = ccCharacterNumber cc
            paintWeaponRange cameraTopleft cs charNum
                             (wdRange $ chrEquippedWeaponData $
                              arsGetCharacter charNum cs)
      case csPhase cs of
        CommandPhase cc -> paintRange cc
        TargetingPhase (CombatTargeting { ctCommander = cc,
                                          ctTargeting = targeting }) -> do
          mbMousePt <- getRelativeMousePos
          paintTargeting cameraTopleft mbMousePt cs
                         (ccCharacterNumber cc) targeting
        ExecutionPhase ce -> maybeM (ceCommander ce) paintRange
        _ -> return ()
      maybeM (acsMessage acs) (paintMessage resources)

    handler _ EvTick =
      maybe Ignore (Action . CombatMove) <$> getArrowKeysDirection
    handler cs (EvMouseDown pt) = do
      whenWithinCanvas pt $ do
      let pt' = pt `pAdd` (camTopleft $ arsCamera cs)
      let pos = pointPosition pt'
      case csPhase cs of
        WaitingPhase ->
          case arsOccupant pos cs of
            Just (Left charNum) ->
              return $ Action $ CombatSidebar $ MakeCharacterActive charNum
            _ -> return Suppress
        CommandPhase cc ->
          -- FIXME device interation
          case arsOccupant pos cs of
            Just (Left charNum) ->
              return $ Action $ CombatSidebar $ MakeCharacterActive charNum
            Just (Right _) -> return $ Action $ CombatAttack pos
            Nothing -> do
              let here = arsCharacterPosition (ccCharacterNumber cc) cs
              if pos == here then return Suppress else do
              return $ Action $ CombatMove $
                if adjacent pos here then ipointDir (pos `pSub` here) else
                  ipointDir (pt' `pSub` positionCenter here)
        ChooseAbilityPhase _ -> return Suppress
        MetaAbilityPhase _ -> return Suppress
        InventoryPhase _ _ -> return Suppress
        TargetingPhase _ -> return $ Action $ CombatTargetPosition pos
        ExecutionPhase _ -> return Suppress
    handler cs (EvKeyDown key _ _) = do
      case key of
        KeySpace -> do
          case csPhase cs of
            WaitingPhase -> return Ignore
            CommandPhase _ -> return $ Action $ CombatEndTurnEarly
            ChooseAbilityPhase _ -> return Suppress
            MetaAbilityPhase _ -> return Suppress
            InventoryPhase _ _ -> return Suppress
            TargetingPhase _ -> return $ Action $ CombatEndTargeting
            ExecutionPhase _ -> return Suppress
        _ ->
          case keyCharacterNumber key of
            Just charNum ->
              case csPhase cs of
                TargetingPhase _ ->
                  return $ Action $ CombatTargetCharacter charNum
                _ -> return Ignore
            Nothing -> return Ignore
    handler _ _ = return Ignore

  return $ View paint handler

-------------------------------------------------------------------------------

paintCharacters :: Resources -> IPoint -> CombatState -> Paint ()
paintCharacters resources cameraTopleft cs =
  mapM_ paintChar [minBound .. maxBound] where
    paintChar charNum = do
      let char = arsGetCharacter charNum cs
      if not (chrIsConscious char) then return () else do
      let ccs = TM.get charNum $ csCharStates cs
      let pos = ccsPosition ccs
      let pose = ccsPose ccs
      let sprite =
            (case cpAnim pose of { AttackAnim _ -> ciAttack; _ -> ciStand })
            (cpFaceDir pose)
            (rsrcCharacterImages resources (chrClass char)
                                 (chrAppearance char))
      let offset = animOffset (cpAnim pose) pos
      -- TODO draw an extra decoration for the active character
      blitStretchTinted (Tint 255 255 255 (cpAlpha pose)) sprite $
        positionRect pos `rectPlus` (offset `pSub` cameraTopleft)
      let prect = makeRect pos (1, 1)
      paintStatusDecorations resources cameraTopleft (arsClock cs)
                             prect (chrStatus char)
      paintHealthBar cameraTopleft True prect offset (chrHealth char)
                     (chrMaxHealth (arsParty cs) char) Nothing

-------------------------------------------------------------------------------
