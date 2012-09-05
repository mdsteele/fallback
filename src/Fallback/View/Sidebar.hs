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

module Fallback.View.Sidebar
  (-- * Sidebar views
   SidebarState(..), ssActiveCharacter, ssParty,
   SidebarAction(..), newSidebarView, newMinimapView,
   --newConversationView,
   -- * Utilities
   keyCharacterNumber)
where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (forM_, when, zipWithM_)

import Fallback.Constants
  (cameraSize, maxActionPoints, maxAdrenaline, maxMoments,
   momentsPerActionPoint, sidebarWidth, tileHeight, tileWidth)
import Fallback.Data.Color
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM (get)
import Fallback.Draw
import Fallback.Event
import Fallback.State.Area (AreaCommonState(..), arsParty)
import Fallback.State.Camera (camTopleft)
import Fallback.State.Combat
import Fallback.State.Creature (ciRightStand)
import Fallback.State.Party
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Town (TownState(..))
import Fallback.Utility (maybeM)
import Fallback.View.Base
import Fallback.View.Widget

-------------------------------------------------------------------------------

data SidebarState = SidebarCombat CombatState
                  | SidebarTown TownState

ssActiveCharacter :: SidebarState -> Maybe CharacterNumber
ssActiveCharacter (SidebarCombat cs) = ccCharacterNumber <$> csCommander cs
ssActiveCharacter (SidebarTown ts) = Just (tsActiveCharacter ts)

ssCameraRect :: SidebarState -> IRect
ssCameraRect ss = makeRect (camTopleft $ acsCamera acs) cameraSize where
  acs = case ss of SidebarCombat cs -> csCommon cs
                   SidebarTown ts -> tsCommon ts

ssCombatState :: SidebarState -> Maybe CombatState
ssCombatState (SidebarCombat cs) = Just cs
ssCombatState (SidebarTown _) = Nothing

ssInCombat :: SidebarState -> Bool
ssInCombat (SidebarCombat _) = True
ssInCombat (SidebarTown _) = False

ssMinimap ::  SidebarState -> Minimap
ssMinimap (SidebarCombat cs) = acsMinimap $ csCommon cs
ssMinimap (SidebarTown ts) = acsMinimap $ tsCommon ts

ssParty :: SidebarState -> Party
ssParty (SidebarCombat cs) = arsParty cs
ssParty (SidebarTown ts) = arsParty ts

ssSidebarEnabled :: SidebarState -> Bool
ssSidebarEnabled _ = True

data SidebarAction = EnterCheatCode
                   | MakeCharacterActive CharacterNumber
                   | QueryMinimap Position
                   | ShowMenu
                   | ToggleAbilities
                   | ToggleInventory
                   | TryToggleCombat

newSidebarView :: (MonadDraw m) => Resources
               -> m (View SidebarState SidebarAction)
newSidebarView resources = do
  bgSprite <- loadSprite "gui/sidebar-background.png"
  buttonIcons <- loadVStrip "gui/sidebar-buttons.png" 5
  let cheatHandler _ (EvKeyDown KeyD [KeyModShift] _) =
        return (Action EnterCheatCode)
      cheatHandler _ _ = return Ignore
  let charRect i = Rect 1 (96 + 85 * i) (sidebarWidth - 6) 84
  let makeButton xIndex iconFn keys value =
        subView_ (Rect (2 + 29 * xIndex) 440 29 36) .
        vmap ((buttonIcons !) . iconFn &&& enabledIf . ssSidebarEnabled) <$>
        newIconButton keys value
  compoundViewM [
    (return $ wallView $ const $ canvasRect >>= blitStretch bgSprite),
    (return $ View (const $ return ()) cheatHandler),
    -- Minimap:
    (subView_ (Rect 2 2 (sidebarWidth - 10) 92) .
     viewMap (ssCameraRect &&& ssMinimap) QueryMinimap <$> newMinimapView),
    -- Character views:
    (compoundViewM [
       subView_ (charRect i) <$> newCharacterView resources charNum
       | (charNum, i) <- zip [minBound .. maxBound] [0 ..]]),
    -- Buttons:
    (compoundViewM [
       (makeButton 0 (const 0) [KeyEscape, KeyM] ShowMenu),
       (makeButton 1 (\s -> if ssInCombat s then 2 else 1) [KeyC]
                   TryToggleCombat),
       (makeButton 2 (const 4) [KeyA] ToggleAbilities),
       (makeButton 3 (const 3) [KeyI] ToggleInventory)])]

-------------------------------------------------------------------------------
-- Minimap view:

newMinimapView :: (MonadDraw m) => m (View (IRect, Minimap) Position)
newMinimapView = do
  let
    paint (camera, minimap) = do
      size <- canvasSize
      let (mincamW, mincamH, offsetX, offsetY) = getOffset camera minimap size
      let mincamX = offsetX + rectX camera * minimapScale `div` tileWidth
          mincamY = offsetY + rectY camera * minimapScale `div` tileHeight
      tintCanvas blackTint
      blitMinimap minimap $ LocTopleft $ Point offsetX offsetY
      drawRect (Tint 255 255 255 192) (Rect mincamX mincamY mincamW mincamH)

    handler (camera, minimap) (EvMouseDown pt) = do
      whenWithinCanvas pt $ do
        (w, h) <- canvasSize
        let (_, _, offsetX, offsetY) = getOffset camera minimap (w, h)
        let x = (pointX pt - offsetX) `div` minimapScale
        let y = (pointY pt - offsetY) `div` minimapScale
        return $ Action $ Point x y
    handler _ _ = return Ignore

    getOffset camera minimap (viewW, viewH) =
      let (mapW, mapH) = minimapBlitSize minimap
          mincamW = rectW camera * minimapScale `div` tileWidth
          mincamH = rectH camera * minimapScale `div` tileHeight
          offsetX = if mapW <= viewW then (viewW - mapW) `div` 2
                    else (viewW - mapW) * rectX camera * minimapScale `div`
                         (tileWidth * (mapW - mincamW))
          offsetY = if mapH <= viewH then (viewH - mapH) `div` 2
                    else (viewH - mapH) * rectY camera * minimapScale `div`
                         (tileHeight * (mapH - mincamH))
      in (mincamW, mincamH, offsetX, offsetY)

  return $ View paint handler

-------------------------------------------------------------------------------
-- Character view:

newCharacterView :: (MonadDraw m) => Resources -> CharacterNumber
                 -> m (View SidebarState SidebarAction)
newCharacterView resources charNum = do
  let getCharacter state = partyGetCharacter (ssParty state) charNum
  let nameTopleft = Point 4 4 :: IPoint
  let pictureTopright = Point 115 6 :: IPoint
  let paintPicture char = do
        blitLoc (ciRightStand $ rsrcCharacterImages resources (chrClass char)
                                                    (chrAppearance char))
                (LocTopright pictureTopright)
  compoundViewM [
    (newCharacterViewBackground charNum),
    (vmap getCharacter <$> compoundViewM [
       (return $ vmap chrName $
        makeLabel_ (rsrcFont resources FontChancery14) blackColor $
        LocTopleft nameTopleft),
       (return $ inertView paintPicture),
       (subView_ (Rect 4 53 52 15) <$> newAdrenalineBarView resources),
       (subView_ (Rect 4 70 104 12) <$> vmap (chrStatus &&& chrHealth) <$>
        newStatusEffectsView resources)]),
    (vmap (ssParty &&& getCharacter) <$> compoundViewM [
       (subView_ (Rect 4 19 80 15) <$> newHealthBarView resources),
       (subView_ (Rect 4 36 80 15) <$> newMojoBarView resources)]),
    (newMaybeView ssCombatState =<< subView_ (Rect 60 53 52 15) <$>
     newTimeBarView resources charNum)]

newCharacterViewBackground :: (MonadDraw m) => CharacterNumber
                           -> m (View SidebarState SidebarAction)
newCharacterViewBackground charNum = do
  let
    paint state = do
      let active = ssActiveCharacter state == Just charNum
      rect <- canvasRect
      tint <- if active then return (Tint 128 0 0 192) else do
        hover <- maybe False (rectContains rect) <$> getRelativeMousePos
        return $ if hover then Tint 128 0 0 128 else Tint 0 0 0 128
      drawRect tint rect
      when active $ do
        drawRect (Tint 128 0 0 128) $ adjustRect1 1 rect
        tintRect (Tint 255 255 255 96) $ adjustRect1 2 rect

    handler _ (EvMouseDown pt) = do
      whenWithinCanvas pt $ return $ Action (MakeCharacterActive charNum)
    handler _ (EvKeyDown key _ _) = do
      return $ if key == characterKey charNum
               then Action (MakeCharacterActive charNum) else Ignore
    handler _ _ = return Ignore

  return $ View paint handler

newHealthBarView :: (MonadDraw m) => Resources -> m (View (Party, Character) b)
newHealthBarView resources = do
  let
    paint (party, char) = do
      rect <- canvasRect
      let fr = fromIntegral (chrHealth char) /
               fromIntegral (chrMaxHealth party char)
      tintRect translucent rect
      tintRect (Tint 255 32 32 255)
               (Rect 0 0 (fr * fromIntegral (rectW rect))
                     (fromIntegral (rectH rect) :: Double))
      paintNumber (rsrcDigitsStripSmall resources) (chrHealth char)
                  (LocCenter $ rectCenter rect)
      blitStretch (hmeLongBarSprite $ rsrcHealthManaEtc resources) rect
  return (inertView paint)

newMojoBarView :: (MonadDraw m) => Resources -> m (View (Party, Character) b)
newMojoBarView resources = do
  let
    paint (party, char) =
      case chrClass char of
        WarriorClass -> paintFocus party char
        RogueClass -> paintFocus party char
        HunterClass -> paintIngredients party
        AlchemistClass -> paintIngredients party
        ClericClass -> paintMana party char
        MagusClass -> paintMana party char
    paintFocus party char = do
      let focus = chrMojo char
      forM_ [0 .. chrMaxMojo party char - 1] $ \idx -> do
        let rect = Rect (5 * idx) 0 4 15
        tintRect (if idx < focus then Tint 128 0 255 255 else translucent) rect
        blitStretch (hmeFocusPipSprite $ rsrcHealthManaEtc resources) rect
    paintIngredients party = do
      blitTopleft (hmeIngredientsSprite $ rsrcHealthManaEtc resources)
                  (pZero :: IPoint)
      let ingredients = partyIngredients party
      let paintIngredient ingredient index = do
            let (row, col) = index `divMod` 4
            paintNumberTinted (rsrcDigitsStripSmall resources)
                              (Tint 226 226 226 255)
                              (TM.get ingredient ingredients)
                              (LocTopright $ Point (15 + 20 * col) (8 * row))
      zipWithM_ paintIngredient [minBound .. maxBound] [0 ..]
    paintMana party char = do
      rect <- canvasRect
      let fr = fromIntegral (chrMojo char) /
               fromIntegral (chrMaxMojo party char)
      tintRect translucent rect
      tintRect (Tint 0 96 255 255) (Rect 0 0 (fr * fromIntegral (rectW rect))
                                         (fromIntegral (rectH rect) :: Double))
      paintNumber (rsrcDigitsStripSmall resources) (chrMojo char)
                  (LocCenter $ rectCenter rect)
      blitStretch (hmeLongBarSprite $ rsrcHealthManaEtc resources) rect
  return (inertView paint)

newAdrenalineBarView :: (MonadDraw m) => Resources -> m (View Character b)
newAdrenalineBarView resources = do
  let
    paint char = do
      rect <- canvasRect
      let fr = fromIntegral (chrAdrenaline char) / fromIntegral maxAdrenaline
      tintRect translucent rect
      tintRect (Tint 224 224 0 255)
               (Rect 0 0 (fr * fromIntegral (rectW rect))
                     (fromIntegral (rectH rect) :: Double))
      paintNumber (rsrcDigitsStripSmall resources) (chrAdrenaline char)
                  ( LocCenter $ rectCenter rect)
      blitStretch (hmeShortBarSprite $ rsrcHealthManaEtc resources) rect
  return (inertView paint)

newTimeBarView :: (MonadDraw m) => Resources -> CharacterNumber
               -> m (View CombatState b)
newTimeBarView resources charNum = do
  let

    paint cs = do
      let ccs = TM.get charNum (csCharStates cs)
      case csCommander cs of
        Just cc | ccCharacterNumber cc == charNum ->
          paintPips (ccsActionPoints ccs) (ccActionPointsUsed cc)
        _ -> paintBar (ccsMoments ccs)

    paintBar moments = do
      let fr = fromIntegral moments / fromIntegral maxMoments
      let actionPoints = moments `div` momentsPerActionPoint
      rect <- canvasRect
      tintRect translucent rect
      let tint = case actionPoints of
                   0 -> Tint 0 96 0 255
                   1 -> Tint 0 128 0 255
                   2 -> Tint 0 160 0 255
                   3 -> Tint 0 192 0 255
                   _ -> Tint 0 224 0 255
      tintRect tint (Rect 0 0 (fr * fromIntegral (rectW rect))
                          (fromIntegral (rectH rect) :: Double))
      paintNumber (rsrcDigitsStripSmall resources) actionPoints
                  (LocCenter $ rectCenter rect)
      blitStretch (hmeShortBarSprite $ rsrcHealthManaEtc resources) rect

    paintPips apStart apUsed = do
      forM_ [0 .. min maxActionPoints apStart - 1] $ \n -> do
        let rect = Rect (13 * n) 0 12 15
        let tint = if n < apUsed then Tint 192 192 0 255 else Tint 0 192 0 255
        tintRect tint rect
        blitStretch (hmeTimePipSprite $ rsrcHealthManaEtc resources) rect

  return (inertView paint)

newStatusEffectsView :: (MonadDraw m) => Resources
                     -> m (View (StatusEffects, Int) b)
newStatusEffectsView resources = do
  let
    paint (se, health) = do
      let blit x i = blitTopleft (rsrcStatusIcons resources ! i) $
                     Point (13 * x) (0 :: Int)
      let blitHarmOrBenefit x _ i (Harmful _) = blit x i
          blitHarmOrBenefit _ _ _ Unaffected = return ()
          blitHarmOrBenefit x i _ (Beneficial _) = blit x i
      blitHarmOrBenefit 0 0 1 (seBlessing se)
      blitHarmOrBenefit 1 2 3 (seDefense se)
      blitHarmOrBenefit 2 4 5 (seHaste se)
      when (sePoison se > 0) $ blit 3 $ if sePoison se >= health then 7 else 6
      maybeM (seMentalEffect se) $ \eff ->
        blit 4 $ case eff of { Dazed -> 8; Confused -> 9; Charmed -> 10 }
      case seInvisibility se of
        NoInvisibility -> return ()
        MinorInvisibility -> blit 5 11
        MajorInvisibility -> blit 5 12
      when (seIsEntangled se) $ blit 6 14
      when (seIsShielded se) $ blit 7 15
  return (inertView paint)

-------------------------------------------------------------------------------
-- Utility functions:

characterKey :: CharacterNumber -> Key
characterKey Character0 = Key1
characterKey Character1 = Key2
characterKey Character2 = Key3
characterKey Character3 = Key4

keyCharacterNumber :: Key -> Maybe CharacterNumber
keyCharacterNumber Key1 = Just Character0
keyCharacterNumber Key2 = Just Character1
keyCharacterNumber Key3 = Just Character2
keyCharacterNumber Key4 = Just Character3
keyCharacterNumber _ = Nothing

translucent :: Tint
translucent = Tint 255 255 255 128

-------------------------------------------------------------------------------
