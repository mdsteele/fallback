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
  (cameraSize, maxActionPoints, maxAdrenaline, momentsPerActionPoint,
   sidebarWidth, tileHeight, tileWidth)
import Fallback.Data.Color
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM (get)
import Fallback.Draw
import Fallback.Event
import Fallback.State.Area (AreaCommonState(..), arsParty)
import Fallback.State.Camera (camTopleft)
import Fallback.State.Combat
import Fallback.State.Creature (ciLeftStand)
import Fallback.State.Party
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcCharacterImages, rsrcFont, rsrcStatusIcons)
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
-- Conversation view:
{-
newConversationView :: Resources -> HoverSink Cursor
                    -> Draw z (View (Conversation f) SidebarAction)
newConversationView resources cursorSink = do
  compoundViewM [
    (vmap conversationWindows <$> newTalkWindowsView resources cursorSink),
    (newMaybeView conversationQuestion =<<
     newQuestionWindowView resources cursorSink)]

newTalkWindowsView :: Resources -> HoverSink Cursor
                   -> Draw z (View [TalkWindow] SidebarAction)
newTalkWindowsView _resources _cursorSink = do
  let
    paint = mapM_ paintWindow . reverse
    paintWindow tw = do
      let rect = twRect tw
      tintRect (Tint 0 0 64 192) rect
      drawRect whiteTint rect
      drawRect whiteTint (adjustRect1 3 rect)
      flip3 zipWithM_ [0..] (twVisibleText tw) $ \index line -> do
        paintTextLine (Color 224 224 224)
                      (LocTopleft $ Point (6 :: Int) (6 + 14 * index)) line

    handler windows rect (EvMouseDown pt) = do
      let pt' = pt `pSub` rectTopleft rect
      return $ if any (flip rectContains pt' . twRect) windows
               then Action HurryConversation else Ignore
    handler _ _ _ = return Ignore

  return (View paint handler)

newQuestionWindowView :: Resources -> HoverSink Cursor
                      -> Draw z (View QuestionWindow SidebarAction)
newQuestionWindowView _resources _cursorSink = do
  let
    paint qw = do
      let rect = qwRect qw
      tintRect (Tint 0 0 64 192) rect
      drawRect whiteTint rect
      drawRect whiteTint (adjustRect1 3 rect)
      flip3 zipWithM_ [0..] (qwOptions qw) $ \index line -> do
        paintTextLine (Color 224 224 224) -- TODO change based on mouse hover
                      (LocTopleft $ Point (6 :: Int) (6 + 14 * index)) line

    handler _ _ _ = return Ignore -- FIXME respond to clicks

  return (View paint handler)
-}
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
        blitLoc (ciLeftStand $ rsrcCharacterImages resources (chrClass char)
                                                   (chrAppearance char))
                (LocTopright pictureTopright)
  compoundViewM [
    (newCharacterViewBackground charNum),
    (vmap getCharacter <$> compoundViewM [
       (return $ vmap chrName $
        makeLabel_ (rsrcFont resources FontChancery14) blackColor $
        LocTopleft nameTopleft),
       (return $ inertView paintPicture),
       (subView_ (Rect 4 53 52 15) <$> newAdrenalineBarView),
       (subView_ (Rect 4 70 104 12) <$> vmap (chrStatus &&& chrHealth) <$>
        newStatusEffectsView resources)]),
    (vmap (ssParty &&& getCharacter) <$> compoundViewM [
       (subView_ (Rect 4 19 80 15) <$> newHealthBarView),
       (subView_ (Rect 4 36 80 15) <$> newMojoBarView)]),
    (newMaybeView ssCombatState =<< subView_ (Rect 60 53 52 15) <$>
     newTimeBarView charNum)]

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

newHealthBarView :: (MonadDraw m) => m (View (Party, Character) b)
newHealthBarView = do
  paintDigits <- newDigitPaint
  let
    paint (party, char) = do
      rect <- canvasRect
      let fr = fromIntegral (chrHealth char) /
               fromIntegral (chrMaxHealth party char)
      tintRect (Tint 255 0 0 255) (Rect 0 0 (fr * fromIntegral (rectW rect))
                                        (fromIntegral (rectH rect) :: Double))
      drawRect (Tint 128 0 0 255) rect
      paintDigits (chrHealth char) $ LocCenter $ rectCenter rect
  return (inertView paint)

newMojoBarView :: (MonadDraw m) => m (View (Party, Character) b)
newMojoBarView = do
  paintDigits <- newDigitPaint
  let
    paint (party, char) =
      case chrClass char of
        WarriorClass -> paintFocus char
        RogueClass -> paintFocus char
        HunterClass -> paintIngredients party
        AlchemistClass -> paintIngredients party
        ClericClass -> paintMana party char
        MagusClass -> paintMana party char
    paintFocus char = do
      height <- canvasHeight
      let paintFocusPip index = tintRect (Tint 0 0 255 255)
                                         (Rect (6 * index) 0 5 height)
      mapM_ paintFocusPip [0 .. chrMojo char - 1]
    paintIngredients party = do
      let ingredients = partyIngredients party
      let paintIngredient ingredient index = do
            let (row, col) = index `divMod` 4
            let tint = case ingredient of
                          AquaVitae -> Tint 255 255 0 255
                          Naphtha -> Tint 255 128 0 255
                          Limestone -> Tint 0 255 0 255
                          Mandrake -> Tint 128 64 32 255
                          Potash -> Tint 128 0 255 255
                          Brimstone -> Tint 192 0 0 255
                          DryIce -> Tint 0 128 255 255
                          Quicksilver -> Tint 128 128 128 255
            tintRect tint (Rect (16 + 20 * col) (1 + 8 * row) 4 5)
            paintDigits (TM.get ingredient ingredients)
                        (LocTopright $ Point (15 + 20 * col) (8 * row))
      zipWithM_ paintIngredient [minBound .. maxBound] [0 ..]
    paintMana party char = do
      rect <- canvasRect
      let fr = fromIntegral (chrMojo char) /
               fromIntegral (chrMaxMojo party char)
      tintRect (Tint 0 0 255 255) (Rect 0 0 (fr * fromIntegral (rectW rect))
                                        (fromIntegral (rectH rect) :: Double))
      drawRect (Tint 0 0 128 255) rect
      paintDigits (chrMojo char) $ LocCenter $ rectCenter rect
  return (inertView paint)

newAdrenalineBarView :: (MonadDraw m) => m (View Character b)
newAdrenalineBarView = do
  paintDigits <- newDigitPaint
  let
    paint char = do
      rect <- canvasRect
      let fr = fromIntegral (chrAdrenaline char) / fromIntegral maxAdrenaline
      tintRect (Tint 192 192 0 255)
               (Rect 0 0 (fr * fromIntegral (rectW rect))
                     (fromIntegral (rectH rect) :: Double))
      drawRect (Tint 128 128 0 255) rect
      paintDigits (chrAdrenaline char) $ LocCenter $ rectCenter rect
  return (inertView paint)

newTimeBarView :: (MonadDraw m) => CharacterNumber -> m (View CombatState b)
newTimeBarView charNum = do
  paintDigits <- newDigitPaint
  let

    paint cs = do
      let ccs = TM.get charNum (csCharStates cs)
      case csCommander cs of
        Just cc | ccCharacterNumber cc == charNum ->
          paintPips (ccsActionPoints ccs) (ccActionPointsUsed cc)
        _ -> paintBar (ccsMoments ccs)

    paintBar moments = do
      let fr = fromIntegral moments /
               fromIntegral (momentsPerActionPoint * maxActionPoints)
      rect <- canvasRect
      tintRect (Tint 0 200 0 255) (Rect 0 0 (fr * fromIntegral (rectW rect))
                                        (fromIntegral (rectH rect) :: Double))
      drawRect (Tint 0 128 0 255) rect
      paintDigits (moments `div` momentsPerActionPoint)
                  (LocCenter $ rectCenter rect)

    paintPips apStart apUsed = do
      forM_ [0 .. min maxActionPoints apStart - 1] $ \n -> do
        let tint = if n < apUsed then Tint 192 192 0 255 else Tint 0 192 0 255
        tintRect tint (Rect (12 * n) 1 9 9)

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
      maybeM (seMentalEffect se) $ \(eff, _) -> blit 4 $
        case eff of
          DazedEffect -> 8
          ConfusedEffect -> 9
          CharmedEffect -> 10
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

-------------------------------------------------------------------------------
