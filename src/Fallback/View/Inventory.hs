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

module Fallback.View.Inventory
  (InventoryState(..), InventoryAction(..), newInventoryView)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow ((&&&))
import qualified Data.IntMap as IntMap
import Data.Ix (range)

import Fallback.Data.Clock (Clock)
import Fallback.Data.Color
import Fallback.Data.Point
import Fallback.Data.TotalMap (tmGet)
import Fallback.Draw
import Fallback.Event
import Fallback.State.Item
import Fallback.State.Party
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcFont, rsrcItemIcon, rsrcSheetEquipButtons)
import Fallback.State.Simple
import Fallback.State.Tags (ItemTag(..))
import Fallback.Utility (flip3, maybeM)
import Fallback.View.Base
import Fallback.View.Dialog (newDialogBackgroundView)
import Fallback.View.Hover
import Fallback.View.Widget

-------------------------------------------------------------------------------

data InventoryState = InventoryState
  { ivsActiveCharacter :: CharacterNumber,
    ivsClock :: Clock,
    ivsParty :: Party }

data InventoryAction = ExchangeItem ItemSlot
                     | UpgradeStats
                     | UseItem ItemSlot

ivsGetCharacter :: InventoryState -> Character
ivsGetCharacter ivs = partyGetCharacter (ivsParty ivs) (ivsActiveCharacter ivs)

-------------------------------------------------------------------------------

newInventoryView :: Resources -> HoverSink Cursor
                 -> Draw z (View InventoryState InventoryAction)
newInventoryView resources cursorSink = do
  itemRef <- newHoverRef Nothing
  let itemSink = hoverSink itemRef
  hoverJunction itemRef <$> compoundViewM [
    (return $ hoverView itemSink Nothing nullView),
    (vmap ivsParty . subView_ (Rect 30 40 250 368) <$>
     newPartyInventoryView resources cursorSink itemSink),
    (subView_ (Rect 300 40 180 368) <$>
     newCharStatsView resources cursorSink itemSink),
    (newItemInfoView resources itemRef)]

newItemInfoView :: Resources -> HoverRef (Maybe ItemTag)
                -> Draw z (View a b)
newItemInfoView resources itemRef = do
  cache <- newDrawRef Nothing
  let inputFn _ = do
        mbTag <- readHoverRef itemRef
        case mbTag of
          Nothing -> do
            writeDrawRef cache Nothing
            return Nothing
          Just itemTag -> do
            mbCached <- readDrawRef cache
            desc <- do
              case mbCached of
                Just (tag, string) | tag == itemTag -> return string
                _ -> do
                  let string = itemFullDescription itemTag
                  writeDrawRef cache $ Just (itemTag, string)
                  return string
            return (Just desc)
  vmapM inputFn . subView_ (Rect 270 0 240 360) <$>
    (newMaybeView id =<< newTooltipView resources)

-------------------------------------------------------------------------------

newPartyInventoryView :: Resources -> HoverSink Cursor
                      -> HoverSink (Maybe ItemTag)
                      -> Draw z (View Party InventoryAction)
newPartyInventoryView resources cursorSink itemSink = do
  let { rows = 7; cols = 5 }
  topRef <- newDrawRef 0
  let getTopAndLimit party = do
        let items = partyItems party
        let maxKey = if IntMap.null items
                     then 0 else fst (IntMap.findMax items)
        let numRows = (maxKey + cols) `div` cols
        let numFree = numRows * cols - IntMap.size items
        let limit = numRows + if numFree < cols then 1 else 0
        modifyDrawRef topRef (min (limit - rows))
        top <- readDrawRef topRef
        return (top, limit)
  let scrollInputFn party = do
        (top, limit) <- getTopAndLimit party
        return (0, limit, rows, top)
  let newInventorySlot (row, col) =
        let inputFn party = do
              top <- fst <$> getTopAndLimit party
              let idx = col + (top + row) * cols
              return (idx, IntMap.lookup idx (partyItems party))
        in viewMapM inputFn (return . Action) .
           subView_ (Rect (22 + col * 40) (16 + row * 40) 36 36) <$>
           newItemSlotWidget resources cursorSink itemSink
  hoverView cursorSink DefaultCursor <$> compoundViewM [
    (newDialogBackgroundView),
    (return $ vmap (("Level:  " ++) . show . partyLevel) $
     makeLabel (rsrcFont resources FontGeorgia11) blackColor $ \(_, h) ->
       LocTopleft $ Point 24 (h - 64)),
    (return $ vmap (("XP to level up:  " ++) . show . (1000 -) .
                    (`mod` 1000) . partyExperience) $
     makeLabel (rsrcFont resources FontGeorgia11) blackColor $ \(_, h) ->
       LocTopleft $ Point 24 (h - 48)),
    (return $ vmap (("You have " ++) . (++ ".") . showCoins . partyCoins) $
     makeLabel (rsrcFont resources FontGeorgia11) blackColor $ \(_, h) ->
       LocTopleft $ Point 24 (h - 32)),
    (viewMapM scrollInputFn ((Suppress <$) . writeDrawRef topRef) .
     subView (\_ (w, h) -> Rect 22 16 (w - 38) (h - 52)) <$> newScrollZone),
    (fmap compoundView $ mapM newInventorySlot $
     range ((0, 0), (rows - 1, cols - 1)))]

newItemSlotWidget :: Resources -> HoverSink Cursor -> HoverSink (Maybe ItemTag)
                  -> Draw z (View (Int, Maybe ItemTag) InventoryAction)
newItemSlotWidget resources _cursorSink itemSink = do
  let
    paint (_, mbTag) = do
      rect <- canvasRect
      flip3 drawBevelRect 4 rect $ case mbTag
        of Nothing -> Tint 0 0 0 50
           Just _ -> Tint 0 0 0 20
      maybeM mbTag $ \tag -> do
        let sprite = rsrcItemIcon resources $ itemIconCoords tag
        blitLoc sprite $ LocCenter $ rectCenter rect

    handler (idx, _) rect (EvMouseDown pt) = do
      if not (rectContains rect pt) then return Ignore else
        return $ Action $ ExchangeItem $ PartyItemSlot idx
    handler _ _ _ = return Ignore

  button <- do
    let
      buttonPaintFn (column, _) buttonState = do
        let row = case buttonState of
                    ButtonUp -> 0
                    ButtonHover -> 1
                    ButtonDown -> 2
                    ButtonDisabled -> 3
        rect <- canvasRect
        blitStretch ((rsrcSheetEquipButtons resources) ! (row, column)) rect
      buttonInputFn (idx, mbTag) = do
        tag <- mbTag
        case tag of
          PotionItemTag _ -> Just (2, idx)
          _ -> Nothing
    newHoverOnlyView =<< newMaybeView buttonInputFn =<<
      f2map (\(_, idx) () -> UseItem $ PartyItemSlot idx) .
      subView (\_ (_, h) -> Rect 2 (h - 14) 16 16) <$>
      newButton buttonPaintFn (const ReadyButton) [] ()

  return $ hoverView' itemSink (Just . snd) $
    compoundView [View paint handler, button]

-------------------------------------------------------------------------------

newCharStatsView :: Resources -> HoverSink Cursor -> HoverSink (Maybe ItemTag)
                 -> Draw z (View InventoryState InventoryAction)
newCharStatsView resources cursorSink itemSink = do
  let headingFont = rsrcFont resources FontGeorgiaBold11
  let infoFont = rsrcFont resources FontGeorgia11
  let makeInfo str strFn pt gap = compoundView [
        (vmap (const str) $ makeLabel_ infoFont blackColor $
         LocTopright (pt :: IPoint)),
        (vmap strFn $ makeLabel_ infoFont blackColor $
         LocTopright $ pt `pAdd` Point gap 0)]
  let makeStat str stat pt = compoundView [
        (vmap (const str) $ makeLabel_ infoFont blackColor $
         LocTopright (pt :: IPoint)),
        (vmap (show . tmGet stat . fst) $
         makeLabel_ infoFont blackColor $ LocTopright pt'),
        (vmap (bonusFn . tmGet stat . snd) $
         makeLabel_ infoFont (Color 192 0 0) $ LocTopleft pt')]
        where pt' = pt `pAdd` Point 26 0
              bonusFn b = if b > 0 then " + " ++ show b
                          else if b < 0 then " - " ++ show (negate b) else ""
--   let makePlus stat top = newPlusButton resources (IncreaseStat stat) $
--                           LocTopleft $ Point 150 top
  let makeResist str resist pt =
        makeInfo str ((\p -> show (round (100 * (1 - p)) :: Int) ++ "%") .
                      tmGet resist) pt 30
  let makeEquip tint fromEqp toItemTag toSlot left =
        subView_ (Rect left 300 36 36) <$>
        newEquipmentSlotWidget resources cursorSink itemSink tint fromEqp
                               toItemTag toSlot
--   let statPointsFn char = let pts = chrStatPoints char
--                           in if pts /= 0 then Just pts else Nothing
  let upgradeFn ivs = if (chrStatPoints $ ivsGetCharacter ivs) > 0
                      then Just (ivsClock ivs) else Nothing
  hoverView cursorSink DefaultCursor <$> compoundViewM [
    (newDialogBackgroundView),
    (vmap ivsGetCharacter <$> compoundViewM [
       (return $ vmap ((++ "'s Stats") . chrName) $
        makeLabel headingFont blackColor $ \(w, _) ->
          LocMidtop $ Point (w `div` 2) 12),
       (return $ vmap (chrBaseStats &&& bonusStats . chrBonuses) $
        compoundView [
          (makeStat "Strength:" Strength $ Point 80 40),
          (makeStat "Agility:" Agility $ Point 80 58),
          (makeStat "Intellect:" Intellect $ Point 80 76)]),
--        (newMaybeView statPointsFn =<< compoundViewM [
--           (makePlus Strength 42),
--           (makePlus Agility 60),
--           (makePlus Intellect 78),
--           (return $ vmap (("Points to spend:  " ++) . show) $
--            makeLabel_  infoFont (Color 64 64 64) $
--            LocTopleft (Point 35 100 :: IPoint))]),
       (return $ vmap chrResistances $ compoundView [
          (makeResist "Armor:" Armor $ Point 100 138),
          (vmap (const "Resistances:") $ makeLabel_ infoFont blackColor $
           LocTopleft (Point 20 160 :: IPoint)),
          (makeResist "Fire:" ResistFire $ Point 100 178),
          (makeResist "Cold:" ResistCold $ Point 100 196),
          (makeResist "Energy:" ResistEnergy $ Point 100 214),
          (makeResist "Poison/Acid:" ResistChemical $ Point 100 232),
          (makeResist "Mental:" ResistMental $ Point 100 250),
          (makeResist "Stun:" ResistStun $ Point 100 268)])]),
    (newMaybeView upgradeFn =<<
     subView (\_ (w, _) -> Rect 35 100 (w - 70) 22) <$>
     newFlashingTextButton resources "Upgrade Stats" [KeyU] UpgradeStats),
    (compoundViewM [
       (makeEquip (Tint 192 0 0 192) eqpWeapon
                  WeaponItemTag CharWeaponSlot 25),
       (makeEquip (Tint 0 0 192 192) eqpArmor
                  ArmorItemTag CharArmorSlot 72),
       (makeEquip (Tint 0 96 0 192) eqpAccessory
                  AccessoryItemTag CharAccessorySlot 119)])]

newEquipmentSlotWidget :: Resources -> HoverSink Cursor
                       -> HoverSink (Maybe ItemTag) -> Tint
                       -> (Equipment -> Maybe a) -> (a -> ItemTag)
                       -> (CharacterNumber -> ItemSlot)
                       -> Draw z (View InventoryState InventoryAction)
newEquipmentSlotWidget resources _cursorSink itemSink tint fromEqp toItemTag
                       toSlot = do
  let
    paint ivs = do
      rect <- canvasRect
      let mbTag = fromEqp $ chrEquipment $ ivsGetCharacter ivs
      flip3 drawBevelRect 4 rect $
        maybe (Tint 0 0 0 50) (const tint) mbTag
      maybeM mbTag $ \tag -> do
        let sprite = rsrcItemIcon resources $ itemIconCoords $ toItemTag tag
        blitLoc sprite $ LocCenter $ rectCenter rect

    handler ivs rect (EvMouseDown pt) = do
      if not (rectContains rect pt) then return Ignore else
        return $ Action $ ExchangeItem $ toSlot $ ivsActiveCharacter ivs
    handler _ _ _ = return Ignore

    hoverFn = Just . fmap toItemTag . fromEqp . chrEquipment . ivsGetCharacter

  return $ hoverView' itemSink hoverFn $ View paint handler

-------------------------------------------------------------------------------
