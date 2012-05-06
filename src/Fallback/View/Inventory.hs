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
  (InventoryState, makeInventoryState, InventoryAction(..), newInventoryView,
   ShoppingState, makeShoppingState, ShoppingAction(..), newShoppingView)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow ((&&&))
import qualified Data.IntMap as IntMap
import Data.Ix (range)
import Data.Maybe (isNothing, listToMaybe)

import Fallback.Constants (cameraHeight, cameraWidth, maxAdrenaline)
import Fallback.Data.Clock (Clock)
import Fallback.Data.Color
import Fallback.Data.Point
import Fallback.Data.TotalMap (tmGet)
import Fallback.Draw
import Fallback.Event
import Fallback.State.Area (AreaState, arsClock, arsParty)
import Fallback.State.Item
import Fallback.State.Party
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcDigitsStripBig, rsrcFont, rsrcItemIcon,
   rsrcSheetEquipButtons)
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
    ivsHolding :: Maybe ItemTag,
    ivsParty :: Party }

makeInventoryState :: (AreaState a) => a -> CharacterNumber -> Maybe ItemTag
                   -> InventoryState
makeInventoryState ars charNum mbTag = InventoryState
  { ivsActiveCharacter = charNum,
    ivsClock = arsClock ars,
    ivsHolding = mbTag,
    ivsParty = arsParty ars }

ivsGetCharacter :: InventoryState -> Character
ivsGetCharacter ivs = partyGetCharacter (ivsParty ivs) (ivsActiveCharacter ivs)

data InventoryAction = ExchangeItem ItemSlot
                     | UpgradeStats
                     | UseItem ItemSlot
                     | DoneInventory

data ShoppingState = ShoppingState
  { spsForSale :: [Either Ingredient ItemTag],
    spsInventory :: InventoryState }

makeShoppingState :: (AreaState a) => a -> CharacterNumber -> Maybe ItemTag
                  -> [Either Ingredient ItemTag] -> ShoppingState
makeShoppingState ars charNum mbTag forsale = ShoppingState
  { spsForSale = forsale, spsInventory = makeInventoryState ars charNum mbTag }

data ShoppingAction = BuyIngredient Int Ingredient
                    | BuyItem ItemTag
                    | SellItem ItemSlot
                    | SwapItem ItemSlot
                    | DoneShopping

-------------------------------------------------------------------------------

data Tooltip = NoTooltip
             | ItemTooltip Bool ItemTag
             | IngredientTooltip Bool Ingredient

col1Width, col2Width, colSpacing, colHeight :: Int
col1Width = 250
col2Width = 220
colSpacing = 10
colHeight = 436

sideMargin, topMargin, col2Left :: Int
sideMargin = (cameraWidth - col1Width - colSpacing - col2Width) `div` 2
topMargin = (cameraHeight - colHeight) `div` 2
col2Left = sideMargin + col1Width + colSpacing

partyInventoryRect :: IRect
partyInventoryRect = Rect sideMargin topMargin col1Width colHeight

-------------------------------------------------------------------------------

newInventoryView :: (MonadDraw m) => Resources -> HoverSink Cursor
                 -> m (View InventoryState InventoryAction)
newInventoryView resources cursorSink = do
  tooltipRef <- newHoverRef NoTooltip
  let tooltipSink = hoverSink tooltipRef
  hoverJunction tooltipRef <$> compoundViewM [
    (return $ hoverView cursorSink DefaultCursor $
     hoverView tooltipSink NoTooltip nullView),
    (vmap ivsParty . subView_ partyInventoryRect <$>
     newPartyInventoryView resources cursorSink tooltipSink),
    (subView_ (Rect col2Left topMargin col2Width colHeight) <$>
     newCharStatsView resources cursorSink tooltipSink),
    (newItemInfoView resources tooltipRef),
    (return $ hoverView' cursorSink (fmap ItemCursor . ivsHolding) nullView)]

newItemInfoView :: (MonadDraw m) => Resources -> HoverRef Tooltip
                -> m (View a b)
newItemInfoView resources tooltipRef = do
  cache <- newDrawRef Nothing
  let inputFn _ = do
        tooltip <- readHoverRef tooltipRef
        case tooltip of
          NoTooltip -> do
            writeDrawRef cache Nothing
            return Nothing
          IngredientTooltip fromShop ingredient -> do
            writeDrawRef cache Nothing
            return $ Just (fromShop, ingredientDescription ingredient)
          ItemTooltip fromShop itemTag -> do
            mbCached <- readDrawRef cache
            desc <- do
              case mbCached of
                Just (tag, string) | tag == itemTag -> return string
                _ -> do
                  let string = itemFullDescription itemTag
                  writeDrawRef cache $ Just (itemTag, string)
                  return string
            return $ Just (fromShop, desc)
  let rectFn (fromShop, _) (_, h) =
        if fromShop then Rect 30 0 240 h else Rect 270 0 240 h
  vmapM inputFn . maybeView id . subView rectFn . vmap snd <$>
    newTooltipView resources

-------------------------------------------------------------------------------

newShoppingView :: (MonadDraw m) => Resources -> HoverSink Cursor
                -> m (View ShoppingState ShoppingAction)
newShoppingView resources cursorSink = do
  tooltipRef <- newHoverRef NoTooltip
  let tooltipSink = hoverSink tooltipRef
  hoverJunction tooltipRef <$> compoundViewM [
    (return $ hoverView cursorSink DefaultCursor $
     hoverView tooltipSink NoTooltip nullView),
    (vmap (ivsParty . spsInventory) . subView_ partyInventoryRect <$>
     newPartyInventoryView resources cursorSink tooltipSink),
    (subView_ (Rect col2Left topMargin col2Width 94) <$>
     newCharEquipmentView resources cursorSink tooltipSink),
    (subView_ (Rect col2Left (topMargin + 104) col2Width (colHeight - 104)) <$>
     newShopkeeperView resources tooltipSink),
    (newItemInfoView resources tooltipRef),
    (return $ hoverView' cursorSink (fmap ItemCursor . ivsHolding .
                                     spsInventory) nullView)]

-------------------------------------------------------------------------------

class ItemsAction a where
  swapAction :: ItemSlot -> a
  useAction :: ItemSlot -> ItemTag -> Maybe (Int, a)

instance ItemsAction InventoryAction where
  swapAction = ExchangeItem
  useAction slot (PotionItemTag _) = Just (2, UseItem slot)
  useAction _ _ = Nothing

instance ItemsAction ShoppingAction where
  swapAction = SwapItem
  useAction slot tag =
    case itemValue tag of CanSell _ -> Just (3, SellItem slot)
                          CannotSell -> Nothing

newPartyInventoryView :: (MonadDraw m, ItemsAction a) => Resources
                      -> HoverSink Cursor -> HoverSink Tooltip
                      -> m (View Party a)
newPartyInventoryView resources cursorSink tooltipSink = do
  let { rows = 6; cols = 5 }
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
  let newInventorySlot (row, col) = do
        let inputFn party = do
              top <- fst <$> getTopAndLimit party
              let idx = col + (top + row) * cols
              return (PartyItemSlot idx, IntMap.lookup idx (partyItems party))
        viewMapM inputFn (return . Action) .
          subView_ (Rect (22 + col * 40) (36 + row * 40) 36 36) <$>
          newItemSlotWidget resources cursorSink tooltipSink
  let levelAndXpTop = 36 + rows * 40 + 10
  let newIngredientSlot ingredient = do
        let idx = fromEnum ingredient
        subView (\_ (w, _) -> Rect (40 * (idx `mod` 4) + (w - 156) `div` 2)
                                   (40 * (idx `div` 4) + levelAndXpTop + 50)
                                   36 36) <$>
          newIngredientSlotWidget resources tooltipSink ingredient
  compoundViewM [
    (newDialogBackgroundView),
    (newTopLabel resources (const "Party Inventory")),
    (return $ vmap (("Level:  " ++) . show . partyLevel) $
     makeLabel_ (rsrcFont resources FontGeorgia11) blackColor $
     LocTopleft $ Point 24 levelAndXpTop),
    (return $ vmap (("XP to level up:  " ++) . show . (1000 -) .
                    (`mod` 1000) . partyExperience) $
     makeLabel_ (rsrcFont resources FontGeorgia11) blackColor $
     LocTopleft $ Point 110 levelAndXpTop),
    (return $ vmap (("You have " ++) . (++ ".") . showCoins . partyCoins) $
     makeLabel_ (rsrcFont resources FontGeorgia11) blackColor $
     LocTopleft $ Point 24 (levelAndXpTop + 18)),
    (viewMapM scrollInputFn ((Suppress <$) . writeDrawRef topRef) .
     subView (\_ (w, _) -> Rect 22 36 (w - 38) (36 + 40 * (rows - 1))) <$>
     newScrollZone),
    (fmap compoundView $ mapM newInventorySlot $
     range ((0, 0), (rows - 1, cols - 1))),
    (compoundViewM $ map newIngredientSlot [minBound .. maxBound])]

newItemSlotWidget :: (MonadDraw m, ItemsAction a) => Resources
                  -> HoverSink Cursor -> HoverSink Tooltip
                  -> m (View (ItemSlot, Maybe ItemTag) a)
newItemSlotWidget resources cursorSink tooltipSink = do
  let
    paint (_, mbTag) = paintItemSlot resources (Tint 0 0 0 20) mbTag
    handler (slot, _) (EvMouseDown pt) = do
      whenWithinCanvas pt $ return $ Action $ swapAction slot
    handler _ _ = return Ignore
  button <- newItemActionButton resources cursorSink
  return $ hoverView' cursorSink (fmap (const HandCursor) . snd) $
    hoverView' tooltipSink (fmap (ItemTooltip False) . snd) $
    compoundView [View paint handler, button]

newItemActionButton :: (MonadDraw m, ItemsAction a) => Resources
                    -> HoverSink Cursor -> m (View (ItemSlot, Maybe ItemTag) a)
newItemActionButton resources cursorSink = do
  let
    buttonPaintFn (column, _) buttonState = do
      let row = case buttonState of
                  ButtonUp -> 0
                  ButtonHover -> 1
                  ButtonDown -> 2
                  ButtonDisabled -> 3
      rect <- canvasRect
      blitStretch ((rsrcSheetEquipButtons resources) ! (row, column)) rect
    buttonInputFn (slot, mbTag) = do
      tag <- mbTag
      useAction slot tag
  newHoverOnlyView =<< newMaybeView buttonInputFn =<<
    f2map (\(_, action) () -> action) .
    subView (\_ (_, h) -> Rect 2 (h - 18) 16 16) .
    hoverView cursorSink DefaultCursor <$>
    newButton buttonPaintFn (const ReadyButton) [] ()

newIngredientSlotWidget :: (MonadDraw m) => Resources -> HoverSink Tooltip
                        -> Ingredient -> m (View Party a)
newIngredientSlotWidget resources tooltipSink ing = do
  let paint party = do
        let count = tmGet ing $ partyIngredients party
        rect <- canvasRect
        drawBevelRect (Tint 0 0 0 50) 4 rect
        blitLocTinted (if count == 0 then Tint 255 255 255 64 else whiteTint)
                      (rsrcItemIcon resources $ ingredientIconCoords ing)
                      (LocCenter $ rectCenter rect)
        paintNumber (rsrcDigitsStripBig resources) count
                    (LocBottomright $ Point 34 34)
  return $ hoverView tooltipSink (IngredientTooltip False ing) $
    inertView paint

-------------------------------------------------------------------------------

-- | A view for character stats and equipment; used in the inventory view.
newCharStatsView :: (MonadDraw m) => Resources -> HoverSink Cursor
                 -> HoverSink Tooltip
                 -> m (View InventoryState InventoryAction)
newCharStatsView resources cursorSink tooltipSink = do
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
  let makeHeathMana pt = compoundView [
        (vmap (\(s, _, _) -> s) $
         makeLabel_ infoFont blackColor $ LocTopright (pt :: IPoint)),
        (vmap (\(_, c, _) -> show c) $
         makeLabel_ infoFont blackColor $ LocTopright pt'),
        (vmap (\(_, _, m) -> " / " ++ show m) $
         makeLabel_ infoFont blackColor $ LocTopleft pt')]
        where pt' = pt `pAdd` Point 40 0
  let mojoFn (party, char) =
        case chrClass char of
          WarriorClass -> Just ("Focus:", party, char)
          RogueClass -> Just ("Focus:", party, char)
          HunterClass -> Nothing
          AlchemistClass -> Nothing
          ClericClass -> Just ("Mana:", party, char)
          MagusClass -> Just ("Mana:", party, char)
  let makeResist str resist pt =
        makeInfo str ((\p -> show (round (100 * (1 - p)) :: Int) ++ "%") .
                      tmGet resist) pt 30
  let upgradeFn ivs = if (chrStatPoints $ ivsGetCharacter ivs) > 0
                      then Just (ivsClock ivs) else Nothing
  let resistancesTop = 230
  let buttonW = 90
  compoundViewM [
    (newDialogBackgroundView),
    (newTopLabel resources ((++ "'s Equipment") . chrName . ivsGetCharacter)),
    (vmap (ivsActiveCharacter &&& ivsGetCharacter) .
     subView (\_ (w, _) -> Rect 0 36 w 36) <$>
     newEquipmentSlotsView resources cursorSink tooltipSink),
    (return $ vmap ((chrBaseStats &&& bonusStats . chrBonuses) .
                    ivsGetCharacter) $ compoundView [
       (makeStat "Strength:" Strength $ Point 116 90),
       (makeStat "Agility:" Agility $ Point 116 108),
       (makeStat "Intellect:" Intellect $ Point 116 126)]),
    (return $ vmap (ivsParty &&& ivsGetCharacter) $ compoundView [
       (vmap (\(p, c) -> ("Health:", chrHealth c, chrMaxHealth p c)) $
        makeHeathMana $ Point 96 160),
       (maybeView mojoFn $
        vmap (\(s, p, c) -> (s, chrMojo c, chrMaxMojo p c)) $
        makeHeathMana $ Point 96 178),
       (vmap (\(_, c) -> ("Adrenaline:", chrAdrenaline c, maxAdrenaline)) $
        makeHeathMana $ Point 96 196)]),
    (return $ vmap (chrResistances . ivsGetCharacter) $ compoundView [
       (makeResist "Armor:" Armor $ Point 120 $ resistancesTop),
       (vmap (const "Resistances:") $ makeLabel_ infoFont blackColor $
        LocTopleft $ Point 40 $ resistancesTop + 22),
       (makeResist "Fire:" ResistFire $ Point 120 $ resistancesTop + 40),
       (makeResist "Cold:" ResistCold $ Point 120 $ resistancesTop + 58),
       (makeResist "Energy:" ResistEnergy $ Point 120 $ resistancesTop + 76),
       (makeResist "Poison/Acid:" ResistChemical $
        Point 120 $ resistancesTop + 94),
       (makeResist "Mental:" ResistMental $ Point 120 $ resistancesTop + 112),
       (makeResist "Stun:" ResistStun $ Point 120 $ resistancesTop + 130)]),
    (newMaybeView upgradeFn =<<
     subView (\_ (_, h) -> Rect 16 (h - 40) buttonW 24) <$>
     newFlashingTextButton resources "Upgrade Stats" [KeyU] UpgradeStats),
    (subView (\_ (w, h) -> Rect (w - buttonW - 16) (h - 40) buttonW 24) .
     vmap ((,) "Done" . enabledIf . isNothing . ivsHolding) <$>
     newTextButton resources [KeyReturn, KeyEscape] DoneInventory)]

-- | A view for just character equipment; used in the shopping view.
newCharEquipmentView :: (MonadDraw m) => Resources -> HoverSink Cursor
                     -> HoverSink Tooltip
                     -> m (View ShoppingState ShoppingAction)
newCharEquipmentView resources cursorSink tooltipSink = do
  let inputFn = (ivsActiveCharacter &&& ivsGetCharacter) . spsInventory
  compoundViewM [
    (newDialogBackgroundView),
    (vmap inputFn <$> compoundViewM [
      (newTopLabel resources ((++ "'s Equipment") . chrName . snd)),
      (subView (\_ (w, _) -> Rect 0 36 w 36) <$>
       newEquipmentSlotsView resources cursorSink tooltipSink)])]

-- | A view for the three equipment slots.
newEquipmentSlotsView :: (MonadDraw m, ItemsAction a) => Resources
                      -> HoverSink Cursor -> HoverSink Tooltip
                      -> m (View (CharacterNumber, Character) a)
newEquipmentSlotsView resources cursorSink tooltipSink = do
  let { size = 36; spacing = 11 }
  let makeEquip tint fromEqp toItemTag toSlot idx =
        subView (\_ (w, _) -> Rect ((w - size * 3 - spacing * 2) `div` 2 +
                                    idx * (size + spacing)) 0 size size) <$>
        newEquipmentSlotWidget resources cursorSink tooltipSink tint fromEqp
                               toItemTag toSlot
  compoundViewM [
    (makeEquip (Tint 192 0 0 192) eqpWeapon
               WeaponItemTag CharWeaponSlot 0),
    (makeEquip (Tint 0 0 192 192) eqpArmor
               ArmorItemTag CharArmorSlot 1),
    (makeEquip (Tint 0 96 0 192) eqpAccessory
               AccessoryItemTag CharAccessorySlot 2)]

-- | A widget for a single equipment slot.
newEquipmentSlotWidget :: (MonadDraw m, ItemsAction a) => Resources
                       -> HoverSink Cursor -> HoverSink Tooltip -> Tint
                       -> (Equipment -> Maybe b) -> (b -> ItemTag)
                       -> (CharacterNumber -> ItemSlot)
                       -> m (View (CharacterNumber, Character) a)
newEquipmentSlotWidget resources cursorSink tooltipSink tint fromEqp toItemTag
                       toSlot = do
  let
    paint (_, char) = paintItemSlot resources tint $ fmap toItemTag $ fromEqp $
                      chrEquipment char

    handler (charNum, _) (EvMouseDown pt) = do
      whenWithinCanvas pt $ return $ Action $ swapAction $ toSlot charNum
    handler _ _ = return Ignore

    itemTagFn = fmap toItemTag . fromEqp . chrEquipment . snd
    cursorFn = fmap (const HandCursor) . itemTagFn
    tooltipFn = fmap (ItemTooltip False) . itemTagFn

  return $ hoverView' cursorSink cursorFn $
    hoverView' tooltipSink tooltipFn $ View paint handler

-------------------------------------------------------------------------------

-- | The dialog pane containing items for sale.
newShopkeeperView :: (MonadDraw m) => Resources -> HoverSink Tooltip
                  -> m (View ShoppingState ShoppingAction)
newShopkeeperView resources tooltipSink = do
  let rows = 6
  topRef <- newDrawRef 0
  let scrollInputFn sps = do
        top <- readDrawRef topRef
        return (0, length (spsForSale sps), rows, top)
  let newShopSlot row = do
        let inputFn sps = do
              top <- readDrawRef topRef
              return $ listToMaybe $ drop (top + row) $ spsForSale sps
        viewMapM inputFn (return . Action) <$> (newMaybeView id =<<
          subView (\_ (w, _) -> Rect 22 (36 + row * 40) (w - 54) 36) <$>
          newShopItemView resources tooltipSink)
  let buttonWidth = 120
  compoundViewM [
    (newDialogBackgroundView),
    (newTopLabel resources (const "Items for Sale")),
    (viewMapM scrollInputFn ((Suppress <$) . writeDrawRef topRef) .
     subView (\_ (w, _) -> Rect 22 36 (w - 38) (36 + 40 * (rows - 1))) <$>
             newScrollZone),
    (fmap compoundView $ mapM newShopSlot [0 .. rows - 1]),
    (subView (\_ (w, h) -> Rect (w - buttonWidth - 16) (h - 40)
                                buttonWidth 24) <$>
     newSimpleTextButton resources "Done Shopping" [KeyReturn, KeyEscape]
                         DoneShopping)]

newShopItemView :: (MonadDraw m) => Resources -> HoverSink Tooltip
                -> m (View (Either Ingredient ItemTag) ShoppingAction)
newShopItemView resources tooltipSink = do
  let infoFont = rsrcFont resources FontGeorgia11
  let paintIcon eith = do
        rect <- canvasRect
        drawBevelRect (Tint 0 0 0 50) 4 rect
        let sprite = rsrcItemIcon resources $
                     either ingredientIconCoords itemIconCoords eith
        blitLoc sprite $ LocTopleft $ Point 1 (1 :: Int)
  let itemCost tag = case itemValue tag of
                       CanSell cost -> cost
                       CannotSell -> 0
  let hoverFn = Just . either (IngredientTooltip True) (ItemTooltip True)
  hoverView' tooltipSink hoverFn <$> compoundViewM [
    (return $ inertView paintIcon),
    (return $ vmap (either ingredientName itemName) $
     makeLabel_ infoFont blackColor $ LocTopleft $ Point 40 (2 :: Int)),
    (return $ vmap (showCoins . either ingredientCost itemCost) $
     makeLabel_ infoFont blackColor $ LocTopleft $ Point 40 (20 :: Int)),
    (newMaybeView (either Just (const Nothing)) =<<
     f2map (\e () -> BuyIngredient 5 e) .
     subView (\_ (w, _) -> Rect (w - 38) 1 35 16) <$>
     newSimpleTextButton resources "x5" [] ()),
    (f2map (\e () -> either (BuyIngredient 1) BuyItem e) .
     subView (\_ (w, _) -> Rect (w - 38) 19 35 16) <$>
     newSimpleTextButton resources "Buy" [] ())]

-------------------------------------------------------------------------------

paintItemSlot :: Resources -> Tint -> Maybe ItemTag -> Paint ()
paintItemSlot resources tint mbTag = do
  rect <- canvasRect
  flip3 drawBevelRect 4 rect $ if isNothing mbTag then Tint 0 0 0 50 else tint
  maybeM mbTag $ \tag -> do
    let sprite = rsrcItemIcon resources $ itemIconCoords tag
    blitLoc sprite $ LocCenter $ rectCenter rect

ingredientIconCoords :: Ingredient -> (Int, Int)
ingredientIconCoords ing = (0, fromEnum ing)

newTopLabel :: (MonadDraw m) => Resources -> (a -> String) -> m (View a b)
newTopLabel resources stringFn = do
  let headingFont = rsrcFont resources FontGeorgiaBold11
  return $ vmap stringFn $ makeLabel headingFont blackColor $ \(w, _) ->
    LocMidtop $ Point (w `div` 2) 12

-------------------------------------------------------------------------------
