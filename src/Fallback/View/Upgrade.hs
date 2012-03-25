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

module Fallback.View.Upgrade
  (UpgradeState(..), UpgradeAction(..), newUpgradeView)
where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM)

import Fallback.Data.Color
import Fallback.Data.Point
import qualified Fallback.Data.SparseMap as SM
import Fallback.Data.TotalMap (tmGet)
import Fallback.Draw
import Fallback.Event (Key(..))
import Fallback.Scenario.Abilities (abilityIconCoords, abilityMinPartyLevel)
import Fallback.State.Party
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcAbilityIcon, rsrcFont, rsrcSheetEquipButtons)
import Fallback.State.Simple
import Fallback.State.Tags (classAbility)
import Fallback.View.Base
import Fallback.View.Dialog (newDialogBackgroundView)
import Fallback.View.Hover
import Fallback.View.Widget

-------------------------------------------------------------------------------

data UpgradeState = UpgradeState
  { upsActiveCharacter :: CharacterNumber,
    upsParty :: Party,
    upsSpentSkills :: SM.SparseMap (CharacterNumber, AbilityNumber) Int,
    upsSpentStats :: SM.SparseMap (CharacterNumber, Stat) Int }

data UpgradeAction = IncreaseSkill AbilityNumber
                   | DecreaseSkill AbilityNumber
                   | IncreaseStat Stat
                   | DecreaseStat Stat
                   | CancelUpgrades
                   | CommitUpgrades

upsGetCharacter :: UpgradeState -> Character
upsGetCharacter ups = partyGetCharacter (upsParty ups) (upsActiveCharacter ups)

upsStatPointsSpent :: UpgradeState -> Int
upsStatPointsSpent ups =
  sum $ map (\s -> SM.get (upsActiveCharacter ups, s) (upsSpentStats ups))
            [minBound .. maxBound]

upsSkillPointsSpent :: UpgradeState -> Int
upsSkillPointsSpent ups =
  sum $ map (\n -> SM.get (upsActiveCharacter ups, n) (upsSpentSkills ups))
            [minBound .. maxBound]

-------------------------------------------------------------------------------

newUpgradeView :: (MonadDraw m) => Resources -> HoverSink Cursor
               -> m (View UpgradeState UpgradeAction)
newUpgradeView resources _cursorSink = do
  let headingFont = rsrcFont resources FontGeorgiaBold11
  let infoFont = rsrcFont resources FontGeorgia11
  upgradeRef <- newHoverRef Nothing
  let upgradeSink = hoverSink upgradeRef
  let rectFn _ (w, h) =
        let { w' = 272; h' = 320 }
        in Rect (half (w - w')) (half (h - h')) w' h'
  let makeStatWidget stat idx =
        subView (\_ (w, _) -> Rect (half (w - 140)) (40 + 20 * idx) 150 20) <$>
        newStatWidget resources upgradeSink stat
  let statPointsFn ups =
        "Stat points to spend: " ++
        show (chrStatPoints (upsGetCharacter ups) - upsStatPointsSpent ups)
  let makeAbilityWidget abilNum idx =
        subView_ (Rect (20 + 48 * (idx `mod` 5))
                       (140 + 48 * (idx `div` 5)) 40 40) <$>
        newAbilityWidget resources abilNum
  let skillPointsFn ups =
        "Skill points to spend: " ++
        show (chrSkillPoints (upsGetCharacter ups) - upsSkillPointsSpent ups)
  hoverJunction upgradeRef <$> compoundViewM [
    (return $ hoverView upgradeSink Nothing nullView),
    (subView rectFn <$> compoundViewM [
       (newDialogBackgroundView),
       (return $ vmap (("Upgrading " ++) . chrName . upsGetCharacter) $
        makeLabel headingFont blackColor $ \(w, _) ->
          LocMidtop $ Point (w `div` 2) 12),
       (compoundView <$>
        zipWithM makeStatWidget [minBound .. maxBound] [0 ..]),
       (return $ vmap statPointsFn $ makeLabel_ infoFont (Color 64 64 64) $
        LocTopleft $ (Point 70 105 :: IPoint)),
       (compoundView <$>
        zipWithM makeAbilityWidget [minBound .. maxBound] [0 ..]),
       (return $ vmap skillPointsFn $ makeLabel_ infoFont (Color 64 64 64) $
        LocTopleft $ (Point 70 240 :: IPoint)),
       (subView (\_ (_, h) -> Rect 20 (h - 44) 80 24) <$>
        newSimpleTextButton resources "Cancel" [KeyEscape] CancelUpgrades),
       (subView (\_ (w, h) -> Rect (w - 100) (h - 44) 80 24) <$>
        newSimpleTextButton resources "Done" [KeyReturn] CommitUpgrades)]),
    (newStatInfoView resources upgradeRef)]

newStatWidget :: (MonadDraw m) => Resources
              -> HoverSink (Maybe (Either Stat AbilityNumber))
              -> Stat -> m (View UpgradeState UpgradeAction)
newStatWidget resources upgradeSink stat = do
  let infoFont = rsrcFont resources FontGeorgia11
  let str = case stat of
              Strength -> "Strength:"
              Agility -> "Agility:"
              Intellect -> "Intellect:"
  let getStatValue ups =
        (tmGet stat $ chrStats $ upsGetCharacter ups) +
        SM.get (upsActiveCharacter ups, stat) (upsSpentStats ups)
  let plusFn ups =
        if chrStatPoints (upsGetCharacter ups) > upsStatPointsSpent ups
        then Just () else Nothing
  let minusFn ups =
        if SM.get (upsActiveCharacter ups, stat) (upsSpentStats ups) > 0
        then Just () else Nothing
  hoverView upgradeSink (Just $ Left stat) <$> compoundViewM [
    (return $ compoundView [
       (vmap (const str) $ makeLabel_ infoFont blackColor $
        LocTopright (Point 60 1 :: IPoint)),
       (vmap (show . getStatValue) $ makeLabel_ infoFont blackColor $
        LocTopright (Point 86 1 :: IPoint))]),
    (newMaybeView minusFn =<< newMinusButton resources (DecreaseStat stat)
                                             (LocTopleft $ Point 100 0)),
    (newMaybeView plusFn =<< newPlusButton resources (IncreaseStat stat)
                                           (LocTopleft $ Point 120 0))]

newAbilityWidget :: (MonadDraw m) => Resources -> AbilityNumber
                 -> m (View UpgradeState UpgradeAction)
newAbilityWidget resources abilNum = do
  let paintIcon ups = do
        let party = upsParty ups
        let char = partyGetCharacter party (upsActiveCharacter ups)
        let abilTag = classAbility (chrClass char) abilNum
        let mbLevel = tmGet abilNum $ chrAbilities char
        let available =
              mbLevel /= Just maxBound &&
              partyLevel party >=
              abilityMinPartyLevel abilTag (nextAbilityLevel mbLevel)
        let tint = if available then whiteTint else Tint 255 255 255 64
        let icon = rsrcAbilityIcon resources (abilityIconCoords abilTag)
        center <- rectCenter <$> canvasRect
        blitLocTinted tint icon (LocCenter center)
  let plusFn ups =
        let party = upsParty ups
            charNum = upsActiveCharacter ups
            char = partyGetCharacter party charNum
            abilTag = classAbility (chrClass char) abilNum
            spentOn n = SM.get (charNum, n) (upsSpentSkills ups)
            curLevel = abilityLevelPlus (tmGet abilNum $ chrAbilities char)
                                        (spentOn abilNum)
        in if curLevel /= Just maxBound &&
              partyLevel party >=
              abilityMinPartyLevel abilTag (nextAbilityLevel curLevel) &&
              chrSkillPoints char > sum (map spentOn [minBound .. maxBound])
           then Just () else Nothing
  let minusFn ups =
        if SM.get (upsActiveCharacter ups, abilNum) (upsSpentSkills ups) > 0
        then Just () else Nothing
  compoundViewM [
    (return $ inertView paintIcon),
    (newMaybeView minusFn =<< newMinusButton resources (DecreaseSkill abilNum)
                                             (LocBottomleft $ Point 0 40)),
    (newMaybeView plusFn =<< newPlusButton resources (IncreaseSkill abilNum)
                                           (LocBottomright $ Point 40 40))]

newStatInfoView :: (MonadDraw m) => Resources
                -> HoverRef (Maybe (Either Stat AbilityNumber)) -> m (View a b)
newStatInfoView resources upgradeRef = do
  let inputFn _ = do
        mbUpgrade <- readHoverRef upgradeRef
        case mbUpgrade of
          Just (Left stat) -> return $ Just $ statDescription stat
          _ -> return Nothing
  let rectFn _ (w, _) = Rect 100 180 (w - 200) 200
  tooltip <- newTooltipView resources
  vmapM inputFn <$> newMaybeView id (subView rectFn tooltip)

-- newAbilityInfoView :: Resources -> HoverRef (Maybe (Either Stat AbilityNumber))
--                    -> Draw z (View UpgradeState b)
-- newAbilityInfoView resources upgradeRef = do
--   let inputFn ups = do
--         mbUpgrade <- readHoverRef upgradeRef
--         case mbUpgrade of
--           Just (Right abilNum) -> do
--             return $ Just $ ability
--           _ -> return Nothing
--         return Nothing -- FIXME
--   let rectFn _ (w, _) = Rect 50 0 (w - 100) 250
--   tooltip <- newTooltipView resources
--   vmapM inputFn <$> newMaybeView id (subView rectFn tooltip)

{-
newUpgradeInfoView :: Resources -> HoverRef (Maybe (Either Stat AbilityNumber))
                   -> Draw z (View UpgradeState b)
newUpgradeInfoView resources upgradeRef = do
  let inputFn ups = do
        mbUpgrade <- readHoverRef upgradeRef
        return (((,) ups) <$> mbUpgrade)
  let rectFn (_, eith) =
        either (const $ Rect)
               (const $ Rect) eith
  let inputFn _ = do
        mbUpgrade <- readHoverRef upgradeRef
        case mbUpgrade of
          Nothing -> do
            writeDrawRef cache Nothing
            return Nothing
          Just upgrade -> do
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
-}
-- newCachedDrawFn :: (Eq a) => a -> (a -> b) -> Draw z (a -> Draw z b)
-- newCachedDrawFn initial fn = do
--   cache <- newDrawRef (initial, fn initial)
--   return $ \input -> do
--     (input', output') <- readDrawRef cache
--     if input' == input then return output' else do
--     let output = fn input
--     writeDrawRef cache $ Just (input, output)
--     return output

-------------------------------------------------------------------------------

newPlusButton :: (MonadDraw m) => Resources -> b -> LocSpec Int -> m (View a b)
newPlusButton = newPlusMinusButton 0

newMinusButton :: (MonadDraw m) => Resources -> b -> LocSpec Int
               -> m (View a b)
newMinusButton = newPlusMinusButton 1

newPlusMinusButton :: (MonadDraw m) => Int -> Resources -> b -> LocSpec Int
                   -> m (View a b)
newPlusMinusButton col resources value loc =
  subView_ (locRect loc (16, 16)) <$>
  newButton paintFn (const ReadyButton) [] value
  where
    paintFn _ buttonState = do
      let row = case buttonState of
                  ButtonUp -> 0
                  ButtonHover -> 1
                  ButtonDown -> 2
                  ButtonDisabled -> 3
      rect <- canvasRect
      blitStretch ((rsrcSheetEquipButtons resources) ! (row, col)) rect

-------------------------------------------------------------------------------
