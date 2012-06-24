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

module Fallback.View.Abilities
  (AbilitiesState(..), AbilitiesAction(..), newAbilitiesView)
where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (zipWithM)
import Data.Maybe (isNothing, listToMaybe)

import Fallback.Data.Color
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM (get)
import Fallback.Draw
import Fallback.Scenario.Abilities
  (getAbility, abilityFullDescription, abilityIconCoords)
import Fallback.Scenario.Feats
  (featCastingCost, featDescription, featIconCoords)
import Fallback.State.Action
import Fallback.State.Item (itemIconCoords, itemName, wdFeats)
import Fallback.State.Party
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcAbilityIcon, rsrcFont, rsrcItemIcon)
import Fallback.State.Simple
import Fallback.State.Tags
  (AbilityTag, FeatTag, ItemTag(WeaponItemTag), WeaponItemTag, classAbility,
   featName)
import Fallback.View.Base
import Fallback.View.Dialog (newDialogBackgroundView)
import Fallback.View.Hover
import Fallback.View.Widget

-------------------------------------------------------------------------------

data AbilitiesState = AbilitiesState
  { abilsActiveCharacter :: CharacterNumber,
    abilsInCombat :: Bool,
    abilsMetaAbilityTag :: Maybe FeatTag,
    abilsParty :: Party }

data AbilitiesAction = UseAbility AbilityNumber
                     | UseCombatFeat FeatTag
                     | UseNormalAttack

abilsGetCharacter :: AbilitiesState -> Character
abilsGetCharacter as =
  partyGetCharacter (abilsParty as) (abilsActiveCharacter as)

data TooltipKey = NoTooltip
                | AbilityTooltip AbilityTag AbilityRank
                | AttackTooltip (Maybe WeaponItemTag)
                | FeatTooltip (Maybe WeaponItemTag) FeatTag

-------------------------------------------------------------------------------

buttonSize, buttonSpacing :: Int
buttonSize = 40
buttonSpacing = 8

-------------------------------------------------------------------------------

newAbilitiesView :: (MonadDraw m) => Resources -> HoverSink Cursor
                 -> m (View AbilitiesState AbilitiesAction)
newAbilitiesView resources cursorSink = do
  let margin = 20
      headingHeight = 20
      sectionSpacing = 10
  let headingFont = rsrcFont resources FontGeorgiaBold11
  tooltipRef <- newHoverRef NoTooltip
  let tooltipSink = hoverSink tooltipRef
  let makeAbilityWidget abilNum index =
        subView_ (Rect (margin + (index `mod` 5) *
                                 (buttonSize + buttonSpacing))
                       (margin + headingHeight +
                        (index `div` 5) * (buttonSize + buttonSpacing))
                       buttonSize buttonSize) <$>
        newAbilityWidget resources tooltipSink abilNum
  let rectFn _ (w, h) =
        let w' = 2 * margin + 5 * buttonSize + 4 * buttonSpacing
            h' = 2 * margin + 2 * headingHeight +
                 3 * buttonSize + buttonSpacing + sectionSpacing
        in Rect ((w - w') `div` 2) ((h - h') `div` 2) w' h'
  hoverJunction tooltipRef <$> compoundViewM [
    (return $ hoverView tooltipSink NoTooltip nullView),
    (subView rectFn <$> compoundViewM [
       (hoverView cursorSink DefaultCursor <$> newDialogBackgroundView),
       (return $ vmap (const "Abilities") $ makeLabel headingFont blackColor $
        \(w, _) -> LocMidtop $ Point (w `div` 2) margin),
       (compoundView <$> zipWithM makeAbilityWidget [minBound .. maxBound]
                                                    [0 ..]),
       (subView_ (Rect margin (margin + headingHeight + 2 * buttonSize +
                               buttonSpacing + sectionSpacing)
                       (buttonSize * 2 + buttonSpacing)
                       (buttonSize + headingHeight)) <$>
        newAttackSection resources tooltipSink),
       (subView_ (Rect (margin + 2 * buttonSize + 2 * buttonSpacing)
                       (margin + headingHeight + 2 * buttonSize +
                        buttonSpacing + sectionSpacing)
                       (buttonSize * 3 + buttonSpacing * 2)
                       (buttonSize + headingHeight)) <$>
        newFeatsSection resources tooltipSink)]),
    (newAbilityInfoView resources tooltipRef),
    (newFeatInfoView resources tooltipRef)]

-------------------------------------------------------------------------------

newAbilityWidget :: (MonadDraw m) => Resources -> HoverSink TooltipKey
                 -> AbilityNumber -> m (View AbilitiesState AbilitiesAction)
newAbilityWidget resources tooltipSink abilNum = do
  let maybeFn as = do
        let char = abilsGetCharacter as
        abilRank <- TM.get abilNum $ chrAbilities char
        let abilTag = classAbility (chrClass char) abilNum
        Just ((as, char), (abilTag, abilRank))
  let eitherFn ((as, char), (abilTag, abilRank)) =
        case getAbility (chrClass char) abilNum abilRank of
          ActiveAbility _ cost eff ->
            -- TODO: take AP needed into account when deciding if can use
            Left (abilTag, canUseActiveAbility as cost eff)
          PassiveAbility -> Right abilTag
  active <- newActiveAbilityWidget resources abilNum
  passive <- newPassiveAbilityWidget resources
  newMaybeView maybeFn =<<
    (hoverView' tooltipSink (Just . uncurry AbilityTooltip . snd) <$>
     newEitherView eitherFn active passive)

newActiveAbilityWidget :: (MonadDraw m) => Resources -> AbilityNumber
                       -> m (View (AbilityTag, Bool) AbilitiesAction)
newActiveAbilityWidget resources abilNum =
  vmap (rsrcAbilityIcon resources . abilityIconCoords *** enabledIf) <$>
  newIconButton [] (UseAbility abilNum)

newPassiveAbilityWidget :: (MonadDraw m) => Resources -> m (View AbilityTag b)
newPassiveAbilityWidget resources = do
  let paint abilTag = do
        rect <- canvasRect
        drawBevelRect (Tint 0 0 0 128) 5 rect
        let icon = rsrcAbilityIcon resources $ abilityIconCoords abilTag
        blitLocTinted (Tint 255 255 255 192) icon $
           LocCenter (rectCenter rect :: IPoint)
  return $ inertView paint

-------------------------------------------------------------------------------

newAttackSection :: (MonadDraw m) => Resources -> HoverSink TooltipKey
                 -> m (View AbilitiesState AbilitiesAction)
newAttackSection resources tooltipSink = do
  let headingFont = rsrcFont resources FontGeorgiaBold11
  let inputFn as = (mbTag, abilsInCombat as) where
        mbTag = eqpWeapon $ chrEquipment $
                partyGetCharacter (abilsParty as) (abilsActiveCharacter as)
  let hoverFn (mbTag, _) = Just (AttackTooltip mbTag)
  let buttonFn (mbTag, enabled) =
        (rsrcItemIcon resources $
         maybe (1, 7) (itemIconCoords . WeaponItemTag) mbTag,
         enabledIf enabled)
  let rectFn _ (w, h) =
        Rect ((w - buttonSize) `div` 2) (h - buttonSize) buttonSize buttonSize
  compoundViewM [
    (return $ vmap (const "Attack") $ makeLabel headingFont blackColor $
     \(w, _) -> LocMidtop $ Point (w `div` 2) 0),
    (subView rectFn . vmap inputFn . hoverView' tooltipSink hoverFn .
     vmap buttonFn <$> newIconButton [] UseNormalAttack)]

-------------------------------------------------------------------------------

newFeatsSection :: (MonadDraw m) => Resources -> HoverSink TooltipKey
                -> m (View AbilitiesState AbilitiesAction)
newFeatsSection resources tooltipSink = do
  let headingFont = rsrcFont resources FontGeorgiaBold11
  let inputFn as =
        let char = partyGetCharacter (abilsParty as) (abilsActiveCharacter as)
        in  (as, eqpWeapon (chrEquipment char),
             wdFeats (chrEquippedWeaponData char))
  let rectFn idx (_, _, featTags) (w, h) =
        Rect ((w - length featTags * (buttonSize + buttonSpacing) +
               buttonSpacing) `div` 2 + idx * (buttonSize + buttonSpacing))
             (h - buttonSize) buttonSize buttonSize
  let makeFeatButton idx =
        subView (rectFn idx) <$> newFeatButton resources tooltipSink idx
  compoundViewM [
    (return $ vmap (const "Combat Feats") $ makeLabel headingFont blackColor $
     \(w, _) -> LocMidtop $ Point (w `div` 2) 0),
    (vmap inputFn . compoundView <$> mapM makeFeatButton [0 .. 2])]

newFeatButton :: (MonadDraw m) => Resources -> HoverSink TooltipKey -> Int
              -> m (View (AbilitiesState, Maybe WeaponItemTag,
                          [FeatTag]) AbilitiesAction)
newFeatButton resources tooltipSink idx = do
  let tryGetInput (as, mbWTag, featTags) = do
        featTag <- listToMaybe $ drop idx featTags
        Just ((mbWTag, featTag),
              (rsrcAbilityIcon resources (featIconCoords featTag),
               if abilsMetaAbilityTag as == Just featTag then DepressedButton
               else enabledIf (abilsInCombat as &&
                 partyCanAffordCastingCost (abilsActiveCharacter as)
                   (featCastingCost featTag) (abilsParty as))))
  newMaybeView tryGetInput =<<
    hoverView' tooltipSink (Just . uncurry FeatTooltip . fst) .
    f2map (const . UseCombatFeat . snd . fst) . vmap snd <$>
    newIconButton [] ()

-------------------------------------------------------------------------------

newAbilityInfoView :: (MonadDraw m) => Resources -> HoverRef TooltipKey
                   -> m (View a b)
newAbilityInfoView resources tooltipRef = do
  cache <- newDrawRef Nothing
  let inputFn _ = do
        tooltipKey <- readHoverRef tooltipRef
        case tooltipKey of
          AbilityTooltip tag rank -> do
            let key = (tag, rank)
            mbCached <- readDrawRef cache
            desc <- do
              case mbCached of
                Just (key', string) | key' == key -> return string
                _ -> do
                  let string = uncurry abilityFullDescription key
                  writeDrawRef cache $ Just (key, string)
                  return string
            return (Just desc)
          _ -> do
            writeDrawRef cache Nothing
            return Nothing
  vmapM inputFn . subView (\_ (w, h) -> Rect (half (w - 400)) (half h)
                                             400 (half h)) <$>
    (newMaybeView id =<< newTooltipView resources)

newFeatInfoView :: (MonadDraw m) => Resources -> HoverRef TooltipKey
                -> m (View a b)
newFeatInfoView resources tooltipRef = do
  let inputFn _ = do
        tooltipKey <- readHoverRef tooltipRef
        case tooltipKey of
          AttackTooltip mbTag -> return $ Just $ attackDescription mbTag
          FeatTooltip mbWTag featTag ->
            return $ Just $ featFullDescription mbWTag featTag
          _ -> return Nothing
  let rectFn _ (w, h) = Rect (half (w - 400)) 0 400 (half h)
  tooltip <- newTooltipView resources
  vmapM inputFn <$> newMaybeView id (subView rectFn tooltip)

-------------------------------------------------------------------------------

canUseActiveAbility :: AbilitiesState -> CastingCost -> AbilityEffect -> Bool
canUseActiveAbility abils cost eff =
  partyCanAffordCastingCost (abilsActiveCharacter abils) cost
                            (abilsParty abils) &&
  case eff of { GeneralAbility _ _ -> True; _ -> abilsInCombat abils }

attackDescription :: Maybe WeaponItemTag -> String
attackDescription mbTag =
  "{b}Attack{_}  --  " ++ weaponName mbTag ++ "  --  " ++
  costDescription NoCost ++ "\nAttack with " ++
  (if isNothing mbTag then "your bare hands." else "your equipped weapon.")

featFullDescription :: Maybe WeaponItemTag -> FeatTag -> String
featFullDescription mbWTag featTag =
  "{b}" ++ featName featTag ++ "{_}  --  " ++ weaponName mbWTag ++ "  --  " ++
  costDescription (featCastingCost featTag) ++ "\n" ++ featDescription featTag

weaponName :: Maybe WeaponItemTag -> String
weaponName = maybe "Fists" (itemName . WeaponItemTag)

-------------------------------------------------------------------------------
