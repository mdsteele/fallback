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
import Data.Maybe (listToMaybe)

import Fallback.Data.Color
import Fallback.Data.Point
import Fallback.Data.TotalMap (tmGet)
import Fallback.Draw
import Fallback.Scenario.Abilities
  (getAbility, abilityFullDescription, abilityIconCoords)
import Fallback.Scenario.Feats (getFeat)
import Fallback.State.Action
import Fallback.State.Item
import Fallback.State.Party
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcAbilityIcon, rsrcFont)
import Fallback.State.Simple
import Fallback.State.Tags (AbilityTag, FeatTag, classAbility)
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

data AbilitiesAction = UpgradeSkills
                     | UseAbility AbilityNumber
                     | UseCombatFeat FeatTag

abilsGetCharacter :: AbilitiesState -> Character
abilsGetCharacter as =
  partyGetCharacter (abilsParty as) (abilsActiveCharacter as)

-------------------------------------------------------------------------------

newAbilitiesView :: (MonadDraw m) => Resources -> HoverSink Cursor
                 -> m (View AbilitiesState AbilitiesAction)
newAbilitiesView resources cursorSink = do
  let margin = 20
      buttonSpacing = 8
      buttonSize = 40
      headingHeight = 20
      sectionSpacing = 10
  let headingFont = rsrcFont resources FontGeorgiaBold11
  let infoFont = rsrcFont resources FontGeorgia11
  abilityRef <- newHoverRef Nothing
  let abilitySink = hoverSink abilityRef
  let makeAbilityWidget abilNum index =
        subView_ (Rect (margin + (index `mod` 5) *
                                 (buttonSize + buttonSpacing))
                       (margin + headingHeight +
                        (index `div` 5) * (buttonSize + buttonSpacing))
                       buttonSize buttonSize) <$>
        newAbilityWidget resources abilitySink abilNum
  let newFeatButton index =
        let tryGetInput as = do
              let featTags = wdFeats $ chrEquippedWeaponData $
                             partyGetCharacter (abilsParty as)
                                               (abilsActiveCharacter as)
              featTag <- listToMaybe $ drop index featTags
              let feat = getFeat featTag
              Just ((featTag, length featTags),
                    (rsrcAbilityIcon resources (cfIconCoords feat),
                     if abilsMetaAbilityTag as == Just featTag
                     then DepressedButton
                     else enabledIf (abilsInCombat as &&
                       partyCanAffordCastingCost (abilsActiveCharacter as)
                         (cfCastingCost feat) (abilsParty as))))
            rectFn ((_, numFeats), _) (w, _) =
              Rect ((w - numFeats * (buttonSize + buttonSpacing) +
                     buttonSpacing) `div` 2 +
                    index * (buttonSize + buttonSpacing))
                   (margin + 2 * headingHeight + 2 * buttonSize +
                    buttonSpacing + sectionSpacing)
                   buttonSize buttonSize
        in newMaybeView tryGetInput =<<
           f2map (const . UseCombatFeat . fst . fst) . subView rectFn .
           vmap snd <$> newIconButton [] ()
  let rectFn _ (w, h) =
        let w' = 2 * margin + 5 * buttonSize + 4 * buttonSpacing
            h' = 2 * margin + 2 * headingHeight +
                 3 * buttonSize + buttonSpacing + sectionSpacing
        in Rect ((w - w') `div` 2) ((h - h') `div` 2) w' h'
  let skillPtsFn as =
        let pts = chrSkillPoints $
                  partyGetCharacter (abilsParty as) (abilsActiveCharacter as)
        in if pts == 0 then Left () else Right pts
  hoverJunction abilityRef <$> compoundViewM [
    (return $ hoverView abilitySink Nothing nullView),
    (subView rectFn <$> compoundViewM [
       (hoverView cursorSink DefaultCursor <$> newDialogBackgroundView),
       (newEitherView skillPtsFn
          (vmap (const "Abilities") $
           makeLabel headingFont blackColor $ \(w, _) ->
             LocMidtop $ Point (w `div` 2) margin)
          (compoundView [
             (vmap (const "Abilities") $ makeLabel_ headingFont blackColor $
              LocTopleft $ Point 40 margin),
             (vmap (("Points to spend:  " ++) . show) $
              makeLabel infoFont blackColor $ \(w, _) ->
                LocTopleft $ Point (w - 135) margin)])),
       (compoundView <$> zipWithM makeAbilityWidget [minBound .. maxBound]
                                                    [0 ..]),
       (return $ vmap (const "Combat Feats") $
        makeLabel headingFont blackColor $ \(w, _) -> LocMidtop $
          Point (w `div` 2) (margin + headingHeight + 2 * buttonSize +
                             buttonSpacing + sectionSpacing)),
       (compoundView <$> mapM newFeatButton [0 .. 4])]),
    (newAbilityInfoView resources abilityRef)]

-------------------------------------------------------------------------------

newAbilityWidget :: (MonadDraw m) => Resources
                 -> HoverSink (Maybe (AbilityTag, AbilityLevel))
                 -> AbilityNumber -> m (View AbilitiesState AbilitiesAction)
newAbilityWidget resources abilitySink abilNum = do
  let maybeFn as = do
        let char = abilsGetCharacter as
        level <- tmGet abilNum $ chrAbilities char
        let abilTag = classAbility (chrClass char) abilNum
        Just ((as, char), (abilTag, level))
  let eitherFn ((as, char), (abilTag, level)) =
        case getAbility (chrClass char) abilNum level of
          ActiveAbility cost eff ->
            Left (abilTag, canUseActiveAbility as cost eff)
          PassiveAbility -> Right abilTag
  active <- newActiveAbilityWidget resources abilNum
  passive <- newPassiveAbilityWidget resources
  newMaybeView maybeFn =<< (hoverView' abilitySink (Just . Just . snd) <$>
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

newAbilityInfoView :: (MonadDraw m) => Resources
                   -> HoverRef (Maybe (AbilityTag, AbilityLevel))
                   -> m (View a b)
newAbilityInfoView resources abilRef = do
  cache <- newDrawRef Nothing
  let inputFn _ = do
        mbKey <- readHoverRef abilRef
        case mbKey of
          Nothing -> do
            writeDrawRef cache Nothing
            return Nothing
          Just key -> do
            mbCached <- readDrawRef cache
            desc <- do
              case mbCached of
                Just (key', string) | key' == key -> return string
                _ -> do
                  let string = uncurry abilityFullDescription key
                  writeDrawRef cache $ Just (key, string)
                  return string
            return (Just desc)
  vmapM inputFn . subView (\_ (w, h) -> Rect (half (w - 400)) (half h)
                                             400 (half h)) <$>
    (newMaybeView id =<< newTooltipView resources)

-------------------------------------------------------------------------------

canUseActiveAbility :: AbilitiesState -> CastingCost -> AbilityEffect -> Bool
canUseActiveAbility abils cost eff =
  partyCanAffordCastingCost (abilsActiveCharacter abils) cost
                            (abilsParty abils) &&
  case eff of { GeneralAbility _ _ -> True; _ -> abilsInCombat abils }

-------------------------------------------------------------------------------
