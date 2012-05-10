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

module Fallback.Mode.NewGame (newNewGameMode) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Data.IntMap as IntMap (fromList)
import qualified Data.Map as Map (empty)
import qualified Data.Set as Set (empty)

import Fallback.Control.Error (IOEO, onlyIO)
import qualified Fallback.Data.SparseMap as SM (make)
import Fallback.Data.TotalMap (makeTotalMap)
import Fallback.Draw (handleScreen, paintScreen)
import Fallback.Event
import Fallback.Mode.Base
import Fallback.Mode.Dialog (newHorizontalDialogMode, newQuitWithoutSavingMode)
import Fallback.Mode.Error (popupIfErrors)
import Fallback.Scenario.Areas
  (enterPartyIntoArea, startingArea, startingPosition)
import Fallback.Scenario.Triggers (initialProgress)
import Fallback.State.Party
import Fallback.State.Resources (Resources)
import Fallback.State.Simple
import Fallback.State.Status (initStatusEffects)
import Fallback.State.Tags
import Fallback.State.Town (TownState)
import Fallback.View (View, fromAction, viewHandler, viewPaint)
import Fallback.View.NewGame

-------------------------------------------------------------------------------

newNewGameMode :: Resources -> Modes -> Mode -> View a b -> a -> IO Mode
newNewGameMode resources modes prevMode bgView bgInput = do
  view <- newNewGameView resources bgView bgInput
  let mode EvQuit = do
        ChangeMode <$> newQuitWithoutSavingMode resources mode view ()
      mode event = do
        action <- handleScreen $ viewHandler view () event
        when (event == EvTick) $ paintScreen (viewPaint view ())
        case fromAction action of
          Nothing -> return SameMode
          Just CancelNewGame ->
            ChangeMode <$> newDiscardPartyMode resources prevMode mode view ()
          Just (StartNewGame spec) -> do
            popupIfErrors resources view () (return mode)
                          (newGameTownState resources spec) $ \ts -> do
              ChangeMode <$> newTownMode' modes ts
  return mode

newDiscardPartyMode :: Resources -> Mode -> Mode -> View a b -> a -> IO Mode
newDiscardPartyMode resources menuMode prevMode bgView bgInput =
  newHorizontalDialogMode resources text buttons nextMode bgView bgInput where
    text = "Are you sure you want to discard this party and return to the\
           \ main menu?"
    buttons = [("Discard", [KeyReturn], True), ("Cancel", [KeyEscape], False)]
    nextMode d = if d then return (ChangeMode menuMode)
                 else return (ChangeMode prevMode)

-------------------------------------------------------------------------------

newGameTownState :: Resources -> NewGameSpec -> IOEO TownState
newGameTownState resources spec = do
  party <- onlyIO $ newParty spec
  enterPartyIntoArea resources party (partyCurrentArea party) startingPosition

initCharacter :: NewCharacterSpec -> Character
initCharacter spec = Character
  { chrAbilities = makeTotalMap initRank,
    chrAdrenaline = 0,
    chrAppearance = ncsAppearance spec,
    chrBaseStats = makeTotalMap startingStat,
    chrClass = cls,
    chrEquipment = Equipment { eqpWeapon = Just weapon, eqpArmor = Nothing,
                               eqpAccessory = Nothing },
    chrHealth = 0,
    chrMojo = 0,
    chrName = ncsName spec,
    chrSkillPoints = 0,
    chrStatPoints = 0,
    chrStatus = initStatusEffects }
  where
    cls = ncsClass spec
    weapon =
      case cls of
        WarriorClass -> Shortsword
        RogueClass -> Dagger
        HunterClass -> Shortbow
        AlchemistClass -> Quarterstaff
        ClericClass -> Dagger
        MagusClass -> Dagger
    initRank Ability0 = Just Rank1
    initRank _ = Nothing
    startingStat stat =
      case (cls, stat) of
        (WarriorClass, Strength) -> 27
        (WarriorClass, Agility) -> 15
        (WarriorClass, Intellect) -> 13
        (RogueClass, Strength) -> 17
        (RogueClass, Agility) -> 24
        (RogueClass, Intellect) -> 14
        (HunterClass, Strength) -> 13
        (HunterClass, Agility) -> 23
        (HunterClass, Intellect) -> 19
        (AlchemistClass, Strength) -> 24
        (AlchemistClass, Agility) -> 11
        (AlchemistClass, Intellect) -> 20
        (ClericClass, Strength) -> 17
        (ClericClass, Agility) -> 10
        (ClericClass, Intellect) -> 28
        (MagusClass, Strength) -> 8
        (MagusClass, Agility) -> 16
        (MagusClass, Intellect) -> 31

newParty :: NewGameSpec -> IO Party
newParty spec = do
  let characters = initCharacter <$> ngsCharacters spec
  let numHAs = numIngredientUsers characters
  let party = Party
        { partyCharacters = characters,
          partyClearedAreas = Set.empty,
          partyCoins = 250,
          partyCurrentArea = startingArea,
          partyDifficulty = ngsDifficulty spec,
          partyExperience = 1900,
          partyExploredMaps = Map.empty,
          partyFoundAreas = Set.empty,
          partyIngredients =
            makeTotalMap ((numHAs *) . ingredientStartQuantity),
          partyItems = IntMap.fromList $ zip [0..] ([PotionItemTag HealingTincture, ArmorItemTag AdamantPlate, AccessoryItemTag TitanFists] ++ map WeaponItemTag [Sunrod, Starspear, Moonbow, Lifeblade, Longbow]), -- FIXME
          partyLevel = 1,
          partyProgress = initialProgress,
          partyQuests = SM.make QuestUntaken }
  let healChar char = char { chrHealth = chrMaxHealth party char,
                             chrMojo = chrMaxMojo party char }
  return party { partyCharacters = fmap healChar (partyCharacters party) }
  where
    ingredientStartQuantity AquaVitae = 20
    ingredientStartQuantity Naphtha = 20
    ingredientStartQuantity Limestone = 10
    ingredientStartQuantity Mandrake = 10
    ingredientStartQuantity Potash = 5
    ingredientStartQuantity Brimstone = 5
    ingredientStartQuantity DryIce = 0
    ingredientStartQuantity Quicksilver = 0

-------------------------------------------------------------------------------
