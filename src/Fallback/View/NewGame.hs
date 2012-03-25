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

module Fallback.View.NewGame
  (NewGameAction(..), NewGameSpec(..), NewCharacterSpec(..), newNewGameView)
where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM)
import Data.Ix (range)

import Fallback.Data.Color
import Fallback.Data.Point
import Fallback.Data.TotalMap
import Fallback.Draw
import Fallback.Event
import Fallback.State.Creature (CreatureImages, ciRightStand)
import Fallback.State.Resources
  (FontTag(..), Resources, rsrcCharacterImages, rsrcFont)
import Fallback.State.Simple
import Fallback.View.Base
import Fallback.View.Dialog (newDialogView)
import Fallback.View.Widget
  (newDynamicTextWrapView, newSimpleTextButton, newTextBox)

-------------------------------------------------------------------------------

data NewGameAction = CancelNewGame | StartNewGame NewGameSpec

data NewCharacterSpec = NewCharacterSpec
  { ncsName :: String,
    ncsClass :: CharacterClass,
    ncsAppearance :: CharacterAppearance }

data NewGameSpec = NewGameSpec
  { ngsDifficulty :: Difficulty,
    ngsCharacters :: TotalMap CharacterNumber NewCharacterSpec }

initialNewGameSpec :: NewGameSpec
initialNewGameSpec = NewGameSpec
  { ngsDifficulty = Casual,
    ngsCharacters = makeTotalMap $ \charNum ->
      case charNum of
        Character0 -> makeNCS "Erika" WarriorClass
        Character1 -> makeNCS "Aedrus" HunterClass
        Character2 -> makeNCS "Daria" ClericClass
        Character3 -> makeNCS "Xoid" MagusClass }
  where makeNCS n c = NewCharacterSpec n c minBound

-------------------------------------------------------------------------------

newNewGameView :: (MonadDraw m) => Resources -> View c d -> c
               -> m (View () NewGameAction)
newNewGameView resources bgView bgInput = do
  dialog <- newNewGameDialog resources
  newDialogView bgView bgInput dialog $ Rect 25 90 590 340 -- 120 90 400 340

newNewGameDialog :: (MonadDraw m) => Resources -> m (View () NewGameAction)
newNewGameDialog resources = do
  stateRef <- newDrawRef (InternalState initialNewGameSpec minBound)
  let handleAction action = do
        state <- readDrawRef stateRef
        writeDrawRef stateRef $ case action of
          SelectCharacter charNum ->
            state { insSelectedCharacter = charNum }
          ChangeName name ->
            insAlterCharacter state $ \ncs -> ncs { ncsName = name }
          ChangeClass cls ->
            insAlterCharacter state $ \ncs -> ncs { ncsClass = cls }
          ChangeAppearance app ->
            insAlterCharacter state $ \ncs -> ncs { ncsAppearance = app }
          SetDifficulty diff ->
            state { insSpec = (insSpec state) { ngsDifficulty = diff } }
        return Suppress
  let newCharRadio charNum i =
        subView_ (Rect 20 (34 + i * 60) 150 54) <$>
        newCharacterRadio resources charNum
  compoundViewM [
    (viewMapM (const $ readDrawRef stateRef) handleAction <$> compoundViewM [
       (compoundView <$> zipWithM newCharRadio [minBound .. maxBound] [0 ..]),
       (subView (\_ (w, h) -> Rect 170 20 (w - 380{-190-}) (h - 70)) <$>
        newEditCharView resources),
       (subView (\_ (w, h) -> Rect (w - 200) 40 180 (h - 110)) <$>
        newDifficultyView resources)]),
    (subView (\_ (_, h) -> Rect 16 (h - 40) 100 24) <$>
     newSimpleTextButton resources "Back to Menu" [KeyEscape] CancelNewGame),
    (viewMapM return (const (Action . StartNewGame . insSpec <$>
                             readDrawRef stateRef)) .
     subView (\_ (w, h) -> Rect (w - 116) (h - 40) 100 24) <$>
     newSimpleTextButton resources "Start Game" [KeyReturn] ())]

-------------------------------------------------------------------------------

data InternalState = InternalState
  { insSpec :: NewGameSpec,
    insSelectedCharacter :: CharacterNumber }

insSelectedCharacterSpec :: InternalState -> NewCharacterSpec
insSelectedCharacterSpec is =
  tmGet (insSelectedCharacter is) $ ngsCharacters $ insSpec is

insAlterCharacter :: InternalState -> (NewCharacterSpec -> NewCharacterSpec)
                  -> InternalState
insAlterCharacter is fn =
  let charNum = insSelectedCharacter is
      ngs = insSpec is
      characters = ngsCharacters ngs
      ncs = tmGet charNum characters
  in is { insSpec = ngs { ngsCharacters = tmSet charNum (fn ncs) characters } }

data InternalAction = SelectCharacter CharacterNumber
                    | ChangeName String
                    | ChangeClass CharacterClass
                    | ChangeAppearance CharacterAppearance
                    | SetDifficulty Difficulty

newCharacterRadio :: (MonadDraw m) => Resources -> CharacterNumber
                  -> m (View InternalState InternalAction)
newCharacterRadio resources charNum = do
  let nameFont = rsrcFont resources FontChancery18
  let classFont = rsrcFont resources FontGeorgiaBold10
  let
    paint state = do
      let charSpec = tmGet charNum $ ngsCharacters $ insSpec state
      blitLoc (ciRightStand $ rsrcCharacterImages resources (ncsClass charSpec)
                                                  (ncsAppearance charSpec))
              (LocTopleft (Point 8 10 :: IPoint))
      drawText nameFont blackColor (LocTopleft (Point 44 14 :: IPoint))
               (ncsName charSpec)
      drawText classFont blackColor (LocBottomleft (Point 48 44 :: IPoint))
               (className $ ncsClass charSpec)
      (w, h) <- canvasSize
      drawLineChain (if insSelectedCharacter state == charNum
                     then blackTint else Tint 0 0 0 40)
                    [Point w 0, Point 5 0, Point 0 5,
                     Point 0 (h - 6), Point 5 (h - 1), Point w (h - 1)]
    handler _ rect (EvMouseDown pt) =
      if not (rectContains rect pt) then return Ignore
      else return $ Action $ SelectCharacter charNum
    handler _ _ _ = return Ignore
  return $ View paint handler

newEditCharView :: (MonadDraw m) => Resources
                -> m (View InternalState InternalAction)
newEditCharView resources = do
  let paintBackground state = do
        (w, h) <- canvasSize
        let startY = 14 + 60 * fromEnum (insSelectedCharacter state)
        drawLineChain blackTint [
          Point 0 startY, Point 0 5, Point 5 0, Point (w - 6) 0,
          Point (w - 1) 5, Point (w - 1) (h - 6), Point (w - 6) (h - 1),
          Point 5 (h - 1), Point 0 (h - 6), Point 0 (startY + 53)]
  let newClsRadio cls (col, row) =
        subView (\_ (w, _) ->
                   let { w' = half (w - 2 * mgn - gap);
                         mgn = 15; gap = 10 }
                   in Rect (mgn + (w' + gap) * col) (50 + 28 * row) w' 20) <$>
        newClassRadio resources cls
  let newAppRadio app i =
        subView (\_ (w, h) -> Rect (15 + ((w - 190) `div` 3 + 40) * i)
                                   (h - 55) 40 40) <$>
        newAppearanceRadio resources app
  let nameFont = rsrcFont resources FontChancery18
  compoundViewM [
    (return $ inertView paintBackground),
    (subView (\_ (w, _) -> Rect 30 15 (w - 60) 20) .
     viewMap (ncsName . insSelectedCharacterSpec) ChangeName <$>
     newTextBox resources (return . (<= 100) . textRenderWidth nameFont)),
    (compoundView <$> zipWithM newClsRadio [minBound .. maxBound]
                                           (range ((0, 0), (1, 2)))),
    (subView (\_ (w, h) -> Rect 15 135 (w - 30) (h - 190)) .
     vmap (classDescription . ncsClass . insSelectedCharacterSpec) <$>
     newDynamicTextWrapView resources),
    (compoundView <$> zipWithM newAppRadio [minBound .. maxBound] [0 ..])]

newClassRadio :: (MonadDraw m) => Resources -> CharacterClass
              -> m (View InternalState InternalAction)
newClassRadio resources cls = do
  let font1 = rsrcFont resources FontGeorgia14
  let font2 = rsrcFont resources FontGeorgiaBold14
  let
    paint state = do
      let ncs = insSelectedCharacterSpec state
      let selected = ncsClass ncs == cls
      rect <- canvasRect
      drawText (if selected then font2 else font1)
               (if selected then Color 192 0 0 else blackColor)
               (LocCenter $ rectCenter rect) (className cls)
      drawBevelRect (if selected then Tint 192 32 32 255 else Tint 0 0 0 40)
                    3 rect
    handler _ rect (EvMouseDown pt) =
      if not (rectContains rect pt) then return Ignore else
        return $ Action $ ChangeClass cls
    handler _ _ _ = return Ignore
  return $ View paint handler

newAppearanceRadio :: (MonadDraw m) => Resources -> CharacterAppearance
                   -> m (View InternalState InternalAction)
newAppearanceRadio resources appear = do
  let
    paint state = do
      let ncs = insSelectedCharacterSpec state
      let sprite = ciRightStand $
                   rsrcCharacterImages resources (ncsClass ncs) appear
      rect <- canvasRect
      blitLoc sprite $ LocCenter $ rectCenter rect
      drawBevelRect (if ncsAppearance ncs == appear
                     then Tint 192 32 32 255 else Tint 0 0 0 40) 5 rect
    handler _ rect (EvMouseDown pt) =
      if not (rectContains rect pt) then return Ignore else
        return $ Action $ ChangeAppearance appear
    handler _ _ _ = return Ignore
  return $ View paint handler

newDifficultyView :: (MonadDraw m) => Resources
                  -> m (View InternalState InternalAction)
newDifficultyView resources = do
  let font = rsrcFont resources FontGeorgiaBold14
  let paintBackground _ = do
        rect <- canvasRect
        drawBevelRect (Tint 0 0 0 80) 5 rect
        drawText font blackColor (LocCenter $ Point (half $ rectW rect) 15)
                 "Difficulty"
  let newDiffRadio diff row =
        subView (\_ (w, _) -> Rect 30 (35 + 20 * row) (w - 60) 16) <$>
        newDifficultyRadio resources diff
  compoundViewM [
    (return $ inertView paintBackground),
    (compoundView <$> zipWithM newDiffRadio [minBound .. maxBound] [0..]),
    (subView (\_ (w, h) -> Rect 10 125 (w - 20) (h - 140)) .
     vmap (difficultyDescription . ngsDifficulty . insSpec) <$>
     newDynamicTextWrapView resources)]

newDifficultyRadio :: (MonadDraw m) => Resources -> Difficulty
                   -> m (View InternalState InternalAction)
newDifficultyRadio resources diff = do
  let font1 = rsrcFont resources FontGeorgia11
  let font2 = rsrcFont resources FontGeorgiaBold12
  let
    paint state = do
      let selected = ngsDifficulty (insSpec state) == diff
      rect <- canvasRect
      drawText (if selected then font2 else font1)
               (if selected then Color 192 0 0 else blackColor)
               (LocCenter $ rectCenter rect) (difficultyName diff)
      drawBevelRect (if selected then Tint 192 32 32 255 else Tint 0 0 0 40)
                    3 rect
    handler _ rect (EvMouseDown pt) =
      if not (rectContains rect pt) then return Ignore else
        return $ Action $ SetDifficulty diff
    handler _ _ _ = return Ignore
  return $ View paint handler

-------------------------------------------------------------------------------

classDescription :: CharacterClass -> String
classDescription WarriorClass =
  "Warriors are trained in melee combat, and can give or take enormous\
  \ punishment."
classDescription RogueClass =
  "Rogues can sneak by or confuse their enemies, then strike from the shadows\
  \ with blades or bows."
classDescription HunterClass =
  "Hunters are highly skilled with bows, and can summon wild creatures to\
  \ their aid."
classDescription AlchemistClass =
  "Alchemists can cast offensive and defensive spells by mixing together\
  \ different ingredients."
classDescription ClericClass =
  "Clerics can use their protective magic to heal and support their allies or\
  \ to banish the undead."
classDescription MagusClass =
  "Magi are physically weak, but can call down lightning, ice, and acid upon\
  \ their foes."

difficultyDescription :: Difficulty -> String
difficultyDescription Casual =
  "An easier game, perfect for new players or those who just want to\
  \ experience the storyline with minimal frustration."
difficultyDescription NotSoEasy =
  "A comfortable challenge, aimed at those familiar with RPGs.  Don't expect\
  \ to survive every fight on the first try."
difficultyDescription QuiteHard =
  "A strenuous journey, meant for experienced players.  You'll need clever\
  \ strategies to win; brute force won't cut it.  Expect to die a lot."
difficultyDescription Ruthless =
  "A mercilessly difficult game.  It isn't just hard; it's completely\
  \ unfair.  Don't say you weren't warned."

-------------------------------------------------------------------------------
