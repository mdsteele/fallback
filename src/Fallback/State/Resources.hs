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

module Fallback.State.Resources
  (Resources, newResources,
   -- * Creature images
   rsrcCharacterImages, rsrcMonsterImages,
   -- * Doodads
   ProjTag(..), rsrcProj,
   SpriteTag(..), rsrcSprite,
   StripTag(..), rsrcStrip,
   WordTag(..), rsrcWordSprite,
   -- * Fonts
   FontTag(..), rsrcFont,
   -- * GUI graphics
   rsrcCursorsStrip, rsrcDigitsStripBig, rsrcSheetSmallButtons,
   rsrcPaintDigits,
   -- * Health, mana, etc.
   HealthManaEtc(..), rsrcHealthManaEtc,
   -- * Icons
   rsrcAbilityIcon, rsrcItemIcon, rsrcStatusIcons,
   -- * Music
   MusicTag(..), musicPath,
   -- * Sounds
   SoundTag(..), rsrcSound,
   -- * Status decorations
   StatusDecorations(..), rsrcStatusDecorations,
   -- * Terrain
   rsrcTerrainSprite, rsrcTerrainOverlaySprite, rsrcTileset)
where

import Control.Arrow ((***))
import Control.Monad (forM)
import Data.Array (Array, Ix, bounds, listArray, range)

import Fallback.Constants (tileHeight, tileWidth)
import Fallback.Data.Point (IRect, LocSpec, Rect(Rect))
import qualified Fallback.Data.TotalMap as TM
import Fallback.Draw
import Fallback.Sound (Sound, loadSound)
import Fallback.State.Creature
  (CreatureImages(CreatureImages), MonsterType, mtImageRow, mtSize)
import Fallback.State.Simple
  (CharacterClass, CharacterAppearance, CreatureSize(..))
import Fallback.State.Tileset (Tileset, loadTileset)

-------------------------------------------------------------------------------

data Resources = Resources
  { rsrcAbilityIcons :: Sheet,
    rsrcAllCharacterImages ::
      TM.TotalMap (CharacterClass, CharacterAppearance) CreatureImages,
    rsrcAllMonsterImages ::
      TM.TotalMap CreatureSize (Array Int CreatureImages),
    rsrcCursorsStrip :: Strip,
    rsrcDigitsStripBig :: Strip,
    rsrcFonts :: TM.TotalMap FontTag Font,
    rsrcHealthManaEtc :: HealthManaEtc,
    rsrcItemIcons :: Sheet,
    rsrcPaintDigits :: Int -> LocSpec Int -> Paint (),
    rsrcProjs :: TM.TotalMap ProjTag Sprite,
    rsrcSheetSmallButtons :: Sheet,
    rsrcSounds :: TM.TotalMap SoundTag Sound,
    rsrcSprites :: TM.TotalMap SpriteTag Sprite,
    rsrcStatusDecorations :: StatusDecorations,
    rsrcStatusIcons :: Strip,
    rsrcStrips :: TM.TotalMap StripTag Strip,
    rsrcTerrainSheet :: Sheet,
    rsrcTerrainOverlaySheet :: Sheet,
    rsrcTileset :: Tileset,
    rsrcWordSprites :: TM.TotalMap WordTag Sprite }

newResources :: IO Resources
newResources = do
  abilityIcons <- loadSheet "abilities.png" (10, 10)
  charSheet <- loadSheet "characters.png" (24, 4)
  cursors <- loadVStrip "gui/cursors.png" 20
  healthManaEtc <- loadHealthManaEtc
  itemIcons <- loadSheetWithTileSize (34, 34) "items.png"
  sheetSmallButtons <- loadSheet "gui/small-buttons.png" (4, 6)
  fonts <- TM.makeA (uncurry loadFont . fontSpec)
  monsterImages <- TM.makeA loadMonsterImages
  paintDigits <- newDigitPaint
  projStrip <- loadVStrip "doodads/projectiles.png" 5
  spritesSheet <- loadSheet "doodads/sprites.png" (2, 8)
  sounds <- TM.makeA (loadSound . soundPath)
  statusDecorations <- loadStatusDecorations
  statusIcons <- loadVStrip "gui/status-icons.png" 16
  strips <- TM.makeA (uncurry loadVStrip . stripSpec)
  terrainSheet <- loadSheetWithTileSize (tileWidth, tileHeight) "terrain.png"
  terrainOverlaySheet <-
    loadSheetWithTileSize (tileWidth, tileHeight) "terrain-overlays.png"
  tileset <- loadTileset
  digitsWords <- loadTexture "big-digits.png"
  return Resources
    { rsrcAbilityIcons = abilityIcons,
      rsrcAllCharacterImages = TM.make $ \(cls, app) ->
        let row = fromEnum cls * 4 + fromEnum app
        in CreatureImages (charSheet ! (row, 0)) (charSheet ! (row, 1))
                          (charSheet ! (row, 2)) (charSheet ! (row, 3)),
      rsrcAllMonsterImages = monsterImages,
      rsrcDigitsStripBig =
        listArray (0, 9) $ flip map [0..9] $ \n ->
          makeSubSprite (Rect (7 * n) 0 7 9) digitsWords,
      rsrcCursorsStrip = cursors,
      rsrcFonts = fonts,
      rsrcHealthManaEtc = healthManaEtc,
      rsrcItemIcons = itemIcons,
      rsrcPaintDigits = paintDigits,
      rsrcProjs = TM.make ((projStrip !) . projIndex),
      rsrcSheetSmallButtons = sheetSmallButtons,
      rsrcSounds = sounds,
      rsrcSprites = TM.make ((spritesSheet !) . spriteCoords),
      rsrcStatusDecorations = statusDecorations,
      rsrcStatusIcons = statusIcons,
      rsrcStrips = strips,
      rsrcTerrainSheet = terrainSheet,
      rsrcTerrainOverlaySheet = terrainOverlaySheet,
      rsrcTileset = tileset,
      rsrcWordSprites = TM.make (flip makeSubSprite digitsWords . wordRect) }

loadMonsterImages :: CreatureSize -> IO (Array Int CreatureImages)
loadMonsterImages size =
  case size of
    SizeSmall -> loadSmalls
    SizeWide -> load "monsters/wide.png" (2, 1)
    SizeTall -> load "monsters/tall.png" (1, 2)
    SizeHuge -> load "monsters/huge.png" (2, 2)
  where
    load name (w, h) = do
      sheet <- loadSheetWithTileSize (w * tileWidth, h * tileHeight) name
      return $ listToArray $ sheetToList sheet
    loadSmalls = do
      let numSheets = 11 :: Int
      sheets <- forM [0 .. numSheets - 1] $ \num -> do
        loadSheetWithTileSize (tileWidth, tileHeight) $
          "monsters/small" ++ (if num < 10 then "0" else "") ++ show num ++
          ".png"
      let arr = listToArray $ concatMap sheetToList sheets
      return arr
    sheetToList sheet = map make (range $ (fst *** fst) $ bounds sheet) where
      make row = CreatureImages (sheet ! (row, 0)) (sheet ! (row, 1))
                                (sheet ! (row, 2)) (sheet ! (row, 3))
    listToArray list = listArray (0, length list - 1) list

-------------------------------------------------------------------------------

rsrcAbilityIcon :: Resources -> (Int, Int) -> Sprite
rsrcAbilityIcon rsrc coords = rsrcAbilityIcons rsrc ! coords

rsrcCharacterImages :: Resources -> CharacterClass -> CharacterAppearance
                    -> CreatureImages
rsrcCharacterImages rsrc cls app =
  TM.get (cls, app) $ rsrcAllCharacterImages rsrc

rsrcItemIcon :: Resources -> (Int, Int) -> Sprite
rsrcItemIcon rsrc coords = rsrcItemIcons rsrc ! coords

rsrcMonsterImages :: Resources -> MonsterType -> CreatureImages
rsrcMonsterImages rsrc mtype =
  TM.get (mtSize mtype) (rsrcAllMonsterImages rsrc) ! mtImageRow mtype

rsrcTerrainSprite :: Resources -> (Int, Int) -> Sprite
rsrcTerrainSprite rsrc coords = rsrcTerrainSheet rsrc ! coords

rsrcTerrainOverlaySprite :: Resources -> Int -> Int -> Sprite
rsrcTerrainOverlaySprite rsrc row col =
  rsrcTerrainOverlaySheet rsrc ! (row, col)

-------------------------------------------------------------------------------

data HealthManaEtc = HealthManaEtc
  { hmeFocusPipSprite :: Sprite,
    hmeIngredientsSprite :: Sprite,
    hmeLongBarSprite :: Sprite,
    hmeShortBarSprite :: Sprite,
    hmeTimePipSprite :: Sprite }

loadHealthManaEtc :: IO HealthManaEtc
loadHealthManaEtc = do
  texture <- loadTexture "gui/health-mana-etc.png"
  return HealthManaEtc
    { hmeFocusPipSprite = makeSubSprite (Rect 53 30 4 15) texture,
      hmeIngredientsSprite = makeSubSprite (Rect 0 15 80 15) texture,
      hmeLongBarSprite = makeSubSprite (Rect 0 0 80 15) texture,
      hmeShortBarSprite = makeSubSprite (Rect 0 30 52 15) texture,
      hmeTimePipSprite = makeSubSprite (Rect 58 30 12 15) texture }

-------------------------------------------------------------------------------

data StatusDecorations = StatusDecorations
  { sdDefenseSprite :: Sprite,
    sdWeaknessSprite :: Sprite,
    sdSlowSprite :: Sprite,
    sdDazedStrip :: Strip,
    sdConfusedSprite :: Sprite,
    sdCharmedSprite :: Sprite,
    sdEntangledSprite :: Sprite,
    sdMagicShieldSprite :: Sprite }

loadStatusDecorations :: IO StatusDecorations
loadStatusDecorations = do
  texture <- loadTexture "status-decorations.png"
  return StatusDecorations
    { sdDefenseSprite = makeSubSprite (Rect 0 0 4 12) texture,
      sdWeaknessSprite = makeSubSprite (Rect 5 1 7 7) texture,
      sdSlowSprite = makeSubSprite (Rect 21 1 6 25) texture,
      sdDazedStrip = listArray (0, 2) $
        [makeSubSprite (Rect 0 39 13 13) texture,
         makeSubSprite (Rect 0 26 13 13) texture,
         makeSubSprite (Rect 0 13 13 13) texture],
      sdConfusedSprite = makeSubSprite (Rect 14 27 9 12) texture,
      sdCharmedSprite = makeSubSprite (Rect 15 40 7 12) texture,
      sdEntangledSprite = makeSubSprite (Rect 1 53 11 6) texture,
      sdMagicShieldSprite = makeSubSprite (Rect 13 1 7 7) texture }

-------------------------------------------------------------------------------
-- Doodads:

data ProjTag = AcidProj | FireProj | IceProj | StarProj | ArrowProj
  deriving (Bounded, Eq, Ix, Ord)

projIndex :: ProjTag -> Int
projIndex AcidProj = 0
projIndex FireProj = 1
projIndex IceProj = 2
projIndex StarProj = 3
projIndex ArrowProj = 4

rsrcProj :: Resources -> ProjTag -> Sprite
rsrcProj rsrc tag = TM.get tag $ rsrcProjs rsrc

data SpriteTag = MineCartEmptyHorzSprite | MineCartEmptyVertSprite
               | MineCartFullHorzSprite | MineCartFullVertSprite
               | WebbingSprite
  deriving (Bounded, Eq, Ix, Ord)

spriteCoords :: SpriteTag -> (Int, Int)
spriteCoords MineCartEmptyHorzSprite = (0, 0)
spriteCoords MineCartEmptyVertSprite = (0, 1)
spriteCoords MineCartFullHorzSprite = (0, 2)
spriteCoords MineCartFullVertSprite = (0, 3)
spriteCoords WebbingSprite = (1, 0)

rsrcSprite :: Resources -> SpriteTag -> Sprite
rsrcSprite rsrc tag = TM.get tag $ rsrcSprites rsrc

data StripTag = SrpBarrierAura | SrpFireAura | SrpGasAura | SrpIceAura
              | SrpSmokeAura
              | AcidBoom | DarkBoom | EnergyBoom | FireBoom | HealBoom
              | IceBoom | LightBoom | SmokeBoom | SunBoom
              | SlashLeft | SlashRight
  deriving (Bounded, Eq, Ix, Ord)

stripSpec :: StripTag -> (String, Int)
stripSpec SrpBarrierAura = ("doodads/aura-barrier.png", 4)
stripSpec SrpFireAura = ("doodads/aura-fire.png", 4)
stripSpec SrpGasAura = ("doodads/aura-gas.png", 4)
stripSpec SrpIceAura = ("doodads/aura-ice.png", 4)
stripSpec SrpSmokeAura = ("doodads/aura-smoke.png", 4)
stripSpec AcidBoom = ("doodads/boom-acid.png", 8)
stripSpec DarkBoom = ("doodads/boom-dark.png", 8)
stripSpec EnergyBoom = ("doodads/boom-energy.png", 8)
stripSpec FireBoom = ("doodads/boom-fire.png", 8)
stripSpec HealBoom = ("doodads/boom-heal.png", 8)
stripSpec IceBoom = ("doodads/boom-ice.png", 8)
stripSpec LightBoom = ("doodads/boom-light.png", 8)
stripSpec SmokeBoom = ("doodads/boom-smoke.png", 8)
stripSpec SunBoom = ("doodads/boom-sun.png", 8)
stripSpec SlashLeft = ("doodads/slash-left.png", 8)
stripSpec SlashRight = ("doodads/slash-right.png", 8)

rsrcStrip :: Resources -> StripTag -> Strip
rsrcStrip rsrc tag = TM.get tag $ rsrcStrips rsrc

data WordTag = WordBackstab | WordCritical | WordDeath | WordDodge
             | WordFinalBlow | WordKO | WordMiss | WordParry | WordRiposte
  deriving (Bounded, Eq, Ix, Ord)

wordRect :: WordTag -> IRect
wordRect WordBackstab = Rect 0 27 35 9
wordRect WordCritical = Rect 36 27 29 9
wordRect WordDeath = Rect 0 36 23 9
wordRect WordDodge = Rect 23 9 22 9
wordRect WordFinalBlow = Rect 0 18 36 9
wordRect WordKO = Rect 24 36 11 9
wordRect WordMiss = Rect 46 9 17 9
wordRect WordParry = Rect 0 9 22 9
wordRect WordRiposte = Rect 37 27 28 9

rsrcWordSprite :: Resources -> WordTag -> Sprite
rsrcWordSprite rsrc tag = TM.get tag $ rsrcWordSprites rsrc

-------------------------------------------------------------------------------
-- Fonts:

data FontTag = FontChancery14 | FontChancery18 | FontChancery72
             | FontGeorgia10 | FontGeorgia11 | FontGeorgia12 | FontGeorgia14
             | FontGeorgiaBold10 | FontGeorgiaBold11 | FontGeorgiaBold12
             | FontGeorgiaBold14 | FontGeorgiaBold16
             | FontGeorgiaItalic11
  deriving (Bounded, Eq, Ix, Ord)

fontSpec :: FontTag -> (String, Int)
fontSpec FontChancery14 = ("chancery.ttf", 14)
fontSpec FontChancery18 = ("chancery.ttf", 18)
fontSpec FontChancery72 = ("chancery.ttf", 72)
fontSpec FontGeorgia10 = ("georgia.ttf", 10)
fontSpec FontGeorgia11 = ("georgia.ttf", 11)
fontSpec FontGeorgia12 = ("georgia.ttf", 12)
fontSpec FontGeorgia14 = ("georgia.ttf", 14)
fontSpec FontGeorgiaBold10 = ("georgia_b.ttf", 10)
fontSpec FontGeorgiaBold11 = ("georgia_b.ttf", 11)
fontSpec FontGeorgiaBold12 = ("georgia_b.ttf", 12)
fontSpec FontGeorgiaBold14 = ("georgia_b.ttf", 14)
fontSpec FontGeorgiaBold16 = ("georgia_b.ttf", 16)
fontSpec FontGeorgiaItalic11 = ("georgia_i.ttf", 11)

rsrcFont :: Resources -> FontTag -> Font
rsrcFont rsrc tag = TM.get tag $ rsrcFonts rsrc

-------------------------------------------------------------------------------
-- Sounds:

data SoundTag = SndArrow
              | SndBarrier | SndBite | SndBlessing | SndBreath
              | SndBoomBig | SndBoomSmall
              | SndChemicalDamage | SndClaw
              | SndCombatEnd | SndCombatStart
              | SndDie1 | SndDie2
              | SndDoorOpen | SndDoorShut
              | SndDrain
              | SndFireDamage | SndFreeze
              | SndHaste | SndHeal
              | SndHit1 | SndHit2 | SndHit3 | SndHit4
              | SndHurtFemale | SndHurtMale
              | SndIllusion
              | SndLevelUp | SndLever | SndLightning | SndLuminaire
              | SndMineCartStop | SndMineCartTurn
              | SndMiss1 | SndMiss2
              | SndRevive
              | SndShielding | SndSummon | SndSunbeam
              | SndThrow
              | SndUnlock
  deriving (Bounded, Eq, Ix, Ord)

soundPath :: SoundTag -> String
soundPath SndArrow = "arrow-shoot-12.wav"
soundPath SndBarrier = "barrier-cfxr.wav"
soundPath SndBite = "bite-87.wav"
soundPath SndBlessing = "blessing-4.wav"
soundPath SndBreath = "breath-44.wav"
soundPath SndBoomBig = "boom-big-5.wav"
soundPath SndBoomSmall = "boom-small-60.wav"
soundPath SndChemicalDamage = "chemical-damage-88.wav"
soundPath SndClaw = "claw-86.wav"
soundPath SndCombatEnd = "combat-end.wav"
soundPath SndCombatStart = "combat-start.wav"
soundPath SndDie1 = "die1-31.wav"
soundPath SndDie2 = "die2-32.wav"
soundPath SndDoorOpen = "door-open-58.wav"
soundPath SndDoorShut = "door-shut-59.wav"
soundPath SndDrain = "drain-cfxr.wav"
soundPath SndFireDamage = "fire-damage-73.wav"
soundPath SndFreeze = "freeze-75.wav"
soundPath SndHaste = "haste-cfxr.wav"
soundPath SndHeal = "heal-68.wav"
soundPath SndHit1 = "hit1-70.wav"
soundPath SndHit2 = "hit2-72.wav"
soundPath SndHit3 = "hit3-69.wav"
soundPath SndHit4 = "hit4-71.wav"
soundPath SndHurtFemale = "hurt-female-30.wav"
soundPath SndHurtMale = "hurt-male-29.wav"
soundPath SndIllusion = "illusion-52.wav"
soundPath SndLevelUp = "level-up-16.wav"
soundPath SndLever = "lever-94.wav"
soundPath SndLightning = "lightning-43.wav"
soundPath SndLuminaire = "luminaire-53.wav"
soundPath SndMineCartStop = "minecart-stop-cfxr.wav"
soundPath SndMineCartTurn = "minecart-turn-cfxr.wav"
soundPath SndMiss1 = "miss1-2.wav"
soundPath SndMiss2 = "miss2-19.wav"
soundPath SndRevive = "resurrect-24.wav"
soundPath SndShielding = "shielding-51.wav"
soundPath SndSummon = "summon-61.wav"
soundPath SndSunbeam = "sunbeam-25.wav"
soundPath SndThrow = "throw-14.wav"
soundPath SndUnlock = "unlock-9.wav"

rsrcSound :: Resources -> SoundTag -> Sound
rsrcSound rsrc tag = TM.get tag $ rsrcSounds rsrc

-------------------------------------------------------------------------------
-- Music:

data MusicTag = MusicMovementProposition
              | MusicPsychedelicCrater
              | MusicRocket

musicPath :: MusicTag -> String
musicPath MusicMovementProposition = "movement-proposition.wav"
musicPath MusicPsychedelicCrater = "psychedelic-crater.mp3"
musicPath MusicRocket = "rocket.mp3"

-------------------------------------------------------------------------------
