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
   -- * Fonts
   FontTag(..), rsrcFont,
   -- * GUI graphics
   rsrcCursorsStrip, rsrcSheetEquipButtons, rsrcPaintDigits,
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

import Data.Array (Array, Ix, listArray)

import Fallback.Data.Point (LocSpec, Rect(Rect))
import Fallback.Data.TotalMap (TotalMap, makeTotalMap, makeTotalMapA, tmGet)
import Fallback.Draw
import Fallback.Sound (Sound, loadSound)
import Fallback.State.Creature (CreatureImages(CreatureImages))
import Fallback.State.Simple
  (CharacterClass, CharacterAppearance, CreatureSize(..))
import Fallback.State.Tileset (Tileset, loadTileset)

-------------------------------------------------------------------------------

data Resources = Resources
  { rsrcAbilityIcons :: Sheet,
    rsrcAllCharacterImages ::
      TotalMap (CharacterClass, CharacterAppearance) CreatureImages,
    rsrcAllMonsterImages :: TotalMap CreatureSize (Array Int CreatureImages),
    rsrcCursorsStrip :: Strip,
    rsrcFonts :: TotalMap FontTag Font,
    rsrcItemIcons :: Sheet,
    rsrcPaintDigits :: Int -> LocSpec Int -> Paint (),
    rsrcProjs :: TotalMap ProjTag Sprite,
    rsrcSheetEquipButtons :: Sheet,
    rsrcSounds :: TotalMap SoundTag Sound,
    rsrcSprites :: TotalMap SpriteTag Sprite,
    rsrcStatusDecorations :: StatusDecorations,
    rsrcStatusIcons :: Strip,
    rsrcStrips :: TotalMap StripTag Strip,
    rsrcTerrainSheet :: Sheet,
    rsrcTerrainOverlaySheet :: Sheet,
    rsrcTileset :: Tileset }

newResources :: IO Resources
newResources = do
  abilityIcons <- loadSheet "abilities.png" (10, 10)
  charSheet <- loadSheet "characters.png" (24, 4)
  cursors <- loadVStrip "gui/cursors.png" 20
  itemIcons <- loadSheet "items.png" (8, 8)
  sheetEquipButtons <- loadSheet "gui/equip-buttons.png" (4, 3)
  fonts <- makeTotalMapA (uncurry loadFont . fontSpec)
  monsterImages <- makeTotalMapA loadMonsterImages
  paintDigits <- newDigitPaint
  projStrip <- loadVStrip "doodads/projectiles.png" 5
  spritesSheet <- loadSheet "doodads/sprites.png" (2, 8)
  sounds <- makeTotalMapA (loadSound . soundPath)
  statusDecorations <- loadStatusDecorations
  statusIcons <- loadVStrip "gui/status-icons.png" 16
  strips <- makeTotalMapA (uncurry loadVStrip . stripSpec)
  terrainSheet <- loadSheet "terrain.png" (50, 12)
  terrainOverlaySheet <- loadSheet "terrain-overlays.png" (6, 8)
  tileset <- loadTileset
  return Resources
    { rsrcAbilityIcons = abilityIcons,
      rsrcAllCharacterImages = makeTotalMap $ \(cls, app) ->
        let row = fromEnum cls * 4 + fromEnum app
        in CreatureImages (charSheet ! (row, 0)) (charSheet ! (row, 1))
                          (charSheet ! (row, 2)) (charSheet ! (row, 3)),
      rsrcAllMonsterImages = monsterImages,
      rsrcCursorsStrip = cursors,
      rsrcFonts = fonts,
      rsrcItemIcons = itemIcons,
      rsrcPaintDigits = paintDigits,
      rsrcProjs = makeTotalMap ((projStrip !) . projIndex),
      rsrcSheetEquipButtons = sheetEquipButtons,
      rsrcSounds = sounds,
      rsrcSprites = makeTotalMap ((spritesSheet !) . spriteCoords),
      rsrcStatusDecorations = statusDecorations,
      rsrcStatusIcons = statusIcons,
      rsrcStrips = strips,
      rsrcTerrainSheet = terrainSheet,
      rsrcTerrainOverlaySheet = terrainOverlaySheet,
      rsrcTileset = tileset }

loadMonsterImages :: CreatureSize -> IO (Array Int CreatureImages)
loadMonsterImages size =
  case size of
    SizeSmall -> load "monsters/small.png" 20
    SizeWide -> load "monsters/wide.png" 1
    SizeTall -> load "monsters/tall.png" 1
    SizeHuge -> load "monsters/huge.png" 1
  where
    load name rows = do
      sheet <- loadSheet name (rows, 4)
      let make row = CreatureImages (sheet ! (row, 0)) (sheet ! (row, 1))
                                    (sheet ! (row, 2)) (sheet ! (row, 3))
      return $ listArray (0, rows - 1) $ map make [0 .. rows - 1]

-------------------------------------------------------------------------------

rsrcAbilityIcon :: Resources -> (Int, Int) -> Sprite
rsrcAbilityIcon rsrc coords = rsrcAbilityIcons rsrc ! coords

rsrcCharacterImages :: Resources -> CharacterClass -> CharacterAppearance
                    -> CreatureImages
rsrcCharacterImages rsrc cls app =
  tmGet (cls, app) $ rsrcAllCharacterImages rsrc

rsrcItemIcon :: Resources -> (Int, Int) -> Sprite
rsrcItemIcon rsrc coords = rsrcItemIcons rsrc ! coords

rsrcMonsterImages :: Resources -> CreatureSize -> Int -> CreatureImages
rsrcMonsterImages rsrc size row = tmGet size (rsrcAllMonsterImages rsrc) ! row

rsrcTerrainSprite :: Resources -> (Int, Int) -> Sprite
rsrcTerrainSprite rsrc coords = rsrcTerrainSheet rsrc ! coords

rsrcTerrainOverlaySprite :: Resources -> Int -> Int -> Sprite
rsrcTerrainOverlaySprite rsrc row col =
  rsrcTerrainOverlaySheet rsrc ! (row, col)

-------------------------------------------------------------------------------

data StatusDecorations = StatusDecorations
  { sdDefenseSprite :: Sprite,
    sdWeaknessSprite :: Sprite,
    sdSlowSprite :: Sprite,
    sdMagicShieldSprite :: Sprite }

loadStatusDecorations :: IO StatusDecorations
loadStatusDecorations = do
  texture <- loadTexture "status-decorations.png"
  return StatusDecorations
    { sdDefenseSprite = makeSubSprite (Rect 0 0 4 12) texture,
      sdWeaknessSprite = makeSubSprite (Rect 5 1 7 7) texture,
      sdSlowSprite = makeSubSprite (Rect 21 1 6 25) texture,
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
rsrcProj rsrc tag = tmGet tag $ rsrcProjs rsrc

data SpriteTag = MineCartEmptyHorzSprite | MineCartEmptyVertSprite
               | MineCartFullHorzSprite | MineCartFullVertSprite
  deriving (Bounded, Eq, Ix, Ord)

spriteCoords :: SpriteTag -> (Int, Int)
spriteCoords MineCartEmptyHorzSprite = (0, 0)
spriteCoords MineCartEmptyVertSprite = (0, 1)
spriteCoords MineCartFullHorzSprite = (0, 2)
spriteCoords MineCartFullVertSprite = (0, 3)

rsrcSprite :: Resources -> SpriteTag -> Sprite
rsrcSprite rsrc tag = tmGet tag $ rsrcSprites rsrc

data StripTag = SrpBarrierAura | SrpFireAura | SrpGasAura | SrpIceAura
              | SrpSmokeAura
              | AcidBoom | DarkBoom | EnergyBoom | FireBoom | HealBoom
              | IceBoom | LightBoom | SunBoom
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
stripSpec SunBoom = ("doodads/boom-sun.png", 8)
stripSpec SlashLeft = ("doodads/slash-left.png", 8)
stripSpec SlashRight = ("doodads/slash-right.png", 8)

rsrcStrip :: Resources -> StripTag -> Strip
rsrcStrip rsrc tag = tmGet tag $ rsrcStrips rsrc

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
rsrcFont rsrc tag = tmGet tag $ rsrcFonts rsrc

-------------------------------------------------------------------------------
-- Sounds:

data SoundTag = SndArrow
              | SndBarrier | SndBite | SndBlessing | SndBreath
              | SndBoomBig | SndBoomSmall
              | SndChemicalDamage | SndClaw
              | SndCombatEnd | SndCombatStart
              | SndDie1 | SndDie2
              | SndDoorOpen | SndDoorShut
              | SndFireDamage | SndFreeze
              | SndHeal
              | SndHit1 | SndHit2 | SndHit3 | SndHit4
              | SndHurtFemale | SndHurtMale
              | SndIllusion
              | SndLevelUp | SndLever | SndLightning | SndLuminaire
              | SndMineCartStop | SndMineCartTurn
              | SndMiss1 | SndMiss2
              | SndShielding | SndSummon | SndSunbeam
              | SndThrow
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
soundPath SndFireDamage = "fire-damage-73.wav"
soundPath SndFreeze = "freeze-75.wav"
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
soundPath SndShielding = "shielding-51.wav"
soundPath SndSummon = "summon-61.wav"
soundPath SndSunbeam = "sunbeam-25.wav"
soundPath SndThrow = "throw-14.wav"

rsrcSound :: Resources -> SoundTag -> Sound
rsrcSound rsrc tag = tmGet tag $ rsrcSounds rsrc

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
