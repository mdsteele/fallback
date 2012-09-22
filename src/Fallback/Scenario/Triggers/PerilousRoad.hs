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

module Fallback.Scenario.Triggers.PerilousRoad
  (compilePerilousRoad)
where

import Control.Applicative ((<$>))
import Control.Monad (forM, when)
import Data.Maybe (isNothing)

import Fallback.Constants (framesPerRound)
import Fallback.Data.Color (Tint(Tint))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Resources (SoundTag(..), StripTag(IceBoom))
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags (AreaTag(..), MonsterTag(..))
import Fallback.State.Terrain (positionCenter)
import Fallback.State.Tileset (TileTag(..))

-------------------------------------------------------------------------------

compilePerilousRoad :: Globals -> CompileScenario ()
compilePerilousRoad globals = compileArea PerilousRoad Nothing $ do

  makeExit Holmgare ["ToHolmgare1", "ToHolmgare2"] "FromHolmgare"
  makeExit StoneBridge ["ToStoneBridge"] "FromStoneBridge"
  makeExit IcyConfluence ["ToIcyConfluence"] "FromIcyConfluence"

  onStartDaily 184809 $ do
    addUnlockedDoors globals
    mapM_ (addRemains Bones) =<< lookupTerrainMark "Bones"

  once 480298 (walkIn "JustInside1" `orP` walkIn "JustInside2") $ do
    narrate "FIXME"

  simpleEnemy_ 879920 "ZomA1" Zombie MindlessAI
  simpleEnemy_ 297279 "ZomA2" Zombie MindlessAI
  simpleEnemy_ 400982 "ZomA3" Zombie MindlessAI
  simpleEnemy_ 178721 "GhoulA" Ghoul MindlessAI
  simpleEnemy_ 338940 "WraithA1" Wraith MindlessAI
  simpleEnemy_ 612041 "WraithA2" Wraith MindlessAI

  simpleEnemy_ 740821 "ZomB1" Zombie MindlessAI
  simpleEnemy_ 300100 "ZomB2" Zombie MindlessAI
  simpleEnemy_ 428302 "ZomB3" Zombie MindlessAI
  simpleEnemy_ 485082 "GhoulB1" Ghoul MindlessAI
  simpleEnemy_ 585092 "GhoulB2" Ghoul MindlessAI
  simpleEnemy_ 500192 "WraithB" Wraith MindlessAI

  simpleEnemy_ 984113 "ZomC1" Zombie MindlessAI
  simpleEnemy_ 448020 "ZomC2" Zombie MindlessAI
  simpleEnemy_ 397299 "SkelC1" Skeleton MindlessAI
  simpleEnemy_ 321353 "SkelC2" Skeleton MindlessAI
  simpleEnemy_ 575792 "WraithC" Wraith MindlessAI

  simpleEnemy_ 477201 "GhastD1" Ghast MindlessAI
  simpleEnemy_ 313516 "GhastD2" Ghast MindlessAI
  simpleEnemy_ 297987 "WraithD" Wraith MindlessAI

  uniqueDevice 572098 "RoadSign" signRadius $ \_ _ -> do
    narrate "There's a weather-beaten signpost along the road here.  It\
      \ says:\n\n\
      \      {c}City of Tragorda: 8 mi. S{_}\n\
      \      {c}Sabina Confluence: 4 mi. E{_}\n\
      \      {c}Village of Holmgare: 4 mi. NW{_}"

  (bonemasterKey, bonemasterDead) <-
    scriptedMonster 841042 "Bonemaster" Bonemaster False ImmobileAI

  crystalCounter <- newPersistentVar 672092 (0 :: Int)
  bossBattle <- newScriptedBattle 568009 "BossRoom" $ do
    trigger 575022 periodicP $ do
      (crystalMark, blockMark, columnMark) <- do
        idx <- (`mod` 3) . (1 +) <$> readVar crystalCounter
        writeVar crystalCounter idx
        case idx of
          0 -> return ("CrystalW", "BlockW", "ColumnW")
          1 -> return ("CrystalNE", "BlockNE", "ColumnNE")
          2 -> return ("CrystalSE", "BlockSE", "ColumnSE")
          _ -> fail "bad crystal counter"
      unlessP (walkOn blockMark) $ do
      crystalPos <- demandOneTerrainMark crystalMark
      columnPos <- demandOneTerrainMark columnMark
      addLightningDoodad (Tint 0 128 255 192) crystalPos columnPos
      runePos <- demandOneTerrainMark "Rune"
      mbOccupant <- areaGet (arsOccupant runePos)
      case mbOccupant of
        Nothing -> do
          key <- readVar bonemasterKey
          degradeMonstersSummonedBy (Right key)
          tag <- getRandomElem [Zombie, Skeleton, Ghoul, Wraith]
          let lifetime = framesPerRound * 16
          playSound SndSummon
          mbEntry <- trySummonMonsterNear runePos (Right key) tag lifetime True
          when (isNothing mbEntry) $ fail "Couldn't place monster on rune"
        Just (Right entry) | not (monstIsAlly $ Grid.geValue entry) -> do
          let hitTarget = HitMonster (Grid.geKey entry)
          playSound SndHeal
          healDamage . (:[]) . (,) hitTarget =<< getRandomR 90 110
          playSound SndBlessing
          playSound SndHaste
          alterStatus hitTarget $
            (seApplyHaste $ Beneficial 12) .
            (seApplyBlessing $ Beneficial 15) . sePurgeAllBadEffects
        Just _ -> do
          playSound SndFreeze
          hits <- forM (prectPositions $ expandPosition runePos) $ \pos -> do
            damage <- getRandomR 50 70
            return (HitPosition pos, ColdDamage, damage)
          forkScript $ doExplosionDoodad IceBoom (positionCenter runePos)
          wait 5 >> dealDamage hits
      wait 16

  once 492011 (walkIn "BossRoom") $ do
    setTerrain BasaltGateClosedTile =<< lookupTerrainMark "BossGate"
    narrate "TODO Fight!"
    startScriptedBattle bossBattle

  trigger 490194 (varTrue bonemasterDead) $ do
    setAreaCleared PerilousRoad True
    setTerrain BasaltGateOpenTile =<< lookupTerrainMark "BossGate"
    setTerrain BasaltGateOpenTile =<< lookupTerrainMark "ExitGate"

-------------------------------------------------------------------------------
