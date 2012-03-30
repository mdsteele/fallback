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

{-# LANGUAGE GADTs, Rank2Types #-}

module Fallback.Scenario.Script
  (module Fallback.Control.Script, FromAreaEffect(..), ToAreaEffect(..),
   emitAreaEffect,
   -- * Script actions

   -- ** Control
   wait, alsoWith, also_, concurrent, concurrent_, concurrentAny,
   whenCombat, unlessCombat, whenDifficulty,

   -- ** Query
   areaGet, lookupMonsterEntry, withMonsterEntry,
   -- *** Group retrieval
   getAllConsciousCharacters, getAllAllyMonsters, getAllEnemyMonsters,
   getAllAllyTargets,
   -- *** Randomization
   getRandomR, getRandomElem, randomPermutation,

   -- ** Actions
   -- *** Movement
   getPartyPosition, setPartyPosition, partyWalkTo, charWalkTo, teleport,
   exitTo, walkMonster, setMonsterTownAI,
   -- *** Attacks
   characterBeginOffensiveAction, characterWeaponAttack,
   characterWeaponInitialAnimation, characterWeaponBaseDamage,
   characterWeaponChooseCritical, characterWeaponHit,
   monsterBeginOffensiveAction, monsterPerformAttack,

   -- ** Effects
   -- *** Damage
   HitTarget(..), dealDamage,
   -- *** Healing/restoration
   healCharacter, healMonster, alterCharacterMana, restoreManaToFull,
   addToCharacterAdrenaline,
   -- *** Status effects
   alterStatus, grantInvisibility, inflictPoison,
   -- *** Other
   grantExperience, removeFields, setFields,
   getTerrainTile, resetTerrain, setTerrain,

   -- ** Animation
   -- *** Camera motion
   shakeCamera,
   -- *** Creature animation
   faceCharacterToward, faceMonsterToward, facePartyToward,
   setCharacterAnim, setMonsterAnim, setPartyAnim,
   getMonsterHeadPos,
   -- *** Doodads
   addBallisticDoodad, addBeamDoodad, addBlasterDoodad, addBoomDoodadAtPoint,
   addBoomDoodadAtPosition, addDeathDoodad, addLightningDoodad,
   addLightWallDoodad, addNumberDoodadAtPoint, addNumberDoodadAtPosition,
   addShockwaveDoodad, doExplosionDoodad,
   addExtendingHookshotDoodad, addExtendedHookshotDoodad,
   addRetractingHookshotDoodad,

   -- ** UI
   -- *** Messages and conversation
   debug, setMessage, narrate,
   forcedChoice, maybeChoice, multiChoice,
   ConvChoice(..), forcedConversationLoop,
   -- *** Mode changes
   startCombat, gameOver,
   -- *** Sound
   playSound, startMusic, stopMusic, fadeOutMusic,

   -- ** Other

   addBasicEnemyMonster, addDevice_, grantAndEquipWeapon, removeDevice,
   replaceDevice, summonAllyMonster, tryAddMonster,

   inflictAllPeriodicDamage,

   -- * Targeting
   aoeTarget, beamTarget, splashTarget, wallTarget)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (right, second)
import Control.Exception (assert)
import Control.Monad
  (foldM, foldM_, forM, forM_, replicateM, replicateM_, unless, when)
import Data.Array (elems, listArray, range)
import qualified Data.Array.ST as STArray
import qualified Data.Foldable as Fold
import Data.List (foldl1')
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import qualified Data.Set as Set (empty)
import System.Random (Random)

import Fallback.Constants
  (framesPerSecond, maxAdrenaline, momentsPerActionPoint, sightRangeSquared)
import Fallback.Control.Script
import Fallback.Data.Color (Tint(Tint, tintAlpha))
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Data.TotalMap (tmGet)
import Fallback.Draw
import Fallback.Scenario.Monsters (getMonsterType)
import Fallback.State.Action (TargetKind(..))
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Item (WeaponData(..))
import Fallback.State.Party
import Fallback.State.Progress (Var)
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags
  (AbilityTag(Recuperation), AreaTag, ItemTag(..), MonsterTag, WeaponItemTag)
import Fallback.State.Terrain
  (TerrainTile, positionCenter, prectRect, terrainMap, tmapGet)
import Fallback.State.Tileset (TileTag, tilesetGet)
import Fallback.Utility
  (ceilDiv, flip3, flip4, groupKey, maybeM, sortKey, square)

-------------------------------------------------------------------------------

class FromAreaEffect f where
  fromAreaEffect :: AreaEffect a -> f a
  isWaitEffect :: f a -> (a -> Script f b) -> Maybe (Script f b)

instance FromAreaEffect AreaEffect where
  fromAreaEffect = id
  isWaitEffect EffWait fn = Just (fn ())
  isWaitEffect _ _ = Nothing

instance FromAreaEffect CombatEffect where
  fromAreaEffect = EffCombatArea
  isWaitEffect (EffCombatArea EffWait) fn = Just (fn ())
  isWaitEffect _ _ = Nothing

instance FromAreaEffect TownEffect where
  fromAreaEffect = EffTownArea
  isWaitEffect (EffTownArea EffWait) fn = Just (fn ())
  isWaitEffect _ _ = Nothing

class ToAreaEffect f where toAreaEffect :: f a -> AreaEffect a
instance ToAreaEffect PartyEffect where
  toAreaEffect = EffAreaCommon . EffAreaParty
instance ToAreaEffect AreaCommonEffect where toAreaEffect = EffAreaCommon
instance ToAreaEffect AreaEffect where toAreaEffect = id

emitAreaEffect :: (ToAreaEffect e, FromAreaEffect f) => e a -> Script f a
emitAreaEffect = emitEffect . fromAreaEffect . toAreaEffect

-------------------------------------------------------------------------------
-- Control:

-- | Pause the script for the given number of frames.
wait :: (FromAreaEffect f) => Int -> Script f ()
wait n = replicateM_ n (emitAreaEffect EffWait)

-- | Run two scripts in parallel, combining their final results with the given
-- function.
alsoWith :: (FromAreaEffect f) => (a -> b -> c) -> Script f a -> Script f b
         -> Script f c
alsoWith oper script1 script2 =
  case execScript script1 of
    ResultFinal value1 -> fmap (value1 `oper`) script2
    ResultEffect eff1 sfn1 ->
      case isWaitEffect eff1 sfn1 of
        Nothing -> do
          value1 <- emitEffect eff1
          alsoWith oper (sfn1 value1) script2
        Just script1' ->
          case execScript script2 of
            ResultFinal value2 -> fmap (`oper` value2) continue1
            ResultEffect eff2 sfn2 ->
              case isWaitEffect eff2 sfn2 of
                Nothing -> do
                  value2 <- emitEffect eff2
                  alsoWith oper continue1 (sfn2 value2)
                Just script2' -> do
                  emitAreaEffect EffWait
                  alsoWith oper script1' script2'
          where continue1 = emitAreaEffect EffWait >> script1'

-- | Run two scripts in parallel.
also_ :: (FromAreaEffect f) => Script f () -> Script f () -> Script f ()
also_ = alsoWith const

-- | Map the script function over the given list, running all the scripts in
-- parallel and returning the list of the final results.
concurrent :: (FromAreaEffect f) => [a] -> (a -> Script f b) -> Script f [b]
concurrent list fn = foldr (alsoWith (:)) (return []) $ map fn list

-- | Map the script function over the given list, running all the scripts in
-- parallel.
concurrent_ :: (FromAreaEffect f) => [a] -> (a -> Script f ()) -> Script f ()
concurrent_ list fn = foldr also_ (return ()) $ map fn list

-- | Map the script function over the given list, running all the scripts in
-- parallel and returning 'True' if any of the scripts returned 'True'.
concurrentAny :: (FromAreaEffect f) => [a] -> (a -> Script f Bool)
              -> Script f Bool
concurrentAny list fn = foldr (alsoWith (||)) (return False) $ map fn list

-- | Run the given script if we are in town mode, otherwise do nothing.
unlessCombat :: (FromAreaEffect f) => Script TownEffect () -> Script f ()
unlessCombat = emitAreaEffect . EffIfCombat (return ())

-- | Run the given script if we are in combat mode, otherwise do nothing.
whenCombat :: (FromAreaEffect f) => Script CombatEffect () -> Script f ()
whenCombat = emitAreaEffect . flip EffIfCombat (return ())

whenDifficulty :: (FromAreaEffect f) => (Difficulty -> Bool) -> Script f ()
               -> Script f ()
whenDifficulty fn script = do
  difficulty <- areaGet (partyDifficulty . arsParty)
  when (fn difficulty) script

-------------------------------------------------------------------------------
-- Query:

areaGet :: (FromAreaEffect f) => (forall s. (AreaState s) => s -> a)
        -> Script f a
areaGet = emitAreaEffect . EffAreaGet

lookupMonsterEntry :: (FromAreaEffect f) => Grid.Key Monster
                   -> Script f (Maybe (Grid.Entry Monster))
lookupMonsterEntry key = areaGet (Grid.lookup key . arsMonsters)

demandMonsterEntry :: (FromAreaEffect f) => Grid.Key Monster
                   -> Script f (Grid.Entry Monster)
demandMonsterEntry key =
  maybe (fail "demandMonsterEntry") return =<< lookupMonsterEntry key

withMonsterEntry :: (FromAreaEffect f) => Grid.Key Monster
                 -> (Grid.Entry Monster -> Script f ()) -> Script f ()
withMonsterEntry key action = do
  mbEntry <- lookupMonsterEntry key
  maybeM mbEntry action

-------------------------------------------------------------------------------
-- Group retrieval:

-- | Return a list of all character numbers for characters who are conscious
-- (have non-zero health), whether or not they are under a mental effect.
getAllConsciousCharacters :: (FromAreaEffect f) => Script f [CharacterNumber]
getAllConsciousCharacters = do
  party <- areaGet arsParty
  return $ filter (chrIsConscious . partyGetCharacter party)
                  [minBound .. maxBound]

-- | Return a list of all ally monster grid entries.  In combat mode, this will
-- only include monsters within the combat arena.
getAllAllyMonsters :: (FromAreaEffect f) => Script f [Grid.Entry Monster]
getAllAllyMonsters = do
  monsters <- areaGet arsMonsters
  return $ filter (monstIsAlly . Grid.geValue) $ Grid.entries monsters

-- | Return a list of all enemy (non-ally) monster grid entries.  In combat
-- mode, this will only include monsters within the combat arena.
getAllEnemyMonsters :: (FromAreaEffect f) => Script f [Grid.Entry Monster]
getAllEnemyMonsters = do
  monsters <- areaGet arsMonsters
  return $ filter (not . monstIsAlly . Grid.geValue) $ Grid.entries monsters

-- | Get the 'HitTarget' for each conscious party member and each living ally
-- monster (in no particular order).
getAllAllyTargets :: (FromAreaEffect f) => Script f [HitTarget]
getAllAllyTargets = do
  chars <- map HitCharacter <$> getAllConsciousCharacters
  allies <- map (HitMonster . Grid.geKey) <$> getAllAllyMonsters
  return (chars ++ allies)

-------------------------------------------------------------------------------
-- Randomization:

-- | Choose a random element from a non-empty list.
getRandomElem :: (FromAreaEffect f) => [a] -> Script f a
getRandomElem list = if null list then fail "getRandomElem: empty list"
                     else (list !!) <$> getRandomR 0 (length list - 1)

-- | Choose a random value within the given range.  @'getRandomR' a b@ is
-- the 'Script' monad equivalent of @'randomRIO' (a, b)@.
getRandomR :: (FromAreaEffect f, Random a) => a -> a -> Script f a
getRandomR lo hi = emitAreaEffect $ EffRandom lo hi

-- getRandomLucky :: (FromAreaEffect f) => CharacterNumber -> Double -> Double
--                -> Script f Double
-- getRandomLucky charNum lo hi = do
--   char <- areaGet (arsGetCharacter charNum)
--   let luck = chrGetStat Luck char
--   let d = 1 - 0.995 ^^ luck
--   x <- emitAreaEffect $ EffRandom 0 1
--   return (lo + x * (1 - d + d * x) * (hi - lo))
{-
p(x) = (1 - d) + 2 * d * x
P(x) = (1 - d)*x + d * x^2 + C
f(x) = \int_0^x p(z) dz
     = P(x) - P(0)
     = (1 - d)*x + d * x^2
     = x * (1 - d + d * x)
-}

-- | Return a random permutation of the given list.
randomPermutation :: (FromAreaEffect f) => [a] -> Script f [a]
randomPermutation list = do
  let size = length list
  pairs <- mapM (\i -> (,) i <$> getRandomR 0 i) [size - 1, size - 2 .. 1]
  return $ elems $ STArray.runSTArray $ do
    arr <- STArray.newListArray (0, size - 1) list
    forM_ pairs $ \(i, j) -> when (i /= j) $ do
      x <- STArray.readArray arr i
      STArray.writeArray arr i =<< STArray.readArray arr j
      STArray.writeArray arr j x
    return arr

-------------------------------------------------------------------------------
-- Movement:

getPartyPosition :: Script TownEffect Position
getPartyPosition = emitEffect EffGetPartyPosition

setPartyPosition :: Position -> Script TownEffect ()
setPartyPosition = emitEffect . EffSetPartyPosition

partyWalkTo :: Position -> Script TownEffect ()
partyWalkTo pos = do
  oldPos <- getPartyPosition
  facePartyToward pos
  setPartyPosition pos
  emitEffect $ EffSetPartyAnim $ WalkAnim 4 4 oldPos
  wait 4

charWalkTo :: CharacterNumber -> Position -> Script CombatEffect Int
charWalkTo charNum pos = do
  let frames = 4
  oldPos <- areaGet (arsCharacterPosition charNum)
  faceCharacterToward charNum pos
  emitEffect $ EffSetCharPosition charNum pos
  emitEffect $ EffSetCharAnim charNum $ WalkAnim frames frames oldPos
  return frames

teleport :: AreaTag -> Position -> Script TownEffect ()
teleport tag pos = emitEffect $ EffTeleportToArea tag pos

exitTo :: AreaTag -> Script TownEffect ()
exitTo tag = do
  fadeOutMusic 0.8
  emitEffect $ EffExitTowardArea tag

walkMonster :: (FromAreaEffect f) => Int -> Grid.Key Monster -> Position
            -> Script f ()
walkMonster frames gkey pos' = do
  withMonsterEntry gkey $ \entry -> do
    let rect' = makeRect pos' $ rectSize $ Grid.geRect entry
    ok <- emitAreaEffect $ EffTryMoveMonster (Grid.geKey entry) rect'
    when ok $ do
      let deltaX = pointX $ pos' `pSub` rectTopleft (Grid.geRect entry)
      let dir = if deltaX < 0 then FaceLeft else FaceRight
      let anim' = WalkAnim frames frames $ rectTopleft $ Grid.geRect entry
      emitAreaEffect $ EffReplaceMonster (Grid.geKey entry) $
        Just (Grid.geValue entry) { monstAnim = anim', monstFaceDir = dir }
      wait frames

setMonsterTownAI :: (FromAreaEffect f) => Grid.Key Monster -> MonsterTownAI
                 -> Script f ()
setMonsterTownAI key townAI = withMonsterEntry key $ \entry -> do
  emitAreaEffect (EffReplaceMonster key $
                  Just (Grid.geValue entry) { monstTownAI = townAI })

-------------------------------------------------------------------------------
-- Attacks:

characterBeginOffensiveAction :: CharacterNumber -> Position
                              -> Script CombatEffect ()
characterBeginOffensiveAction charNum target = do
  faceCharacterToward charNum target
  alterCharacterStatus charNum $ seSetInvisibility Nothing
  setCharacterAnim charNum (AttackAnim 8)

characterWeaponAttack :: CharacterNumber -> Position -> Script CombatEffect ()
characterWeaponAttack charNum target = do
  char <- areaGet (arsGetCharacter charNum)
  let wd = chrEquippedWeaponData char
  characterWeaponInitialAnimation charNum target wd
  miss <- do
    -- TODO take bless/curse and passive abilities into account
    attackerRoll <- getRandomR 0 $ 2 * chrGetStat Agility char
    defenderRoll <- getRandomR 0 20 -- FIXME
    return (attackerRoll < defenderRoll)
  if miss then do
    playSound =<< getRandomElem [SndMiss1, SndMiss2]
   else do
    (critical, damage) <- characterWeaponChooseCritical char =<<
                          characterWeaponBaseDamage char wd
    characterWeaponHit wd target critical damage

characterWeaponInitialAnimation  :: CharacterNumber -> Position
                                 -> WeaponData -> Script CombatEffect ()
characterWeaponInitialAnimation charNum target wd = do
  characterBeginOffensiveAction charNum target
  origin <- areaGet (arsCharacterPosition charNum)
  attackInitialAnimation (wdAppearance wd) (wdElement wd) origin target

characterWeaponBaseDamage :: (FromAreaEffect f) => Character -> WeaponData
                          -> Script f Double
characterWeaponBaseDamage char wd = do
  let dieRoll = uncurry getRandomR (wdDamageRange wd)
  let rollDice n = sum <$> replicateM n dieRoll
  let strength = chrGetStat Strength char
  extraDie <- (strength `mod` 5 >) <$> getRandomR 0 4
  fromIntegral <$> rollDice (wdDamageBonus wd + strength `div` 5 +
                             if extraDie then 1 else 0)

characterWeaponChooseCritical :: (FromAreaEffect f) => Character -> Double
                              -> Script f (Bool, Double)
characterWeaponChooseCritical char damage = do
  critical <- (0.998 ^^ chrGetStat Intellect char <) <$>
              getRandomR 0 (1 :: Double)
  return (critical, if critical then damage * 1.5 else damage)

characterWeaponHit :: WeaponData -> Position -> Bool -> Double
                   -> Script CombatEffect ()
characterWeaponHit wd target critical damage = do
  attackHit (wdAppearance wd) (wdElement wd) (wdEffects wd)
            target critical damage

monsterBeginOffensiveAction :: Grid.Key Monster -> Position
                            -> Script CombatEffect ()
monsterBeginOffensiveAction key target = do
  faceMonsterToward key target
  alterMonsterStatus key $ seSetInvisibility Nothing
  setMonsterAnim key (AttackAnim 8)

monsterPerformAttack :: Grid.Key Monster -> MonsterAttack -> Position
                     -> Script CombatEffect ()
monsterPerformAttack key attack target = do
  monsterAttackInitialAnimation key attack target
  miss <- do
    -- TODO take bless/curse into account
    -- TODO take monster agility into account
    -- TODO take target agility into account
    return False
  if miss then do
    playSound =<< getRandomElem [SndMiss1, SndMiss2]
   else do
    (critical, damage) <- monsterAttackChooseCritical attack =<<
                          monsterAttackBaseDamage attack
    monsterAttackHit attack target critical damage

monsterAttackInitialAnimation :: Grid.Key Monster -> MonsterAttack -> Position
                              -> Script CombatEffect ()
monsterAttackInitialAnimation key attack target = do
  monsterBeginOffensiveAction key target
  withMonsterEntry key $ \entry -> do
    attackInitialAnimation (maAppearance attack) (maElement attack)
                           (monsterHeadPos entry) target

monsterAttackBaseDamage :: (FromAreaEffect f) => MonsterAttack
                        -> Script f Double
monsterAttackBaseDamage attack = do
  let dieRoll = uncurry getRandomR (maDamageRange attack)
  let rollDice n = sum <$> replicateM n dieRoll
  fromIntegral <$> rollDice (maDamageCount attack)

monsterAttackChooseCritical :: (FromAreaEffect f) => MonsterAttack -> Double
                            -> Script f (Bool, Double)
monsterAttackChooseCritical attack damage = do
  critical <- (maCriticalChance attack >) <$> getRandomR 0 1
  return (critical, if critical then damage * 1.5 else damage)

monsterAttackHit :: MonsterAttack -> Position -> Bool -> Double
                 -> Script CombatEffect ()
monsterAttackHit ma target critical damage = do
  attackHit (maAppearance ma) (maElement ma) (maEffects ma)
            target critical damage

attackInitialAnimation :: (FromAreaEffect f) => AttackAppearance
                       -> AttackElement -> Position -> Position -> Script f ()
attackInitialAnimation appearance element origin target = do
  case appearance of
    BiteAttack -> return ()
    BladeAttack -> return ()
    BluntAttack -> return ()
    BowAttack -> do
      playSound SndArrow
      addBallisticDoodad ArrowProj origin target 250 >>= wait
    BreathAttack -> do
      let proj = case element of
                   AcidAttack -> AcidProj
                   EnergyAttack -> IceProj -- FIXME
                   FireAttack -> FireProj
                   IceAttack -> IceProj
                   PhysicalAttack -> StarProj
      playSound SndBreath
      addBallisticDoodad proj origin target 220 >>= wait
    ClawAttack -> return ()
    ThrownAttack -> do
      playSound SndThrow
      addBallisticDoodad StarProj origin target 200 >>= wait
    WandAttack -> do
      let tint = case element of
                   AcidAttack -> Tint 0 255 0 192
                   EnergyAttack -> Tint 0 0 255 192
                   FireAttack -> Tint 255 0 0 192
                   IceAttack -> Tint 0 255 255 192
                   PhysicalAttack -> Tint 255 255 255 192
      addLightningDoodad tint origin target

attackHit :: AttackAppearance -> AttackElement -> [AttackEffect] -> Position
          -> Bool -> Double -> Script CombatEffect ()
attackHit appearance element effects target critical damage = do
  -- TODO take attack effects into account (in addition to main attack element)
  -- when choosing sound/doodad
  let elementSnd AcidAttack = SndChemicalDamage
      elementSnd FireAttack = if critical then SndBoomSmall else SndFireDamage
      elementSnd PhysicalAttack = if critical then SndHit3 else SndHit4
      elementSnd _ = error "FIXME attackHit"
  playSound =<<
    case appearance of
      BiteAttack -> return SndBite
      BowAttack -> if critical then return SndHit2 else return SndHit1
      BladeAttack -> if critical then return SndHit4
                     else getRandomElem [SndHit1, SndHit2, SndHit3]
      BluntAttack -> if critical then return SndHit2 else return SndHit1
      BreathAttack -> return $ elementSnd element
      ClawAttack -> return SndClaw
      ThrownAttack -> if critical then return SndHit2 else return SndHit1
      WandAttack -> return $ elementSnd element
  let elementBoom = do
        let boom = case element of
                     AcidAttack -> AcidBoom
                     EnergyAttack -> EnergyBoom
                     FireAttack -> FireBoom
                     IceAttack -> IceBoom
                     PhysicalAttack -> SlashRight
        addBoomDoodadAtPosition boom (if critical then 3 else 2) target
  case appearance of
    BiteAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    BowAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    BladeAttack -> do
      addBoomDoodadAtPosition SlashRight 2 target
      when critical $ addBoomDoodadAtPosition SlashLeft 3 target
    BluntAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    BreathAttack -> elementBoom
    ClawAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    ThrownAttack -> addBoomDoodadAtPosition SlashRight 2 target -- FIXME
    WandAttack -> elementBoom
  let hitTarget = HitPosition target
  extraHits <- flip3 foldM [] effects $ \hits effect -> do
    case effect of
      ExtraAcidDamage mult -> do
        return ((hitTarget, AcidDamage, mult * damage) : hits)
      ExtraEnergyDamage mult -> do
        return ((hitTarget, EnergyDamage, mult * damage) : hits)
      ExtraFireDamage mult -> do
        return ((hitTarget, FireDamage, mult * damage) : hits)
      ExtraIceDamage mult -> do
        return ((hitTarget, ColdDamage, mult * damage) : hits)
      InflictPoison mult -> do
        inflictPoison hitTarget (mult * damage)
        return hits
      _ -> return hits -- FIXME
  -- FIXME change main hit damage element based on attack element
  dealDamage ((hitTarget, PhysicalDamage, damage) : extraHits)
  wait 16

-------------------------------------------------------------------------------
-- Damage:

data HitTarget = HitCharacter CharacterNumber
               | HitMonster (Grid.Key Monster)
               | HitPosition Position

dealDamage :: (FromAreaEffect f) => [(HitTarget, DamageType, Double)]
           -> Script f ()
dealDamage = dealDamage' False

dealDamage' :: (FromAreaEffect f) => Bool
            -> [(HitTarget, DamageType, Double)] -> Script f ()
dealDamage' gentle hits = do
  let convert (hitTarget, dmgType, damage) = do
        mbOccupant <- getHitTargetOccupant hitTarget
        return $ case mbOccupant of
                   Just occupant -> Just (occupant, dmgType, damage)
                   Nothing -> Nothing
  let resist (occupant, dmgType, damage) = do
        -- Stun is measured in action points.  We do 1 AP stun per 100 damage.
        let stun = if gentle then 0 else damage * 0.01
        (damage', stun') <- do
          case occupant of
            Left charNum -> do
              char <- areaGet (arsGetCharacter charNum)
              return $ applyCharResistances char dmgType damage stun
            Right monstEntry -> do
              return $ applyMonsterResistances (Grid.geValue monstEntry)
                                               dmgType damage stun
        return (occupant, damage', stun')
  let keyFn (k, _, _) = right Grid.geKey k
  let combine (_, dmg1, s1) (k, dmg2, s2) = (k, dmg1 + dmg2, s1 + s2)
  let inflict (occupant, damage, stun) = do
        case occupant of
          Left charNum -> dealRawDamageToCharacter gentle charNum damage stun
          Right monstEntry ->
            dealRawDamageToMonster gentle (Grid.geKey monstEntry) damage stun
  mapM_ inflict . map (foldl1' combine) . groupKey keyFn . sortKey keyFn =<<
    (mapM resist . catMaybes =<< mapM convert hits)

-- | Inflict damage and stun on a character, ignoring armor and resistances.
-- Stun is measured in action points.
dealRawDamageToCharacter :: (FromAreaEffect f) => Bool -> CharacterNumber
                         -> Int -> Double -> Script f ()
dealRawDamageToCharacter gentle charNum damage stun = do
  pos <- areaGet (arsCharacterPosition charNum)
  char <- areaGet (arsGetCharacter charNum)
  -- If this is non-gentle damage, wake us up from being dazed, and add
  -- adrenaline.
  unless gentle $ do
    alterCharacterStatus charNum seWakeFromDaze
    party <- areaGet arsParty
    let maxHealth = chrMaxHealth party $ partyGetCharacter party charNum
    addToCharacterAdrenaline (adrenalineForDamage damage maxHealth) charNum
  -- Do stun (if in combat):
  when (stun > 0) $ whenCombat $ do
    moments <- emitEffect $ EffGetCharMoments charNum
    emitEffect $ EffSetCharMoments charNum $
      max 0 (moments - round (stun * fromIntegral momentsPerActionPoint))
  -- Do damage:
  minHealth <- emitAreaEffect $ EffIfCombat (return 0) (return 1)
  let health' = max minHealth (chrHealth char - damage)
  emitAreaEffect $ EffAlterCharacter charNum $ \c -> c { chrHealth = health' }
  emitAreaEffect $ EffIfCombat (setCharacterAnim charNum $ HurtAnim 12)
                               (setPartyAnim $ HurtAnim 12)
  addNumberDoodadAtPosition damage pos
  -- If we're in combat, the character can die:
  whenCombat $ when (health' <= 0) $ do
    alterCharacterMana charNum (const 0)
    addToCharacterAdrenaline (negate maxAdrenaline) charNum
    alterCharacterStatus charNum (const initStatusEffects)
    resources <- areaGet arsResources
    let images = rsrcCharacterImages resources (chrClass char)
                                     (chrAppearance char)
    faceDir <- emitEffect $ EffGetCharFaceDir charNum
    playSound (characterScreamSound char)
    playSound SndDie1
    addDeathDoodad images faceDir (makeRect pos (1, 1))
    -- If all characters are now dead, it's game over:
    alive <- areaGet (Fold.any chrIsConscious . partyCharacters . arsParty)
    unless alive $ emitAreaEffect EffGameOver

-- | Inflict damage and stun on a monster, ignoring armor and resistances.
-- Stun is measured in action points.  The monster must exist.
dealRawDamageToMonster :: (FromAreaEffect f) => Bool -> Grid.Key Monster
                       -> Int -> Double -> Script f ()
dealRawDamageToMonster gentle key damage stun = do
  entry <- demandMonsterEntry key
  let monst = Grid.geValue entry
  -- If this is non-gentle damage, wake us up from being dazed and add
  -- adrenaline.
  unless gentle $ alterMonsterStatus key seWakeFromDaze
  let adrenaline' = monstAdrenaline monst +
        if gentle then 0 else
          adrenalineForDamage damage $ mtMaxHealth $ monstType monst
  -- Do damage and stun:
  let health' = monstHealth monst - damage
  let moments' = max 0 (monstMoments monst -
                        round (stun * fromIntegral momentsPerActionPoint))
  let mbMonst' = if health' <= 0 then Nothing else
                   Just monst { monstAdrenaline = adrenaline',
                                monstAnim = HurtAnim 12, monstHealth = health',
                                monstMoments = moments' }
  emitAreaEffect $ EffReplaceMonster key mbMonst'
  addNumberDoodadAtPoint damage $ rectCenter $ prectRect $ Grid.geRect entry
  -- If the monster is now dead, add a death doodad, set the monster's
  -- "dead" var (if any) to True, and grant experience if it's an enemy.
  when (isNothing mbMonst') $ do
    resources <- areaGet arsResources
    let mtype = monstType monst
    addDeathDoodad (rsrcMonsterImages resources (mtSize mtype)
                                      (mtImageRow mtype))
                   (monstFaceDir monst) (Grid.geRect entry)
    maybeM (monstDeadVar monst) (emitAreaEffect . flip EffSetVar True)
    unless (monstIsAlly monst) $ do
      grantExperience $ mtExperienceValue $ monstType monst

adrenalineForDamage :: Int -> Int -> Int
adrenalineForDamage damage maxHealth =
  round $ (fromIntegral maxAdrenaline *) $ square $ min 1 $
  2 * fromIntegral damage / (fromIntegral maxHealth :: Double)

-------------------------------------------------------------------------------
-- Healing/restoration:

healCharacter :: (FromAreaEffect f) => CharacterNumber -> Int -> Script f ()
healCharacter charNum baseAmount = do
  multiplier <- chrAbilityMultiplier Recuperation 1.1 1.2 1.4 <$>
                areaGet (arsGetCharacter charNum)
  let amount = round (multiplier * fromIntegral baseAmount)
  pos <- areaGet (arsCharacterPosition charNum)
  addNumberDoodadAtPosition amount pos
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrHealth = min (chrMaxHealth party char)
                           (chrHealth char + amount) }

healMonster :: (FromAreaEffect f) => Grid.Key Monster -> Int -> Script f ()
healMonster key amount = withMonsterEntry key $ \entry -> do
  let monst = Grid.geValue entry
  let health' = min (monstHealth monst + amount)
                    (mtMaxHealth $ monstType monst)
  let monst' = monst { monstHealth = health' }
  emitAreaEffect $ EffReplaceMonster key (Just monst')
  addNumberDoodadAtPoint amount $ rectCenter $ prectRect $ Grid.geRect entry

alterCharacterMana :: (FromAreaEffect f) => CharacterNumber -> (Int -> Int)
                   -> Script f ()
alterCharacterMana charNum fn = do
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrMana = max 0 $ min (chrMaxMana party char) $ fn (chrMana char) }

restoreManaToFull :: (FromAreaEffect f) => CharacterNumber -> Script f ()
restoreManaToFull charNum = do
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrMana = chrMaxMana party char }

addToCharacterAdrenaline :: (FromAreaEffect f) => Int -> CharacterNumber
                         -> Script f ()
addToCharacterAdrenaline delta charNum =
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrAdrenaline = max 0 $ min maxAdrenaline $
                           chrAdrenaline char + delta }

-------------------------------------------------------------------------------
-- Status effects:

-- | Directly alters the status effects of the target.
alterStatus :: (FromAreaEffect f) => HitTarget
            -> (StatusEffects -> StatusEffects) -> Script f ()
alterStatus hitTarget fn = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> alterCharacterStatus charNum fn
    Just (Right monstEntry) -> alterMonsterStatus (Grid.geKey monstEntry) fn
    Nothing -> return ()

alterCharacterStatus :: (FromAreaEffect f) => CharacterNumber
                     -> (StatusEffects -> StatusEffects) -> Script f ()
alterCharacterStatus charNum fn = do
  fn' <- emitAreaEffect $ EffIfCombat (return fn) (return (townifyStatus . fn))
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrStatus = fn' (chrStatus char) }

alterMonsterStatus :: (FromAreaEffect f) => Grid.Key Monster
                   -> (StatusEffects -> StatusEffects) -> Script f ()
alterMonsterStatus key fn = do
  fn' <- emitAreaEffect $ EffIfCombat (return fn) (return (townifyStatus . fn))
  mbMonst <- fmap Grid.geValue <$> lookupMonsterEntry key
  let mbMonst' = (\m -> m { monstStatus = fn' (monstStatus m) }) <$> mbMonst
  emitAreaEffect $ EffReplaceMonster key mbMonst'

-- | Grants the given level of invisibility to the target, unless the target is
-- already more invisible.
grantInvisibility :: (FromAreaEffect f) => HitTarget -> Invisibility
                  -> Script f ()
grantInvisibility hitTarget invis = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      status <- chrStatus <$> areaGet (arsGetCharacter charNum)
      when (seInvisibility status < Just invis) $ do
        alterCharacterStatus charNum $ seSetInvisibility $ Just invis
    Just (Right monstEntry) -> do
      let status = monstStatus $ Grid.geValue monstEntry
      when (seInvisibility status < Just invis) $ do
        alterMonsterStatus (Grid.geKey monstEntry) $ seSetInvisibility $
          Just invis
    Nothing -> return ()

-- | Inflicts poison damage (as opposed to direct damage) onto the target,
-- taking resistances into account.
inflictPoison :: (FromAreaEffect f) => HitTarget -> Double -> Script f ()
inflictPoison hitTarget basePoison = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      let poison = round (basePoison * chrGetResistance ResistChemical char)
      alterCharacterStatus charNum $ seAlterPoison (poison +)
    Just (Right monstEntry) -> do
      let poison = round $ (basePoison *) $ tmGet ResistChemical $
                   mtResistances $ monstType $ Grid.geValue monstEntry
      alterMonsterStatus (Grid.geKey monstEntry) $ seAlterPoison (poison +)
    Nothing -> return ()

-------------------------------------------------------------------------------
-- Camera motion:

shakeCamera :: (FromAreaEffect f) => Double -> Int -> Script f ()
shakeCamera amplitude duration = do
  emitAreaEffect $ EffShakeCamera amplitude duration

-------------------------------------------------------------------------------
-- Creature animation:

faceCharacterToward :: CharacterNumber -> Position -> Script CombatEffect ()
faceCharacterToward charNum pos = do
  deltaX <- pointX . (pos `pSub`) <$> areaGet (arsCharacterPosition charNum)
  when (deltaX /= 0) $ do
    let dir = if deltaX < 0 then FaceLeft else FaceRight
    emitEffect $ EffSetCharFaceDir charNum dir

faceMonsterToward :: (FromAreaEffect f) => Grid.Key Monster -> Position
                  -> Script f ()
faceMonsterToward key pos = do
  withMonsterEntry key $ \entry -> do
    let deltaX = pointX $ (pos `pSub`) $ monsterHeadPos entry
    when (deltaX /= 0) $ do
      let dir = if deltaX < 0 then FaceLeft else FaceRight
      emitAreaEffect $ EffReplaceMonster key $
        Just (Grid.geValue entry) { monstFaceDir = dir }

facePartyToward :: Position -> Script TownEffect ()
facePartyToward pos = do
  deltaX <- pointX . (pos `pSub`) <$> getPartyPosition
  when (deltaX /= 0) $ do
    let dir = if deltaX < 0 then FaceLeft else FaceRight
    emitEffect $ EffSetPartyFaceDir dir

setCharacterAnim :: CharacterNumber -> CreatureAnim -> Script CombatEffect ()
setCharacterAnim charNum anim = emitEffect $ EffSetCharAnim charNum anim

setMonsterAnim :: (FromAreaEffect f) => Grid.Key Monster -> CreatureAnim
               -> Script f ()
setMonsterAnim key anim = do
  withMonsterEntry key $ \entry -> do
    let monst = Grid.geValue entry
    emitAreaEffect $ EffReplaceMonster key $ Just monst { monstAnim = anim }

setPartyAnim :: CreatureAnim -> Script TownEffect ()
setPartyAnim = emitEffect . EffSetPartyAnim

getMonsterHeadPos :: (FromAreaEffect f) => Grid.Key Monster
                  -> Script f Position
getMonsterHeadPos key = do
  entry <- demandMonsterEntry key
  return $ monsterHeadPos entry

-------------------------------------------------------------------------------
-- Doodads:

addContinuousDoodad :: (FromAreaEffect f) => DoodadHeight -> Int
                    -> (Double -> IPoint -> Paint ()) -> Script f ()
addContinuousDoodad height limit paintFn = do
  let paintFn' n topleft =
        paintFn (fromIntegral (limit - n) / fromIntegral limit) topleft
  emitAreaEffect $ EffAddDoodad $ Doodad
    { doodadCountdown = limit, doodadHeight = height, doodadPaint = paintFn' }

-- | Return the number of frames the projectile will spend travelling.
addBallisticDoodad :: (FromAreaEffect f) => ProjTag -> Position -> Position
                   -> Double -> Script f Int
addBallisticDoodad ptag start end speed = do
  sprite <- flip rsrcProj ptag <$> areaGet arsResources
  let spt = positionCenter start
      ept = positionCenter end
  let gravity = 80 -- arbitrary value that gives reasonably nice-looking motion
  --let time = sqrt (height / gravity)
  let time = sqrt (spt `pSqDist` ept) / speed  -- seconds
  let height = gravity * time * time
  let limit = floor (time * fromIntegral framesPerSecond)  -- frames
  let paint count topleft = do
        let t = 1.0 - fromIntegral count / fromIntegral limit
        let x = pointX spt + t * (pointX ept - pointX spt)
        let dx = pointX ept - pointX spt
        let y = pointY spt + t * (pointY ept - pointY spt) -
                4 * (t - t * t) * height -- remember, positive y is down
        let dy = (pointY ept - pointY spt) - 4 * (1 - 2 * t) * height
        blitRotate sprite (Point x y `pSub` fmap fromIntegral topleft)
                   (atan2 dy dx)
  emitAreaEffect $ EffAddDoodad $ Doodad
    { doodadCountdown = limit, doodadHeight = MidDood, doodadPaint = paint }
  return limit

addBeamDoodad :: (FromAreaEffect f) => Tint -> DPoint -> DPoint -> Int
              -> Script f ()
addBeamDoodad tint startPt endPt limit = do
  let semiWidth = 8
  let perp = pPolar semiWidth (pAtan2 (endPt `pSub` startPt) + pi / 2)
  let tint' = tint { tintAlpha = 0 }
  let paint count cameraTopleft = do
        let start = startPt `pSub` fmap fromIntegral cameraTopleft
            end = endPt `pSub` fmap fromIntegral cameraTopleft
        let f = 2 * fromIntegral (limit - count) / fromIntegral limit
        let (v1, v2) = if f <= 1.0 then (pZero, perp `pMul` f)
                       else (perp `pMul` (f - 1.0), perp)
        gradientPolygon [(tint, start `pAdd` v1), (tint, end `pAdd` v1),
                         (tint', end `pAdd` v2), (tint', start `pAdd` v2)]
        gradientPolygon [(tint, start `pSub` v1), (tint, end `pSub` v1),
                         (tint', end `pSub` v2), (tint', start `pSub` v2)]
  emitAreaEffect $ EffAddDoodad $ Doodad
    { doodadCountdown = limit, doodadHeight = MidDood, doodadPaint = paint }

addBlasterDoodad :: (FromAreaEffect f) => Tint -> Double -> Double -> Position
                 -> Position -> Double -> Script f Int
addBlasterDoodad tint width len start end speed = do
  let tint' = tint { tintAlpha = 0 }
  let spt = positionCenter start
      ept = positionCenter end
  let delta = ept `pSub` spt
  let perp = pPolar (width / 2) (pAtan2 delta + pi / 2)
  let distance = pDist spt ept
  let duration = (distance + len) / speed  -- seconds
  let limit = floor (duration * fromIntegral framesPerSecond)  -- frames
  let frac = len / (distance + len)
  addContinuousDoodad MidDood limit $ \t topleft -> do
    let spt' = spt `pSub` fmap fromIntegral topleft
    let t1 = min 1 (t / (1 - frac))
    let t2 = max 0 ((t - frac) / (1 - frac))
    let p1 = spt' `pAdd` delta `pMul` t1
    let p2 = spt' `pAdd` delta `pMul` t2
    gradientPolygon [(tint, p1 `pAdd` perp), (tint', p2 `pSub` perp),
                     (tint, p1 `pSub` perp), (tint', p2 `pAdd` perp)]
  return $ floor (fromIntegral framesPerSecond * distance / speed)

addBoomDoodadAtPoint :: (FromAreaEffect f) => StripTag -> Int -> IPoint
                        -> Script f ()
addBoomDoodadAtPoint tag slowdown center = do
  strip <- flip rsrcStrip tag <$> areaGet arsResources
  let limit = stripLength strip * slowdown
  let paint count cameraTopleft = do
        blitLoc (strip ! (7 - count `div` slowdown))
                (LocCenter $ center `pSub` cameraTopleft)
  emitAreaEffect $ EffAddDoodad $ Doodad
    { doodadCountdown = limit, doodadHeight = MidDood, doodadPaint = paint }

addBoomDoodadAtPosition :: (FromAreaEffect f) => StripTag -> Int -> Position
                        -> Script f ()
addBoomDoodadAtPosition tag slowdown pos =
  addBoomDoodadAtPoint tag slowdown (positionCenter pos)

addDeathDoodad :: (FromAreaEffect f) => CreatureImages -> FaceDir -> PRect
               -> Script f ()
addDeathDoodad images faceDir prect =
  emitAreaEffect $ EffAddDoodad $ Doodad limit MidDood paint where
    paint count topleft = do
      let tint = let gba = fromIntegral (255 * count `div` limit)
                 in Tint 255 gba gba gba
      let rect = adjustRect1 (2 * (count - limit)) initRect
      blitStretchTinted tint sprite (rect `rectMinus` topleft)
    limit = 15
    initRect = prectRect prect
    sprite = ciStand faceDir images

addLightningDoodad :: (FromAreaEffect f) => Tint -> Position -> Position
                   -> Script f ()
addLightningDoodad tint startPos endPos = do
  let limit = 18
  let startPt = positionCenter startPos
  let endPt = positionCenter endPos
  let dist = pDist startPt endPt
  let numSteps = floor (dist / 20) :: Int
  let step = (endPt `pSub` startPt) `pDiv` fromIntegral numSteps
  lists <- fmap (listArray (0, limit)) $ replicateM (limit + 1) $ do
    forM [0 .. numSteps] $ \idx -> do
      let center = startPt `pAdd` step `pMul` fromIntegral idx
      dx <- getRandomR (-10) 10
      dy <- getRandomR (-10) 10
      return $ pAdd center $ Point dx dy
  let paint count topleft =
        drawThickLineChain 3 tint $
        map (`pSub` fmap fromIntegral topleft) (lists ! count)
  emitAreaEffect $ EffAddDoodad $ Doodad limit MidDood paint

addLightWallDoodad :: (FromAreaEffect f) => Bool -> Tint -> Int
                   -> Double -> DPoint -> DPoint -> Script f ()
addLightWallDoodad foreground tint duration maxHeight startPt endPt = do
  let tint' = tint { tintAlpha = 0 }
  let doodHeight = if foreground then HighDood else MidDood
  addContinuousDoodad doodHeight duration $ \t topleft -> do
    let height = maxHeight * 4 * (t - t * t)
    let offset = fmap fromIntegral topleft
    let p1 = startPt `pSub` offset
        p2 = endPt `pSub` offset
    let p3 = p2 `pSub` Point 0 height
        p4 = p1 `pSub` Point 0 height
    gradientPolygon [(tint, p1), (tint, p2), (tint', p3), (tint', p4)]

-- | Add a number doodad, centered on a given point (map pixel).
addNumberDoodadAtPoint :: (FromAreaEffect f) => Int -> IPoint -> Script f ()
addNumberDoodadAtPoint number (Point x y) = do
  paintDigits <- areaGet (rsrcPaintDigits . arsResources)
  let limit = 30
  let paint count cameraTopleft =
        paintDigits number $ LocCenter $
        Point x (y - (limit - count)) `pSub` cameraTopleft
  emitAreaEffect $ EffAddDoodad $ Doodad { doodadCountdown = limit,
                                           doodadHeight = HighDood,
                                           doodadPaint = paint }

-- | Add a number doodad, centered on a given position (map coordinates).
addNumberDoodadAtPosition :: (FromAreaEffect f) => Int -> Position
                          -> Script f ()
addNumberDoodadAtPosition number pos =
  addNumberDoodadAtPoint number (positionCenter pos)

addShockwaveDoodad :: (FromAreaEffect f) => Int -> DPoint
                   -> (Double -> (Tint, Double, Double, Double)) -> Script f ()
addShockwaveDoodad limit center fn = do
  let paint count cameraTopleft = do
        let (tint, thickness, hRad, vRad) =
              fn (fromIntegral (limit - count) / fromIntegral limit)
        tintRing tint thickness (center `pSub` fmap fromIntegral cameraTopleft)
                 hRad vRad
  emitAreaEffect $ EffAddDoodad $ Doodad { doodadCountdown = limit,
                                           doodadHeight = MidDood,
                                           doodadPaint = paint }

addExtendingHookshotDoodad :: (FromAreaEffect f) => Position -> Position
                           -> Script f Int
addExtendingHookshotDoodad startPos endPos = do
  let limit = round (3 * pDist (fromIntegral <$> startPos)
                               (fromIntegral <$> endPos) :: Double)
  let paint factor cameraTopleft = do
        let startPt = positionCenter startPos `pSub` cameraTopleft
        let endPt = positionCenter endPos `pSub` cameraTopleft
        let delta = fromIntegral <$> (endPt `pSub` startPt)
        let midPt = round <$> ((fromIntegral <$> startPt) `pAdd`
                               delta `pMul` factor)
        drawLine (Tint 128 64 0 255) startPt midPt
        let Point x y = midPt
        tintRect (Tint 128 128 128 255) (Rect (x - 2) (y - 2) 5 5)
  addContinuousDoodad MidDood limit paint
  return limit

addExtendedHookshotDoodad :: (FromAreaEffect f) => Int -> Position -> Position
                          -> Script f ()
addExtendedHookshotDoodad limit startPos endPos = do
  let paint _ cameraTopleft = do
        let startPt = positionCenter startPos `pSub` cameraTopleft
        let endPt = positionCenter endPos `pSub` cameraTopleft
        drawLine (Tint 128 64 0 255) startPt endPt
  addContinuousDoodad MidDood limit paint

addRetractingHookshotDoodad :: (FromAreaEffect f) => Position -> Position
                            -> Script f ()
addRetractingHookshotDoodad startPos endPos = do
  let limit = round (2 * pDist (fromIntegral <$> startPos)
                               (fromIntegral <$> endPos) :: Double)
  let paint factor cameraTopleft = do
        let startPt = fromIntegral <$> (positionCenter startPos `pSub`
                                        cameraTopleft)
        let endPt = fromIntegral <$> (positionCenter endPos `pSub`
                                      cameraTopleft)
        let delta = startPt `pSub` endPt
        let midPt = endPt `pAdd` delta `pMul` factor
        let dist = pDist startPt midPt
        let theta = pAtan2 (midPt `pSub` startPt)
        let perp = pPolar 1 (theta + pi / 2)
        let amplFn r = 0.25 * dist * factor * sin (r * 2 * pi)
        let pointFn r = startPt `pAdd` pPolar (r * dist) theta `pAdd`
                        perp `pMul` amplFn r
        let alpha = floor (255 * (1 - factor))
        drawLineChain (Tint 128 64 0 alpha) $ map pointFn [0, 0.01 .. 1]
  addContinuousDoodad MidDood limit paint

doExplosionDoodad :: (FromAreaEffect f) => StripTag -> DPoint -> Script f ()
doExplosionDoodad tag (Point cx cy) = do
  startTheta <- getRandomR 0 (2 * pi)
  flip3 foldM_ startTheta (replicate 16 ()) $ \oldTheta () -> do
    theta <- getRandomR (oldTheta + 0.5 * pi) (oldTheta + 1.5 * pi)
    rho <- getRandomR 0 1
    let pt = Point (cx + 35 * rho * cos theta) (cy + 45 * rho * sin theta)
    addBoomDoodadAtPoint tag 4 (round <$> pt)
    wait 1
    return theta

-------------------------------------------------------------------------------
-- Messages and conversation:

-- | Print a string to the terminal console, for debugging.
debug :: (FromAreaEffect f) => String -> Script f ()
debug = emitAreaEffect . EffDebug

setMessage :: (FromAreaEffect f) => String -> Script f ()
setMessage = emitAreaEffect . EffMessage

narrate :: (FromAreaEffect f) => String -> Script f ()
narrate = emitAreaEffect . EffNarrate

-- | Put up a conversation dialog, with no option for the player to not answer.
forcedChoice :: (FromAreaEffect f) => String -> [(String, a)] -> Script f a
forcedChoice text choices =
  emitAreaEffect $ EffMultiChoice text choices Nothing

-- | Put up a conversation dialog, with an option for the player to not answer
-- (in which case the result is 'Nothing').
maybeChoice :: (FromAreaEffect f) => String -> [(String, a)]
            -> Script f (Maybe a)
maybeChoice text choices =
  emitAreaEffect $ EffMultiChoice text (map (second Just) choices) Nothing

-- | Put up a conversation dialog, with an option for the player to not answer
-- (in which case the given default value is used).
multiChoice :: (FromAreaEffect f) => String -> [(String, a)] -> a -> Script f a
multiChoice text choices cancel =
  emitAreaEffect $ EffMultiChoice text choices $ Just cancel

data ConvChoice a = ContinueConv String [(String, ConvChoice a)]
                  | StopConv a

forcedConversationLoop :: (FromAreaEffect f) => String
                       -> [(String, ConvChoice a)] -> Script f a
forcedConversationLoop = (choice [] [] .) . ContinueConv where
  choice _ _ (StopConv a) = return a
  choice before after (ContinueConv text options) = do
    let options' = before ++ options ++ after
    idx <- forcedChoice text $ zip (map fst options') [0..]
    let (before', rest) = splitAt idx options'
    let after' = tail rest
    choice before' after' (snd $ head rest)

-------------------------------------------------------------------------------
-- Mode changes:

startCombat :: Script TownEffect ()
startCombat = emitEffect EffStartCombat

gameOver :: (FromAreaEffect f) => Script f ()
gameOver = emitAreaEffect EffGameOver

-------------------------------------------------------------------------------
-- Sound:

playSound :: (FromAreaEffect f) => SoundTag -> Script f ()
playSound tag = do
  resources <- areaGet arsResources
  emitAreaEffect $ EffPlaySound $ rsrcSound resources tag

-- | Start new music looping.  If any music is already playing, stop it.
startMusic :: (FromAreaEffect f) => MusicTag -> Script f ()
startMusic = emitAreaEffect . EffMusicStart

-- | If any music is currently playing, stop it immediately.
stopMusic :: (FromAreaEffect f) => Script f ()
stopMusic = emitAreaEffect $ EffMusicStop

-- | If any music is currently playing, fade it out over the given number of
-- seconds.
fadeOutMusic :: (FromAreaEffect f) => Double -> Script f ()
fadeOutMusic = emitAreaEffect . EffMusicFadeOut

-------------------------------------------------------------------------------
-- Other script actions:

addBasicEnemyMonster :: (FromAreaEffect f) => Position -> MonsterTag
                     -> Maybe (Var Bool) -> MonsterTownAI -> Script f ()
addBasicEnemyMonster nearPos tag mbDeadVar townAi = do
  let mtype = getMonsterType tag
  within <- areaGet arsBoundaryRect
  -- TODO Allow for non-SizeSmall monsters
  spot <- areaGet $ \ars ->
    if not $ arsIsBlockedForParty ars nearPos then nearPos
    else arsFindOpenSpot ars nearPos within Set.empty
  _ <- tryAddMonster spot Monster
         { monstAnim = NoAnim,
           monstAdrenaline = 0,
           monstDeadVar = mbDeadVar,
           monstFaceDir = FaceLeft,
           monstHealth = mtMaxHealth mtype,
           monstIsAlly = False,
           monstMoments = 0,
           monstName = mtName mtype,
           monstScript = Nothing,
           monstStatus = initStatusEffects,
           monstTag = tag,
           monstTownAI = townAi,
           monstType = mtype }
  return ()

addDevice_ :: (FromAreaEffect f) => Device -> Position -> Script f ()
addDevice_ device pos = () <$ emitAreaEffect (EffTryAddDevice pos device)

getTerrainTile :: (FromAreaEffect f) => TileTag -> Script f TerrainTile
getTerrainTile tag = do
  resources <- areaGet arsResources
  return $ tilesetGet tag $ rsrcTileset resources

grantAndEquipWeapon :: (FromAreaEffect f) => WeaponItemTag -> CharacterNumber
                    -> Script f ()
grantAndEquipWeapon tag charNum = do
  party <- areaGet arsParty
  let char = partyGetCharacter party charNum
  let mbOldItem = fmap WeaponItemTag $ eqpWeapon $ chrEquipment char
  emitAreaEffect $ EffAlterCharacter charNum (\c -> c { chrEquipment =
    (chrEquipment c) { eqpWeapon = Just tag } })
  maybeM mbOldItem (emitAreaEffect . EffGrantItem)

grantExperience :: (FromAreaEffect f) => Int -> Script f ()
grantExperience xp = do
  oldLevel <- areaGet (partyLevel . arsParty)
  emitAreaEffect $ EffGrantExperience xp
  newLevel <- areaGet (partyLevel . arsParty)
  when (newLevel > oldLevel) $ do
    setMessage $ "Party is now level " ++ show newLevel ++ "!"
    mapM_ return =<< areaGet arsPartyPositions -- FIXME doodads
    playSound SndLevelUp

removeDevice :: (FromAreaEffect f) => Grid.Key Device -> Script f ()
removeDevice key = emitAreaEffect $ EffReplaceDevice key Nothing

removeFields :: (FromAreaEffect f) => [Position] -> Script f ()
removeFields = emitAreaEffect . EffAlterFields (const Nothing)

replaceDevice :: (FromAreaEffect f) => Grid.Entry Device -> Device
              -> Script f ()
replaceDevice entry device =
  emitAreaEffect $ EffReplaceDevice (Grid.geKey entry) (Just device)

resetTerrain :: (FromAreaEffect f) => [Position] -> Script f ()
resetTerrain positions = do
  tmap <- areaGet (terrainMap . arsTerrain)
  let update pos = (pos, tmapGet tmap pos)
  emitAreaEffect $ EffSetTerrain $ map update positions

setFields :: (FromAreaEffect f) => Field -> [Position] -> Script f ()
setFields field = emitAreaEffect . EffAlterFields fn where
  fn Nothing = Just field
  fn (Just field') = Just $
    case (field', field) of
      (BarrierWall a, BarrierWall b) -> BarrierWall (max a b)
      (BarrierWall a, _) -> BarrierWall a
      (FireWall a, FireWall b) -> FireWall (max a b)
      (IceWall a, IceWall b) -> IceWall (max a b)
      (PoisonCloud a, PoisonCloud b) -> PoisonCloud (max a b)
      (SmokeScreen a, SmokeScreen b) -> SmokeScreen (max a b)
      (Webbing a, Webbing b) -> Webbing (max a b)
      _ -> field

setTerrain :: (FromAreaEffect f) => [(Position, TerrainTile)] -> Script f ()
setTerrain = emitAreaEffect . EffSetTerrain

summonAllyMonster :: (FromAreaEffect f) => Position -> MonsterTag
                  -> Script f ()
summonAllyMonster startPos tag = do
  let mtype = getMonsterType tag
  arena <- areaGet arsBoundaryRect
  -- TODO Allow for non-SizeSmall monsters
  spot <- areaGet (flip4 arsFindOpenSpot startPos arena Set.empty)
  _ <- tryAddMonster spot Monster
         { monstAnim = NoAnim,
           monstAdrenaline = 0,
           monstDeadVar = Nothing,
           monstFaceDir = FaceLeft, -- TODO
           monstHealth = mtMaxHealth mtype,
           monstIsAlly = True,
           monstMoments = 0,
           monstName = "Summoned " ++ mtName mtype,
           monstScript = Nothing,
           monstStatus = initStatusEffects,
           monstTag = tag,
           monstTownAI = ChaseAI,
           monstType = mtype }
  return ()

tryAddMonster :: (FromAreaEffect f) => Position -> Monster
              -> Script f (Maybe (Grid.Entry Monster))
tryAddMonster position monster = do
  emitAreaEffect $ EffTryAddMonster position monster
{-
tryAddMonster :: (FromAreaEffect f) => MonsterTag -> MonsterType -> Position
              -> Maybe (Var Bool) -> MonsterTownAI -> Maybe MonsterScript
              -> Script f (Maybe (Entry Monster))
tryAddMonster tag mtype position mbDeadVar ai mbMscript = do
  emitAreaEffect $ EffTryAddMonster position $ Monster
    { monstAnim = NoAnim,
      monstAdrenaline = 0,
      monstDeadVar = mbDeadVar,
      monstFaceDir = FaceLeft,
      monstHealth = mtMaxHealth mtype,
      monstIsAlly = False,
      monstMoments = 0,
      monstName = mtName mtype,
      monstScript = mbMscript,
      monstStatus = initStatusEffects,
      monstTag = tag,
      monstTownAI = ai,
      monstType = mtype }
-}

-------------------------------------------------------------------------------

inflictAllPeriodicDamage :: (FromAreaEffect f) => Script f ()
inflictAllPeriodicDamage = do
  fields <- Map.assocs <$> areaGet arsFields
  fieldDamages <- fmap catMaybes $ forM fields $ \(pos, field) -> do
    case field of
      BarrierWall _ -> return Nothing
      FireWall baseDamage -> do
        damage <- (baseDamage *) <$> getRandomR 0.8 1.2
        return $ Just (HitPosition pos, FireDamage, damage)
      IceWall baseDamage -> do
        damage <- (baseDamage *) <$> getRandomR 0.8 1.2
        return $ Just (HitPosition pos, ColdDamage, damage)
      PoisonCloud basePoison -> do
        poison <- (basePoison *) <$> getRandomR 0.8 1.2
        Nothing <$ inflictPoison (HitPosition pos) poison
      SmokeScreen _ -> return Nothing
      Webbing ent -> do
        -- FIXME if something's there, remove the webbing
        Nothing <$ alterStatus (HitPosition pos) (seApplyEntanglement ent)
  charNums <- getAllConsciousCharacters
  party <- areaGet arsParty
  charPoisonDamages <- fmap catMaybes $ forM charNums $ \charNum -> do
    let totalPoison = sePoison $ chrStatus $ partyGetCharacter party charNum
    if totalPoison <= 0 then return Nothing else do
      let damage = totalPoison `ceilDiv` 5
      alterCharacterStatus charNum $ seAlterPoison $ subtract damage
      return $ Just (HitCharacter charNum, RawDamage, fromIntegral damage)
  monsters <- Grid.entries <$> areaGet arsMonsters
  monstPoisonDamages <- fmap catMaybes $ forM monsters $ \monstEntry -> do
    let totalPoison = sePoison $ monstStatus $ Grid.geValue monstEntry
    if totalPoison <= 0 then return Nothing else do
      let damage = totalPoison `ceilDiv` 5
      alterMonsterStatus (Grid.geKey monstEntry) $ seAlterPoison $
        subtract damage
      return $ Just (HitMonster (Grid.geKey monstEntry), RawDamage,
                     fromIntegral damage)
  dealDamage' True (fieldDamages ++ charPoisonDamages ++ monstPoisonDamages)

-------------------------------------------------------------------------------
-- Targeting:

circleArea :: SqDist -> Position -> [Position]
circleArea dist center =
  let limit = floor $ sqrt (fromIntegral dist :: Double)
      corner = Point limit limit
  in filter ((dist >=) . pSqDist center) $
     range (center `pSub` corner, center `pAdd` corner)

aoeTarget :: Int -> SqDist -> TargetKind (Position, [Position])
aoeTarget maxRange blastRadiusSquared = AreaTarget (ofRadius maxRange) fn
  where fn _ars _origin target = circleArea blastRadiusSquared target

beamTarget :: TargetKind (Position, [Position])
beamTarget = AreaTarget sightRangeSquared arsBeamPositions

splashTarget :: Int -> TargetKind (Position, [Position])
splashTarget maxRange = AreaTarget (ofRadius maxRange) fn where
  fn ars origin target =
    if origin == target || cannotSeeThrough (arsTerrainOpenness target ars)
    then [target] else
      let dir = ipointDir (target `pSub` origin)
      in [target, target `plusDir` pred dir, target `plusDir` dir,
          target `plusDir` succ dir]

wallTarget :: Int -> Int -> TargetKind (Position, [Position])
wallTarget maxRange radius = AreaTarget (ofRadius maxRange) fn where
  fn ars origin target =
    if origin == target || blocked target then [] else
      let (d1, d2, d3, d4) =
            if isCardinal dir
            then (pred $ pred dir, pred $ pred dir,
                  succ $ succ dir, succ $ succ dir)
            else (pred dir, pred $ pred $ pred dir,
                  succ dir, succ $ succ $ succ dir)
      in target : wing d1 d2 target radius ++ wing d3 d4 target radius
    where
      dir = ipointDir (target `pSub` origin)
      blocked pos =
        isJust (arsOccupant pos ars) ||
        case arsTerrainOpenness pos ars of
          TerrainOpen -> False
          TerrainHover -> False
          _ -> True
      wing dir1 dir2 start n =
        if n <= 0 then [] else
          let pos = start `plusDir` (if n `mod` 2 == 1 then dir1 else dir2)
          in if blocked pos then [] else pos : wing dir1 dir2 pos (n - 1)

-------------------------------------------------------------------------------
-- Private:

applyResistances :: StatusEffects -> Resistances -> DamageType -> Double
                 -> Double -> (Int, Double)
applyResistances status resist dmgType damage stun = (damage', stun')
  where
    armor = tmGet Armor resist * seArmorMultiplier status
    magicArmor = assert (armor >= 0) (sqrt armor) *
                 seMagicShieldMultiplier status
    damage' = round $ (damage *) $
              case dmgType of
                AcidDamage -> tmGet ResistChemical resist * magicArmor
                ColdDamage -> tmGet ResistCold resist * magicArmor
                EnergyDamage -> tmGet ResistEnergy resist * magicArmor
                FireDamage -> tmGet ResistFire resist * magicArmor
                MagicDamage -> magicArmor
                PhysicalDamage -> armor
                RawDamage -> 1
    stun' = stun * tmGet ResistStun resist

applyCharResistances :: Character -> DamageType -> Double -> Double
                     -> (Int, Double)
applyCharResistances char dmgType damage stun =
  applyResistances (chrStatus char) (chrResistances char) dmgType damage stun

applyMonsterResistances :: Monster -> DamageType -> Double -> Double
                        -> (Int, Double)
applyMonsterResistances monst dmgType damage stun =
  applyResistances (monstStatus monst) (mtResistances mt) dmgType damage stun
  where mt = monstType monst

characterScreamSound :: Character -> SoundTag
characterScreamSound char =
  case (chrClass char, chrAppearance char) of
    (WarriorClass, Appearance2) -> SndHurtMale
    (WarriorClass, Appearance3) -> SndHurtMale
    (RogueClass, Appearance1) -> SndHurtMale
    (RogueClass, Appearance2) -> SndHurtMale
    (HunterClass, Appearance0) -> SndHurtMale
    (HunterClass, Appearance3) -> SndHurtMale
    (AlchemistClass, Appearance0) -> SndHurtMale
    (AlchemistClass, Appearance2) -> SndHurtMale
    (ClericClass, Appearance1) -> SndHurtMale
    (ClericClass, Appearance3) -> SndHurtMale
    (MagusClass, Appearance0) -> SndHurtMale
    (MagusClass, Appearance1) -> SndHurtMale
    _ -> SndHurtFemale

getHitTargetOccupant :: (FromAreaEffect f) => HitTarget
                     -> Script f (Maybe (Either CharacterNumber
                                                (Grid.Entry Monster)))
getHitTargetOccupant (HitCharacter charNum) = return $ Just $ Left charNum
getHitTargetOccupant (HitMonster monstKey) =
  fmap Right <$> lookupMonsterEntry monstKey
getHitTargetOccupant (HitPosition pos) = areaGet (arsOccupant pos)

monsterHeadPos :: Grid.Entry Monster -> Position
monsterHeadPos entry =
  let Rect x y w _ = Grid.geRect entry
  in case monstFaceDir (Grid.geValue entry) of
       FaceLeft -> Point x y
       FaceRight -> Point (x + w - 1) y

-------------------------------------------------------------------------------
