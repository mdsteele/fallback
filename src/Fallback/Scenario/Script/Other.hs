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

module Fallback.Scenario.Script.Other
  (-- * Actions
   -- ** Movement
   getPartyPosition, setPartyPosition, partyWalkTo, charWalkTo, charLeapTo,
   teleport, exitTo, walkMonster, setMonsterTownAI,

   -- * Effects
   -- ** Damage
   dealDamage, dealDamageTotal, healDamage,
   -- ** Mojo/adrenaline
   alterMana, alterCharacterMojo, restoreMojoToFull, alterAdrenaline,
   -- ** Status effects
   alterStatus, grantInvisibility, inflictPoison, curePoison, inflictStun,
   -- ** Other
   grantExperience, removeFields, setFields,

   -- * Animation
   -- ** Camera motion
   shakeCamera,
   -- ** Creature animation
   faceCharacterToward, faceMonsterToward, faceMonsterAwayFrom,
   facePartyToward, setCharacterAnim, setMonsterAnim, setPartyAnim,
   getHitTargetHeadPos, getMonsterHeadPos, alterMonsterPose,

   -- * UI
   setMessage, narrate,
   forcedChoice, maybeChoice, multiChoice,
   ConvChoice(..), forcedConversationLoop,

   -- * Objects
   -- ** Devices
   addDevice_, removeDevice, replaceDevice,
   -- ** Monsters
   addBasicEnemyMonster, tryAddMonster, trySummonMonster,
   degradeMonstersSummonedBy, unsummonMonster, tickSummonsByOneRound,
   -- ** Items
   grantAndEquipWeapon, grantItem,

   -- * Other
   inflictAllPeriodicDamage,

   -- * Targeting
   aoeTarget, beamTarget, splashTarget, wallTarget)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (right, second)
import Control.Exception (assert)
import Control.Monad (foldM, forM_, unless, when)
import Data.Array (range)
import qualified Data.Foldable as Fold (any)
import Data.List (foldl1')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import qualified Data.Set as Set

import Fallback.Constants
  (baseFramesPerActionPoint, maxAdrenaline, momentsPerActionPoint, sightRange)
import Fallback.Control.Script
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM (get)
import Fallback.Scenario.Monsters (makeMonster)
import Fallback.Scenario.Script.Base
import Fallback.Scenario.Script.Doodad
import Fallback.State.Action (TargetKind(..))
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Party
import Fallback.State.Progress (Var)
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Status
import Fallback.State.Tags
  (AbilityTag(..), AreaTag, ItemTag(..), MonsterTag, WeaponItemTag)
import Fallback.Utility
  (ceilDiv, flip3, forMaybeM, groupKey, maybeM, sortKey, square, sumM)

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

charLeapTo :: CharacterNumber -> Position -> Script CombatEffect Int
charLeapTo charNum pos = do
  let frames = 20
  oldPos <- areaGet (arsCharacterPosition charNum)
  faceCharacterToward charNum pos
  emitEffect $ EffSetCharPosition charNum pos
  emitEffect $ EffSetCharAnim charNum $ JumpAnim frames frames oldPos
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
    ok <- emitAreaEffect $ EffTryMoveMonster gkey rect'
    when ok $ do
      let deltaX = pointX $ pos' `pSub` rectTopleft (Grid.geRect entry)
      let dir = if deltaX < 0 then FaceLeft else FaceRight
      let anim' = WalkAnim frames frames $ rectTopleft $ Grid.geRect entry
      alterMonsterPose gkey (\p -> p { cpAnim = anim', cpFaceDir = dir })
      wait frames

setMonsterTownAI :: (FromAreaEffect f) => Grid.Key Monster -> MonsterTownAI
                 -> Script f ()
setMonsterTownAI key townAI = withMonsterEntry key $ \entry -> do
  emitAreaEffect (EffReplaceMonster key $
                  Just (Grid.geValue entry) { monstTownAI = townAI })

-------------------------------------------------------------------------------
-- Damage:

dealDamage :: (FromAreaEffect f) => [(HitTarget, DamageType, Double)]
           -> Script f ()
dealDamage hits = dealDamageTotal hits >> return ()

-- | Like 'dealDamage', but return the total damage done to all targets (after
-- resistances).
dealDamageTotal :: (FromAreaEffect f) => [(HitTarget, DamageType, Double)]
                -> Script f Int
dealDamageTotal = dealDamageGeneral False

dealDamageGeneral :: (FromAreaEffect f) => Bool
                  -> [(HitTarget, DamageType, Double)] -> Script f Int
dealDamageGeneral gentle hits = do
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
  sumM inflict . map (foldl1' combine) . groupKey keyFn . sortKey keyFn =<<
    mapM resist =<< forMaybeM hits convert

-- | Inflict damage and stun on a character, ignoring armor and resistances.
-- Stun is measured in action points.
dealRawDamageToCharacter :: (FromAreaEffect f) => Bool -> CharacterNumber
                         -> Int -> Double -> Script f Int
dealRawDamageToCharacter gentle charNum damage stun = do
  char <- areaGet (arsGetCharacter charNum)
  -- If this is non-gentle damage, wake us up from being dazed, and add
  -- adrenaline.
  unless gentle $ do
    alterCharacterStatus charNum seWakeFromDaze
    party <- areaGet arsParty
    let adrenaline = adrenalineForDamage (chrAdrenalineMultiplier char)
                                         damage (chrMaxHealth party char)
    alterAdrenaline charNum (+ adrenaline)
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
  unless (gentle && damage == 0) $ do
    addFloatingNumberOnTarget damage (HitCharacter charNum)
  -- If we're in combat, the character can die:
  whenCombat $ when (health' <= 0) $ do
    alterCharacterMojo charNum (const 0)
    alterAdrenaline charNum (const 0)
    alterCharacterStatus charNum (const initStatusEffects)
    resources <- areaGet arsResources
    let images = rsrcCharacterImages resources (chrClass char)
                                     (chrAppearance char)
    faceDir <- emitEffect $ EffGetCharFaceDir charNum
    playSound (characterScreamSound char)
    playSound SndDie1
    pos <- areaGet (arsCharacterPosition charNum)
    addDeathDoodad images faceDir (makeRect pos (1, 1))
    -- Unsummon monsters as necessary.
    unsummonDependentsOf (Left charNum)
    -- If all characters are now dead, it's game over:
    alive <- areaGet (Fold.any chrIsConscious . partyCharacters . arsParty)
    unless alive $ emitAreaEffect EffGameOver
  return damage

-- | Inflict damage and stun on a monster, ignoring armor and resistances.
-- Stun is measured in action points.  The monster must exist.
dealRawDamageToMonster :: (FromAreaEffect f) => Bool -> Grid.Key Monster
                       -> Int -> Double -> Script f Int
dealRawDamageToMonster gentle key damage stun = do
  entry <- demandMonsterEntry key
  let monst = Grid.geValue entry
  -- If this is non-gentle damage, wake us up from being dazed and add
  -- adrenaline.
  unless gentle $ alterMonsterStatus key seWakeFromDaze
  let adrenaline' = monstAdrenaline monst +
        if gentle then 0 else
          adrenalineForDamage 1 damage $ mtMaxHealth $ monstType monst
  -- Do damage and stun:
  let health' = monstHealth monst - damage
  let moments' = max 0 (monstMoments monst -
                        round (stun * fromIntegral momentsPerActionPoint))
  let mbMonst' = if health' <= 0 then Nothing else Just monst
                   { monstAdrenaline = adrenaline',
                     monstPose = (monstPose monst) { cpAnim = HurtAnim 12 },
                     monstHealth = health', monstMoments = moments' }
  emitAreaEffect $ EffReplaceMonster key mbMonst'
  unless (gentle && damage == 0) $ do
    addFloatingNumberOnTarget damage (HitMonster key)
  -- If the monster is now dead, we need do to several things.
  when (isNothing mbMonst') $ do
    resources <- areaGet arsResources
    let mtype = monstType monst
    -- Add a death doodad.
    addDeathDoodad (rsrcMonsterImages resources (mtSize mtype)
                                      (mtImageRow mtype))
                   (cpFaceDir $ monstPose monst) (Grid.geRect entry)
    -- If the monster has a "dead" var, set it to True.
    maybeM (monstDeadVar monst) (emitAreaEffect . flip EffSetVar True)
    -- If this was an enemy monster, grant experience for killing it.
    unless (monstIsAlly monst) $ do
      grantExperience $ mtExperienceValue $ monstType monst
    -- Unsummon other monsters as necessary.
    unsummonDependentsOf (Right key)
  return damage

adrenalineForDamage :: Double -> Int -> Int -> Int
adrenalineForDamage multiplier damage maxHealth =
  round $ (fromIntegral maxAdrenaline * multiplier *) $ square $ min 1 $
  2 * fromIntegral damage / (fromIntegral maxHealth :: Double)

-------------------------------------------------------------------------------
-- Healing:

healDamage :: (FromAreaEffect f) => [(HitTarget, Double)] -> Script f ()
healDamage hits = do
  heals <- flip3 foldM Map.empty hits $ \totals (hitTarget, amount) -> do
    mbOccupant <- getHitTargetOccupant hitTarget
    return $ case mbOccupant of
               Nothing -> totals
               Just occupant ->
                 Map.alter (Just . (amount +) . fromMaybe 0)
                           (right Grid.geKey occupant) totals
  forM_ (Map.assocs heals) $ \(occupant, amount) -> do
    either healCharacter healMonster occupant amount

healCharacter :: (FromAreaEffect f) => CharacterNumber -> Double -> Script f ()
healCharacter charNum baseAmount = do
  multiplier <- recuperationMultiplier <$> areaGet (arsGetCharacter charNum)
  let amount = max 0 $ round (multiplier * baseAmount)
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrHealth = min (chrMaxHealth party char)
                           (chrHealth char + amount) }
  pos <- areaGet (arsCharacterPosition charNum)
  addBoomDoodadAtPosition HealBoom 4 pos
  addFloatingNumberOnTarget amount (HitCharacter charNum)

healMonster :: (FromAreaEffect f) => Grid.Key Monster -> Double -> Script f ()
healMonster key baseAmount = withMonsterEntry key $ \entry -> do
  let monst = Grid.geValue entry
  let amount = max 0 $ round baseAmount
  let health' = min (monstHealth monst + amount)
                    (mtMaxHealth $ monstType monst)
  let monst' = monst { monstHealth = health' }
  emitAreaEffect $ EffReplaceMonster key (Just monst')
  let prect = Grid.geRect entry
  forM_ (prectPositions prect) $ addBoomDoodadAtPosition HealBoom 4
  addFloatingNumberOnTarget amount (HitMonster key)

-------------------------------------------------------------------------------
-- Mojo/adrenaline:

alterMana :: (FromAreaEffect f) => HitTarget -> (Int -> Int) -> Script f ()
alterMana hitTarget fn = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      case chrClass char of
        ClericClass -> alterCharacterMojo charNum fn
        MagusClass -> alterCharacterMojo charNum fn
        _ -> return ()
    _ -> return ()

alterCharacterMojo :: (FromAreaEffect f) => CharacterNumber -> (Int -> Int)
                   -> Script f ()
alterCharacterMojo charNum fn = do
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrMojo = max 0 $ min (chrMaxMojo party char) $ fn (chrMojo char) }

restoreMojoToFull :: (FromAreaEffect f) => CharacterNumber -> Script f ()
restoreMojoToFull charNum = do
  party <- areaGet arsParty
  emitAreaEffect $ EffAlterCharacter charNum $ \char ->
    char { chrMojo = chrMaxMojo party char }

alterAdrenaline :: (FromAreaEffect f) => CharacterNumber -> (Int -> Int)
                -> Script f ()
alterAdrenaline charNum fn =
  emitAreaEffect $ EffAlterCharacter charNum $ \c ->
    c { chrAdrenaline = max 0 $ min maxAdrenaline $ fn (chrAdrenaline c) }

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
      when (seInvisibility status < invis) $ do
        alterCharacterStatus charNum $ seSetInvisibility invis
    Just (Right monstEntry) -> do
      let status = monstStatus $ Grid.geValue monstEntry
      when (seInvisibility status < invis) $ do
        alterMonsterStatus (Grid.geKey monstEntry) $ seSetInvisibility invis
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
      let poison = round $ (basePoison *) $ TM.get ResistChemical $
                   mtResistances $ monstType $ Grid.geValue monstEntry
      alterMonsterStatus (Grid.geKey monstEntry) $ seAlterPoison (poison +)
    Nothing -> return ()

-- | Cure poison damage from the target, taking the Recuperation skill into
-- account.
curePoison :: (FromAreaEffect f) => HitTarget -> Double -> Script f ()
curePoison hitTarget baseAmount = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      let amount = round (baseAmount * recuperationMultiplier char)
      alterCharacterStatus charNum $ seAlterPoison (max 0 . subtract amount)
    Just (Right monstEntry) -> do
      alterMonsterStatus (Grid.geKey monstEntry) $
        seAlterPoison (max 0 . subtract (round baseAmount))
    Nothing -> return ()

-- | Inflicts stun (measured in action points) onto the target, taking
-- resistances into account.
inflictStun :: HitTarget -> Double -> Script CombatEffect ()
inflictStun hitTarget stun = do
  mbOccupant <- getHitTargetOccupant hitTarget
  case mbOccupant of
    Just (Left charNum) -> do
      char <- areaGet (arsGetCharacter charNum)
      let stun' = max 0 (stun * chrGetResistance ResistStun char)
      moments <- emitEffect $ EffGetCharMoments charNum
      emitEffect $ EffSetCharMoments charNum $
        max 0 (moments - round (stun' * fromIntegral momentsPerActionPoint))
    Just (Right monstEntry) -> do
      let monst = Grid.geValue monstEntry
      let stun' = max 0 $ (stun *) $ TM.get ResistStun $ mtResistances $
                  monstType monst
      emitAreaEffect $ EffReplaceMonster (Grid.geKey monstEntry) $ Just monst
        { monstMoments = monstMoments monst -
                         round (stun' * fromIntegral momentsPerActionPoint) }
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
    let deltaX = pointX $ (pos `pSub`) $ monstHeadPos entry
    when (deltaX /= 0) $ do
      let dir = if deltaX < 0 then FaceLeft else FaceRight
      alterMonsterPose key (\p -> p { cpFaceDir = dir })

faceMonsterAwayFrom :: (FromAreaEffect f) => Grid.Key Monster -> Position
                    -> Script f ()
faceMonsterAwayFrom key pos = do
  faceMonsterToward key pos
  alterMonsterPose key (\p -> p { cpFaceDir = oppositeFaceDir (cpFaceDir p) })

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
setMonsterAnim key anim = alterMonsterPose key (\p -> p { cpAnim = anim })

setPartyAnim :: CreatureAnim -> Script TownEffect ()
setPartyAnim = emitEffect . EffSetPartyAnim

-- | Get the position of the given 'HitTarget'; for monster targets, this
-- returns the position of the monster's head.
getHitTargetHeadPos :: (FromAreaEffect f) => HitTarget -> Script f Position
getHitTargetHeadPos (HitCharacter charNum) =
  areaGet (arsCharacterPosition charNum)
getHitTargetHeadPos (HitMonster key) = getMonsterHeadPos key
getHitTargetHeadPos (HitPosition pos) = return pos

getMonsterHeadPos :: (FromAreaEffect f) => Grid.Key Monster
                  -> Script f Position
getMonsterHeadPos key = do
  entry <- demandMonsterEntry key
  return $ monstHeadPos entry

alterMonsterPose :: (FromAreaEffect f) => Grid.Key Monster
                 -> (CreaturePose -> CreaturePose) -> Script f ()
alterMonsterPose key fn = do
  withMonsterEntry key $ \entry -> do
    let monst = Grid.geValue entry
    emitAreaEffect $ EffReplaceMonster key $ Just monst
      { monstPose = fn (monstPose monst) }

-------------------------------------------------------------------------------
-- Messages and conversation:

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
-- Other script actions:

addBasicEnemyMonster :: (FromAreaEffect f) => Position -> MonsterTag
                     -> Maybe (Var Bool) -> MonsterTownAI -> Script f ()
addBasicEnemyMonster nearPos tag mbDeadVar townAi = do
  let monster = makeMonster tag
  within <- areaGet arsBoundaryRect
  -- TODO Allow for non-SizeSmall monsters
  spot <- areaGet $ \ars ->
    if not $ arsIsBlockedForParty ars nearPos then nearPos
    else arsFindOpenSpot ars nearPos within Set.empty
  _ <- tryAddMonster spot monster { monstDeadVar = mbDeadVar,
                                    monstTownAI = townAi }
  return ()

addDevice_ :: (FromAreaEffect f) => Device -> Position -> Script f ()
addDevice_ device pos = () <$ emitAreaEffect (EffTryAddDevice pos device)

grantAndEquipWeapon :: (FromAreaEffect f) => WeaponItemTag -> CharacterNumber
                    -> Script f ()
grantAndEquipWeapon tag charNum = do
  party <- areaGet arsParty
  let char = partyGetCharacter party charNum
  let mbOldItem = fmap WeaponItemTag $ eqpWeapon $ chrEquipment char
  emitAreaEffect $ EffAlterCharacter charNum (\c -> c { chrEquipment =
    (chrEquipment c) { eqpWeapon = Just tag } })
  maybeM mbOldItem grantItem

grantExperience :: (FromAreaEffect f) => Int -> Script f ()
grantExperience xp = do
  oldLevel <- areaGet (partyLevel . arsParty)
  emitAreaEffect $ EffGrantExperience xp
  newLevel <- areaGet (partyLevel . arsParty)
  when (newLevel > oldLevel) $ do
    setMessage $ "Party is now level " ++ show newLevel ++ "!"
    mapM_ return =<< areaGet arsPartyPositions -- FIXME doodads
    playSound SndLevelUp

grantItem :: (FromAreaEffect f) => ItemTag -> Script f ()
grantItem = emitAreaEffect . EffGrantItem

removeDevice :: (FromAreaEffect f) => Grid.Key Device -> Script f ()
removeDevice key = emitAreaEffect $ EffReplaceDevice key Nothing

removeFields :: (FromAreaEffect f) => [Position] -> Script f ()
removeFields = emitAreaEffect . EffAlterFields (const Nothing)

replaceDevice :: (FromAreaEffect f) => Grid.Entry Device -> Device
              -> Script f ()
replaceDevice entry device =
  emitAreaEffect $ EffReplaceDevice (Grid.geKey entry) (Just device)

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

tryAddMonster :: (FromAreaEffect f) => Position -> Monster
              -> Script f (Maybe (Grid.Entry Monster))
tryAddMonster position monster = do
  emitAreaEffect $ EffTryAddMonster position monster

-- | Summon a monster allied to and somewhere near the given summoner.  Return
-- 'True' on success, or 'False' if there was no open spot to place the
-- summoned monster.
trySummonMonster :: (FromAreaEffect f) =>
                 Either CharacterNumber (Grid.Key Monster) -> MonsterTag
              -> Int -> Bool -> Script f Bool
trySummonMonster summonerKey tag lifetime dieWhenGone = do
  (isAlly, summonerPos) <- do
    case summonerKey of
      Left charNum -> (,) True <$> areaGet (arsCharacterPosition charNum)
      Right monstKey -> do
        entry <- demandMonsterEntry monstKey
        return (monstIsAlly (Grid.geValue entry), monstHeadPos entry)
  let monster = makeMonster tag
  let size = sizeSize $ mtSize $ monstType monster
  mbSpot <- do
    let blocked ars pos = arsOccupied pos ars ||
                          cannotWalkOn (arsTerrainOpenness pos ars)
    unblocked <- areaGet $ \ars pos ->
      not $ any (blocked ars) $ prectPositions $ makeRect pos size
    directions <- randomPermutation allDirections
    listToMaybe . filter unblocked <$>
      areaGet (arsAccessiblePositions directions summonerPos)
  flip (maybe $ return False) mbSpot $ \spot -> do
  faceDir <- getRandomElem [FaceLeft, FaceRight]
  addSummonDoodad $ makeRect spot size
  mbEntry <- tryAddMonster spot monster
    { monstIsAlly = isAlly,
      monstName = "Summoned " ++ monstName monster,
      monstPose = (monstPose monster) { cpAlpha = 0, cpFaceDir = faceDir },
      monstSummoning = Just MonsterSummoning
        { msMaxFrames = lifetime,
          msRemainingFrames = lifetime,
          msSummmoner = summonerKey,
          msUnsummonWhenSummonerGone = dieWhenGone },
      monstTownAI = ChaseAI }
  when (isNothing mbEntry) $ fail "trySummonMonster: tryAddMonster failed"
  return True

-- | Call this when a summoned monster has zero remaining frames; it will
-- unsummon the monster, as well as any other summoned monsters that depend on
-- it.  If the monster no longer exists, this has no effect.
unsummonMonster :: (FromAreaEffect f) => Grid.Key Monster -> Script f ()
unsummonMonster key = do
  withMonsterEntry key $ \entry -> do
    addUnsummonDoodad entry
    -- Note that a monster being unsummoned doesn't count as it dying; no XP is
    -- awarded and the monstDeadVar is not set.
    emitAreaEffect $ EffReplaceMonster key Nothing
    unsummonDependentsOf (Right key)

-- | Call this when a creature dies or is unsummoned; it will unsummon all
-- monsters that 1) were summoned by the given creature, and 2) have
-- 'msUnsummonWhenSummonerGone' set to 'True'.
unsummonDependentsOf :: (FromAreaEffect f) =>
                        Either CharacterNumber (Grid.Key Monster)
                     -> Script f ()
unsummonDependentsOf summoner = do
  monsters <- areaGet arsMonsters
  forM_ (Grid.entries monsters) $ \entry -> do
    let monst = Grid.geValue entry
    maybeM (monstSummoning monst) $ \ms -> do
      when (msUnsummonWhenSummonerGone ms && msSummmoner ms == summoner) $ do
        unsummonMonster (Grid.geKey entry)

-- | Cause all monsters that were summoned by the given summoner to lose one
-- quarter of their remaining lifetime.
degradeMonstersSummonedBy :: (FromAreaEffect f) =>
                             Either CharacterNumber (Grid.Key Monster)
                          -> Script f ()
degradeMonstersSummonedBy summoner = do
  monsters <- areaGet arsMonsters
  forM_ (Grid.entries monsters) $ \entry -> do
    let monst = Grid.geValue entry
    maybeM (monstSummoning monst) $ \ms -> do
      when (msSummmoner ms == summoner) $ do
        let frames' = (msRemainingFrames ms) * 3 `ceilDiv` 4
        emitAreaEffect $ EffReplaceMonster (Grid.geKey entry) $ Just monst
          { monstSummoning = Just ms { msRemainingFrames = frames' } }

-- | Call this once per town step to reduce the remaining frames of all
-- summoned monsters by one round.
tickSummonsByOneRound :: Script TownEffect ()
tickSummonsByOneRound = do
  monsters <- areaGet arsMonsters
  keys <- forMaybeM (Grid.entries monsters) $ \entry -> do
    let monst = Grid.geValue entry
    case monstSummoning monst of
      Nothing -> return Nothing
      Just ms -> do
        let frames' = max 0 $ subtract baseFramesPerActionPoint $
                      msRemainingFrames ms
        if frames' <= 0 then return $ Just $ Grid.geKey entry else do
        emitAreaEffect $ EffReplaceMonster (Grid.geKey entry) $ Just monst
          { monstSummoning = Just ms { msRemainingFrames = frames' } }
        return Nothing
  mapM_ unsummonMonster keys

-------------------------------------------------------------------------------

inflictAllPeriodicDamage :: (FromAreaEffect f) => Script f ()
inflictAllPeriodicDamage = do
  fields <- Map.assocs <$> areaGet arsFields
  fieldDamages <- forMaybeM fields $ \(pos, field) -> do
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
      Webbing rounds -> do
        -- FIXME if something's there, remove the webbing
        Nothing <$ alterStatus (HitPosition pos) (seApplyEntanglement rounds)
  charNums <- getAllConsciousCharacters
  party <- areaGet arsParty
  charPoisonDamages <- forMaybeM charNums $ \charNum -> do
    let totalPoison = sePoison $ chrStatus $ partyGetCharacter party charNum
    if totalPoison <= 0 then return Nothing else do
      let damage = totalPoison `ceilDiv` 5
      alterCharacterStatus charNum $ seAlterPoison $ subtract damage
      return $ Just (HitCharacter charNum, RawDamage, fromIntegral damage)
  monsters <- Grid.entries <$> areaGet arsMonsters
  monstPoisonDamages <- forMaybeM monsters $ \monstEntry -> do
    let totalPoison = sePoison $ monstStatus $ Grid.geValue monstEntry
    if totalPoison <= 0 then return Nothing else do
      let damage = totalPoison `ceilDiv` 5
      alterMonsterStatus (Grid.geKey monstEntry) $ seAlterPoison $
        subtract damage
      return $ Just (HitMonster (Grid.geKey monstEntry), RawDamage,
                     fromIntegral damage)
  dealDamageGeneral True (fieldDamages ++ charPoisonDamages ++
                          monstPoisonDamages) >> return ()

-------------------------------------------------------------------------------
-- Targeting:

-- TODO move this stuff to Fallback.State.Action

circleArea :: SqDist -> Position -> [Position]
circleArea dist center =
  let limit = floor (sqDistRadius dist)
      corner = Point limit limit
  in filter ((dist >=) . pSqDist center) $
     range (center `pSub` corner, center `pAdd` corner)

aoeTarget :: Int -> SqDist -> TargetKind (Position, [Position])
aoeTarget maxRange blastRadiusSquared = AreaTarget fn maxRange
  where fn _ars _origin target = circleArea blastRadiusSquared target

beamTarget :: TargetKind (Position, [Position])
beamTarget = AreaTarget arsBeamPositions sightRange

splashTarget :: Int -> TargetKind (Position, [Position])
splashTarget maxRange = AreaTarget fn maxRange where
  fn ars origin target =
    if origin == target || cannotSeeThrough (arsTerrainOpenness target ars)
    then [target] else
      let dir = ipointDir (target `pSub` origin)
      in [target, target `plusDir` pred dir, target `plusDir` dir,
          target `plusDir` succ dir]

wallTarget :: Int -> Int -> TargetKind (Position, [Position])
wallTarget maxRange radius = AreaTarget fn maxRange where
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
        arsOccupied pos ars ||
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
    armor = TM.get Armor resist * seArmorMultiplier status
    magicArmor = assert (armor >= 0) (sqrt armor) *
                 seMagicShieldMultiplier status
    damage' = round $ (damage *) $
              case dmgType of
                AcidDamage -> TM.get ResistChemical resist * magicArmor
                ColdDamage -> TM.get ResistCold resist * magicArmor
                EnergyDamage -> TM.get ResistEnergy resist * magicArmor
                FireDamage -> TM.get ResistFire resist * magicArmor
                MagicDamage -> magicArmor
                PhysicalDamage -> armor
                RawDamage -> 1
    stun' = stun * TM.get ResistStun resist

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

recuperationMultiplier :: Character -> Double
recuperationMultiplier = chrAbilityMultiplier Recuperation 1.1 1.2 1.4

-------------------------------------------------------------------------------
