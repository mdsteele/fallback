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
   -- ** Mojo/adrenaline
   alterMana, alterCharacterMojo, restoreMojoToFull, alterAdrenaline,
   -- ** Status effects
   alterStatus, alterCharacterStatus, alterMonsterStatus,
   grantInvisibility, inflictPoison, curePoison, inflictStun,
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
   addBasicEnemyMonster, setMonsterIsAlly, tryAddMonster, trySummonMonster,
   degradeMonstersSummonedBy, unsummonMonster, unsummonDependentsOf,
   tickSummonsByOneRound,
   -- ** Items
   grantAndEquipWeapon, grantItem,

   -- * Targeting
   aoeTarget, beamTarget, splashTarget, wallTarget)
where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (second)
import Control.Monad (forM_, when)
import Data.Array (range)
import Data.List (find)
import Data.Maybe (isNothing)
import qualified Data.Set as Set

import Fallback.Constants
  (framesPerRound, maxAdrenaline, momentsPerActionPoint, sightRange)
import Fallback.Control.Script
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import qualified Fallback.Data.TotalMap as TM (get)
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
import Fallback.State.Tags (AreaTag, ItemTag(..), MonsterTag, WeaponItemTag)
import Fallback.Utility (ceilDiv, forMaybeM, maybeM)

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
      status <- areaGet (chrStatus . arsGetCharacter charNum)
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
      let amount = round (baseAmount * chrRecuperation char)
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

setMonsterIsAlly :: (FromAreaEffect f) => Bool -> Grid.Key Monster
                 -> Script f ()
setMonsterIsAlly isAlly key = do
  withMonsterEntry key $ \entry -> do
    emitAreaEffect $ EffReplaceMonster key $ Just (Grid.geValue entry)
      { monstIsAlly = isAlly }

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
  let size = monstRectSize monster
  mbSpot <- do
    let blocked ars pos = arsOccupied pos ars ||
                          cannotWalkOn (arsTerrainOpenness pos ars)
    unblocked <- areaGet $ \ars pos ->
      not $ any (blocked ars) $ prectPositions $ makeRect pos size
    directions <- randomPermutation allDirections
    areaGet (find unblocked . arsAccessiblePositions directions summonerPos)
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
        let frames' = max 0 $ subtract framesPerRound $ msRemainingFrames ms
        if frames' <= 0 then return $ Just $ Grid.geKey entry else do
        emitAreaEffect $ EffReplaceMonster (Grid.geKey entry) $ Just monst
          { monstSummoning = Just ms { msRemainingFrames = frames' } }
        return Nothing
  mapM_ unsummonMonster keys

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
