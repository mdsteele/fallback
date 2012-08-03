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

{-# LANGUAGE GADTs, KindSignatures, Rank2Types, ScopedTypeVariables #-}

module Fallback.State.Area where

import Control.Arrow (right)
import Control.Applicative ((<$), (<$>))
import Data.Function (on)
import Data.List (find, nubBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Traversable (for)
import System.Random (Random, randomRIO)

import Fallback.Constants (maxPartyLevel)
import Fallback.Control.Script (Script)
import Fallback.Data.Clock (Clock, clockInc)
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import qualified Fallback.Data.PriorityQueue as PQ
import qualified Fallback.Data.SparseMap as SM
import Fallback.Draw (Minimap)
import Fallback.Sound (Sound, fadeOutMusic, loopMusic, playSound, stopMusic)
import Fallback.State.Camera (Camera, setCameraShake, tickCamera)
import Fallback.State.Creature
import Fallback.State.Doodad
  (Doodads, Message, decayMessage, makeMessage, tickDoodads)
import Fallback.State.FOV (fieldOfView)
import Fallback.State.Minimap (updateMinimapFromTerrain)
import Fallback.State.Party
import Fallback.State.Progress
  (DeviceId, HasProgress, Var, VarType, progressSetVar)
import Fallback.State.Resources (MusicTag, Resources, musicPath)
import Fallback.State.Simple
import Fallback.State.Tags (AreaTag, ItemTag, QuestTag)
import Fallback.State.Terrain

-------------------------------------------------------------------------------

data AreaCommonState = AreaCommonState
  { acsCamera :: Camera,
    acsClock :: Clock,
    acsDevices :: Grid.Grid Device,
    acsDoodads :: Doodads,
    acsFields :: Map.Map Position Field,
    acsMessage :: Maybe Message,
    acsMinimap :: Minimap,
    acsMonsters :: Grid.Grid Monster,
    acsParty :: Party,
    acsResources :: Resources,
    acsTerrain :: Terrain,
    acsVisible :: Set.Set Position }

tickAnimations :: DPoint -> [Position] -> AreaCommonState -> AreaCommonState
tickAnimations cameraGoalTopleft eyes acs =
  acs { acsClock = clockInc (acsClock acs),
        acsCamera = tickCamera cameraGoalTopleft (acsCamera acs),
        acsDoodads = tickDoodads (acsDoodads acs),
        acsMessage = acsMessage acs >>= decayMessage,
        acsMonsters = Grid.update tickMonsterPose (acsMonsters acs) }
  where
    tickMonsterPose entry = monst { monstPose = pose' } where
      monst = Grid.geValue entry
      pose' = tickCreaturePose invis canSee (monstPose monst)
      invis = monstInvisibility monst
      canSee =
        case invis of
          NoInvisibility -> True
          MinorInvisibility ->
            any (rectContains $ expandPrect $ Grid.geRect entry) eyes
          MajorInvisibility -> False

updateMinimap :: AreaCommonState -> [Position] -> IO ()
updateMinimap acs visible = do
  updateMinimapFromTerrain (acsMinimap acs) (acsTerrain acs) visible

-------------------------------------------------------------------------------

class (HasProgress a) => AreaState a where
  -- | The boundaries of movement.  In town mode, this is the whole map; in
  -- combat mode, it is the combat arena.
  arsBoundaryRect :: a -> PRect

  -- | Return the position of a particular character.  In town mode, this will
  -- be the party position, regardless of which character was asked for.  In
  -- combat mode, this returns the position of the character, or if the
  -- character is unconscious, then the last position the character was at.
  arsCharacterPosition :: CharacterNumber -> a -> Position

  arsCharacterAtPosition :: Position -> a -> Maybe CharacterNumber

  arsCommon :: a -> AreaCommonState
  arsSetCommon :: a -> AreaCommonState -> a

  -- | Return all positions occupied by party members.  In town mode, this will
  -- be the single position of the party.  In combat mode, this will have up to
  -- four positions, one for each conscious party member.
  arsPartyPositions :: a -> [Position]

  arsUpdateVisibility :: a -> IO a

  -- | Return the set of positions visible to just one character.  In town
  -- mode, this is the same as 'arsVisibleForParty' (because all characters are
  -- in the same position), but for combat mode it is not.
  arsVisibleForCharacter :: CharacterNumber -> a -> Set.Set Position

-------------------------------------------------------------------------------

arsCamera :: (AreaState a) => a -> Camera
arsCamera = acsCamera . arsCommon

arsClock :: (AreaState a) => a -> Clock
arsClock = acsClock . arsCommon

arsCurrentArea :: (AreaState a) => a -> AreaTag
arsCurrentArea = partyCurrentArea . acsParty . arsCommon

arsDevices :: (AreaState a) => a -> Grid.Grid Device
arsDevices = acsDevices . arsCommon

arsExploredMap :: (AreaState a) => a -> ExploredMap
arsExploredMap ars = partyExploredMap (arsTerrain ars) (arsParty ars)

arsFields :: (AreaState a) => a -> Map.Map Position Field
arsFields = acsFields . arsCommon

arsGetCharacter :: (AreaState a) => CharacterNumber -> a -> Character
arsGetCharacter charNum ars = partyGetCharacter (arsParty ars) charNum

arsIsVisible :: (AreaState a) => a -> Position -> Bool
arsIsVisible ars pos = Set.member pos (arsVisibleForParty ars)

arsIsVisibleToCharacter :: (AreaState a) => CharacterNumber -> a -> Position
                        -> Bool
arsIsVisibleToCharacter charNum ars pos =
  Set.member pos (arsVisibleForCharacter charNum ars)

arsMonsters :: (AreaState a) => a -> Grid.Grid Monster
arsMonsters = acsMonsters . arsCommon

arsMinimap :: (AreaState a) => a -> Minimap
arsMinimap = acsMinimap . arsCommon

arsParty :: (AreaState a) => a -> Party
arsParty = acsParty . arsCommon

arsResources :: (AreaState a) => a -> Resources
arsResources = acsResources . arsCommon

arsTerrain :: (AreaState a) => a -> Terrain
arsTerrain = acsTerrain . arsCommon

arsTerrainOpenness :: (AreaState a) => Position -> a -> TerrainOpenness
arsTerrainOpenness pos ars =
  case Map.lookup pos $ arsFields ars of
    Just (BarrierWall _) -> TerrainSolid
    Just (SmokeScreen _) -> smokifyOpenness openness
    _ -> if rectContains (arsBoundaryRect ars) pos then openness
         else solidifyOpenness openness
  where openness = ttOpenness $ terrainGetTile pos $ arsTerrain ars

arsVisibleForParty :: (AreaState a) => a -> Set.Set Position
arsVisibleForParty = acsVisible . arsCommon

arsIsOpaque :: (AreaState a) => a -> Position -> Bool
arsIsOpaque ars pos = cannotSeeThrough $ arsTerrainOpenness pos ars

-- | Determine if the given monster cannot occupy the given position (for large
-- monsters, this position corresponds to the top-left position of the
-- monster's rectangle) without falling afoul of the party, terrain, and/or
-- other monsters.
arsIsBlockedForMonster :: (AreaState a) => Grid.Entry Monster -> a -> Position
                       -> Bool
arsIsBlockedForMonster ge ars pos =
  any (rectContains rect') (arsPartyPositions ars) ||
  any ((if monstCanFly $ Grid.geValue ge
        then cannotFlyOver else cannotWalkOn) .
       flip arsTerrainOpenness ars) (prectPositions rect') ||
  not (Grid.couldMove (Grid.geKey ge) rect' $ arsMonsters ars)
  where rect' = makeRect pos $ rectSize $ Grid.geRect ge

-- | Determine if a character could occupy the given position without falling
-- afoul of terrain, monsters, or other characters.
arsIsBlockedForParty :: (AreaState a) => a -> Position -> Bool
arsIsBlockedForParty ars pos =
  cannotWalkOn (arsTerrainOpenness pos ars) || arsOccupied pos ars

-- | Determine if there are any enemy monsters that could reach one or more
-- party members within four steps, taking into account both terrain and the
-- whether the monster(s) can fly.  Monsters that are not currently within
-- line-of-sight to the party do not count.
arsAreEnemiesNearby :: (AreaState a) => a -> Bool
arsAreEnemiesNearby ars = check initQueue initVisited where

  check :: PQ.PriorityQueue (Bool, Int) Position -> Set.Set Position -> Bool
  check queue visited =
    case PQ.popWithPriority queue of
      Just (((flying, steps), pos), queue') ->
        (pos `Set.member` arsVisibleForParty ars && hasEnemy pos flying) ||
        (if steps >= maxSteps then check queue' visited
         else expand flying (steps + 1) pos queue' visited)
      Nothing -> False

  expand :: Bool -> Int -> Position -> PQ.PriorityQueue (Bool, Int) Position
         -> Set.Set Position -> Bool
  expand flying steps pos queue visited =
    let children = filter (`Set.notMember` visited) $
                   map (pos `plusDir`) allDirections
        enqueueChild child =
          let open = arsTerrainOpenness child ars
          in if canWalkOn open then PQ.insert (flying, steps) child
             else if canFlyOver open then PQ.insert (True, steps) child else id
    in check (foldr enqueueChild queue children)
             (foldr Set.insert visited children)

  hasEnemy :: Position -> Bool -> Bool
  hasEnemy pos fly =
    case Grid.search pos (arsMonsters ars) of
      Just entry ->
        let monst = Grid.geValue entry
        in not (monstIsAlly monst) && (not fly || monstCanFly monst)
      Nothing -> False

  initVisited = Set.fromList $ arsPartyPositions ars
  initQueue = PQ.fromList $ map ((,) (False, 0)) $ arsPartyPositions ars
  maxSteps = 4 :: Int

-- | Lazily compute all positions that can be reached via walking from the
-- given start position (ignoring any creatures that may be in the way).  The
-- positions are ordered by distance from the start position, except that as a
-- special case the start position itself comes just after any adjacent
-- positions.
arsAccessiblePositions :: (AreaState a) => [Direction] -> Position -> a
                       -> [Position]
arsAccessiblePositions dirs startPos ars = generate initQueue initVisited where
  generate queue visited =
    case PQ.pop queue of
      Nothing -> []
      Just (pos, queue') -> pos : generate queue'' visited' where
        positions = expand visited pos
        queue'' = foldr (uncurry PQ.insert) queue' $ map annotate positions
        visited' = foldr Set.insert visited positions
  annotate pos = (pSqDist startPos pos, pos)
  expand visited center = filter ok $ map (plusDir center) dirs where
    ok pos = Set.notMember pos visited && isOpen pos
  isOpen pos = canWalkOn $ arsTerrainOpenness pos ars
  initQueue = let most = map annotate $ expand Set.empty startPos
              in PQ.fromList $ if not (isOpen startPos) then most
                               else (SqDist 3, startPos) : most
  initVisited = Set.fromList $ PQ.elems initQueue

-- TODO: deprecated (use arsAccessiblePositions instead)
arsFindOpenSpot :: (AreaState a) => a -> Position -> IRect -> Set.Set Position
                -> Position
arsFindOpenSpot ars start within claimed = check Set.empty [start] where
  check _ [] = start  -- There are no open spots; just give up.
  check visited (next : rest) =
    let ps = filter (canWalkOn . flip arsTerrainOpenness ars) $
             filter (flip Set.notMember visited) $
             filter (rectContains within) $ map (next `plusDir`) allDirections
    in fromMaybe (check (foldr Set.insert visited ps) (rest ++ ps))
                 (find (\p -> Set.notMember p claimed &&
                              not (Grid.occupied p (arsMonsters ars))) ps)

-- | If you shoot a beam spell from the @start@ position, passing through the
-- @thru@ position, what positions does it hit?  It will stop when it reaches
-- either an opaque position or the edge of the arena rectangle.
arsBeamPositions :: (AreaState a) => a -> Position {-^start-}
                 -> Position {-^thru-} -> [Position]
arsBeamPositions ars start thru =
  let delta = thru `pSub` start
  in if delta == pZero then [start] else
       let arena = arsBoundaryRect ars
           blocked pos = not (rectContains arena pos) || arsIsOpaque ars pos
           takeThru _ [] = []
           takeThru p (x : xs) = if p x then [x] else x : takeThru p xs
       in takeThru blocked $ drop 1 $ bresenhamPositions start $
          until (not . rectContains arena) (pAdd delta) start

arsCharacterJumpDestinations :: (AreaState a) => Int -> CharacterNumber -> a
                             -> Set.Set Position
arsCharacterJumpDestinations radius charNum ars =
  Set.filter (\pos -> not (isBlocked pos || arsOccupied pos ars)) $
  fieldOfView (terrainSize $ arsTerrain ars) isBlocked (ofRadius radius)
              (arsCharacterPosition charNum ars) Set.empty
  where isBlocked pos = cannotWalkOn (arsTerrainOpenness pos ars)

-- | Get the occupant of the given position (either a character or a monster),
-- if any.
arsOccupant :: (AreaState a) => Position -> a
            -> Maybe (Either CharacterNumber (Grid.Entry Monster))
arsOccupant pos ars =
  case arsCharacterAtPosition pos ars of
    Just charNum -> Just (Left charNum)
    Nothing -> Right <$> Grid.search pos (arsMonsters ars)

-- | Return 'True' if the given position is occupied (either by a character or
-- a monster), 'False' otherwise.
arsOccupied :: (AreaState a) => Position -> a -> Bool
arsOccupied pos ars = isJust (arsOccupant pos ars)

-- | Return a list of all occupants of the given positions, with no occupant
-- appearing more than once (e.g. in the case that a larger monster occupies
-- multiple positions).
arsOccupants :: (AreaState a) => [Position] -> a
             -> [Either CharacterNumber (Grid.Entry Monster)]
arsOccupants ps ars = nubBy ((==) `on` (right Grid.geKey)) $
                      mapMaybe (flip arsOccupant ars) ps

-- | Return a list of all positions that are occupied by a character or by an
-- ally monster.
arsAllyOccupiedPositions :: (AreaState a) => a -> [Position]
arsAllyOccupiedPositions ars =
  (arsPartyPositions ars ++) $ concatMap (prectPositions . Grid.geRect) $
  filter (monstIsAlly . Grid.geValue) $ Grid.entries (arsMonsters ars)

-------------------------------------------------------------------------------
-- AreaState setters:

arsSetMessage :: (AreaState a) => String -> a -> a
arsSetMessage text ars =
  arsSetCommon ars (arsCommon ars) { acsMessage = Just (makeMessage text) }

-------------------------------------------------------------------------------

data AreaExit = AreaExit
  { aeDestination :: AreaTag,
    aeRects :: [PRect] }

-------------------------------------------------------------------------------

data Device = Device
  { devId :: DeviceId,
    devInteract :: Grid.Entry Device -> CharacterNumber ->
                   Script AreaEffect (),
    devRadius :: Int }

-------------------------------------------------------------------------------

data Targeting :: * -> * where
  TargetingAlly :: Int -> Targeting (Either Position CharacterNumber)
  TargetingArea :: (forall a. (AreaState a) => a -> Position -> Position ->
                    [Position]) -> Int -> Targeting (Position, [Position])
  TargetingJump :: (forall a. (AreaState a) => a -> Position -> Position ->
                    [Position])
                -> Set.Set Position -> Targeting (Position, [Position])
  TargetingMulti :: Int {-max num targets-} -> Int {-range-}
                 -> [Position] {-targets so far-} -> Targeting [Position]
  TargetingSingle :: Int -> Targeting Position

-------------------------------------------------------------------------------

decayFields :: Int -> Map.Map Position Field -> IO (Map.Map Position Field)
decayFields frames fields = fmap (Map.mapMaybe id) $ for fields $ \field -> do
  let decay halflife = do
        let probKeep = 0.5 ** (fromIntegral frames / halflife) :: Double
        keep <- (probKeep >) <$> randomRIO (0, 1)
        return $ if keep then Just field else Nothing
  case field of
    BarrierWall duration -> do
      return $ if duration <= frames then Nothing
               else Just $ BarrierWall (duration - frames)
    FireWall _ -> decay 360
    IceWall _ -> decay 480
    PoisonCloud _ -> decay 300
    SmokeScreen halflife -> decay halflife
    Webbing _ -> return (Just field)

-------------------------------------------------------------------------------

-- | Effects that only impact the party and can be resolved in a
-- non-mode-specific way.
data PartyEffect :: * -> * where
  EffAlterCharacter :: CharacterNumber -> (Character -> Character)
                    -> PartyEffect ()
  -- Change how many coins the party has.
  EffAlterCoins :: (Integer -> Integer) -> PartyEffect ()
  -- Print a debugging string to the console.
  EffDebug :: String -> PartyEffect ()
  -- Give experience points to the party.
  EffGrantExperience :: Int -> PartyEffect ()
  -- Add an item to the party inventory.
  EffGrantItem :: ItemTag -> PartyEffect ()
  -- Change the music.
  EffMusicStart :: MusicTag -> PartyEffect ()
  -- Immediately stop the currently playing music.
  EffMusicStop :: PartyEffect ()
  -- Fade out the currently playing music over the given number of seconds.
  EffMusicFadeOut :: Double -> PartyEffect ()
  -- Play a sound effect.
  EffPlaySound :: Sound -> PartyEffect ()
  -- Remove all copies of an item from the party.
  EffPurgeItem :: ItemTag -> PartyEffect ()
  -- Generate a random value in the specified range.
  EffRandom :: (Random a) => a -> a -> PartyEffect a
  -- Remove the item in the given slot (if any) from the party.
  EffRemoveItem :: ItemSlot -> PartyEffect ()
  -- Set whether the specified area is cleared.
  EffSetAreaCleared :: AreaTag -> Bool -> PartyEffect ()
  -- Set the party's level cap.
  EffSetLevelCap :: Int -> PartyEffect ()
  -- Set the status for a given quest.
  EffSetQuestStatus :: QuestTag -> QuestStatus -> PartyEffect ()
  -- Change the value of a scenario variable.
  EffSetVar :: (VarType a) => Var a -> a -> PartyEffect ()

-- | Effects that can occur in any AreaState.
data AreaCommonEffect :: * -> * where
  EffAreaParty :: PartyEffect a -> AreaCommonEffect a
  EffAlterDoodads :: (Doodads -> Doodads) -> AreaCommonEffect ()
  EffAlterFields :: (Maybe Field -> Maybe Field) -> [Position]
                 -> AreaCommonEffect ()
  EffAreaGet :: (forall s. (AreaState s) => s -> a) -> AreaCommonEffect a
  EffMessage :: String -> AreaCommonEffect ()
  EffTryAddDevice :: Position -> Device
                  -> AreaCommonEffect (Maybe (Grid.Entry Device))
  EffTryAddMonster :: Position -> Monster
                   -> AreaCommonEffect (Maybe (Grid.Entry Monster))
  EffTryMoveMonster :: Grid.Key Monster -> PRect -> AreaCommonEffect Bool
  EffReplaceDevice :: Grid.Key Device -> Maybe Device -> AreaCommonEffect ()
  EffReplaceMonster :: Grid.Key Monster -> Maybe Monster -> AreaCommonEffect ()
  EffShakeCamera :: Double -> Int -> AreaCommonEffect ()
  EffSetTerrain :: [(Position, TerrainTile)] -> AreaCommonEffect ()

-- | Effects that can occur in town mode or combat mode, but that must be
-- handled differently depending on the mode.
data AreaEffect :: * -> * where
  EffAreaCommon :: AreaCommonEffect a -> AreaEffect a
  EffFork :: Script AreaEffect () -> AreaEffect ()
  EffGameOver :: AreaEffect ()
  -- TODO: As currently implemented, EffIfCombat breaks concurrency.  That is,
  -- we stop the world and execute the sub-script, with other "threads" unable
  -- to continue until the sub-script finishes, even if the sub-script contains
  -- EffWait.  Is it possible to make these two play nice together?
  EffIfCombat :: Script CombatEffect a -> Script TownEffect a -> AreaEffect a
  EffMultiChoice :: String -> [(String, a)] -> Maybe a -> AreaEffect a
  EffNarrate :: String -> AreaEffect ()
  EffWait :: AreaEffect ()

-- | Effects that can only happen while in combat mode.
data CombatEffect :: * -> * where
  EffCombatArea :: AreaEffect a -> CombatEffect a
  EffEndCombat :: CombatEffect ()
  EffGetCharFaceDir :: CharacterNumber -> CombatEffect FaceDir
  EffGetCharMoments :: CharacterNumber -> CombatEffect Int
  EffSetCharAnim :: CharacterNumber -> CreatureAnim -> CombatEffect ()
  EffSetCharFaceDir :: CharacterNumber -> FaceDir -> CombatEffect ()
  EffSetCharMoments :: CharacterNumber -> Int -> CombatEffect ()
  EffSetCharPosition :: CharacterNumber -> Position -> CombatEffect ()

-- | Effects that can only happen while in town mode.
data TownEffect :: * -> * where
  EffTownArea :: AreaEffect a -> TownEffect a
  EffExitTowardArea :: AreaTag -> TownEffect ()
  EffGetActiveCharacter :: TownEffect CharacterNumber
  EffGetPartyFaceDir :: TownEffect FaceDir
  EffGetPartyPosition :: TownEffect Position
  EffSetPartyAnim :: CreatureAnim -> TownEffect ()
  EffSetPartyFaceDir :: FaceDir -> TownEffect ()
  EffSetPartyPosition :: Position -> TownEffect ()
  EffShop :: [Either Ingredient ItemTag] -> TownEffect ()
  EffStartCombat :: Bool {-can run away-} -> Position {-arena topleft-}
                 -> TownEffect ()
  EffTeleportToArea :: AreaTag -> Position -> TownEffect ()

-------------------------------------------------------------------------------

executePartyEffect :: PartyEffect a -> Party -> IO (a, Party)
executePartyEffect eff party =
  case eff of
    EffAlterCharacter charNum fn ->
      return ((), partyAlterCharacter charNum fn party)
    EffAlterCoins fn ->
      return ((), party { partyCoins = max 0 $ fn $ partyCoins party })
    EffDebug string -> ((), party) <$ putStrLn string
    EffGrantExperience xp -> return ((), partyGrantExperience xp party)
    EffGrantItem tag -> return ((), partyGrantItem tag party)
    EffMusicStart tag -> ((), party) <$ loopMusic (musicPath tag)
    EffMusicStop -> ((), party) <$ stopMusic
    EffMusicFadeOut seconds -> ((), party) <$ fadeOutMusic seconds
    EffPlaySound sound -> ((), party) <$ playSound sound
    EffPurgeItem tag -> return ((), partyPurgeItem tag party)
    EffRandom lo hi -> do
      value <- randomRIO (lo, hi)
      return (value, party)
    EffRemoveItem slot -> return ((), partyRemoveItem slot party)
    EffSetAreaCleared tag clear -> do
      let cleared' = (if clear then Set.insert tag else Set.delete tag)
                     (partyClearedAreas party)
      return ((), party { partyClearedAreas = cleared' })
    EffSetLevelCap cap -> do
      return ((), party { partyLevelCap = max 1 $ min maxPartyLevel cap })
    EffSetQuestStatus tag qs -> do
      return ((), party { partyQuests = SM.set tag qs (partyQuests party) })
    EffSetVar var value -> do
      let progress' = progressSetVar var value $ partyProgress party
      return ((), party { partyProgress = progress' })

executeAreaCommonEffect :: forall a s. (AreaState s) => AreaCommonEffect a -> s
                        -> IO (a, s)
executeAreaCommonEffect eff ars = do
  let acs = arsCommon ars
  case eff of
    EffAreaParty partyEff -> do
      (result, party') <- executePartyEffect partyEff (arsParty ars)
      return (result, set acs { acsParty = party' })
    EffAlterDoodads fn -> change acs { acsDoodads = fn (acsDoodads acs) }
    EffAlterFields fn ps -> do
      let fields' = foldr (Map.alter fn) (acsFields acs) ps
      ars' <- arsUpdateVisibility $ set acs { acsFields = fields' }
      return ((), ars')
    EffAreaGet fn -> return (fn ars, ars)
    EffMessage text -> change acs { acsMessage = Just (makeMessage text) }
    EffTryAddDevice pos device -> do
      case Grid.tryInsert (makeRect pos (1, 1)) device (acsDevices acs) of
        Nothing -> return (Nothing, ars)
        Just (entry, devices') ->
          return (Just entry, set acs { acsDevices = devices' })
    EffTryAddMonster topleft monster -> do
      case Grid.tryInsert (makeRect topleft $ monstRectSize monster) monster
                          (acsMonsters acs) of
        Nothing -> return (Nothing, ars)
        Just (entry, monsters') ->
          return (Just entry, set acs { acsMonsters = monsters' })
    EffTryMoveMonster monstKey rect -> do
      case Grid.tryMove monstKey rect (acsMonsters acs) of
        Nothing -> return (False, ars)
        Just grid' -> return (True, set acs { acsMonsters = grid' })
    EffReplaceDevice key mbDevice' -> do
      change acs { acsDevices = maybe (Grid.delete key) (Grid.replace key)
                                      mbDevice' (acsDevices acs) }
    EffReplaceMonster key mbMonst' -> do
      change acs { acsMonsters = maybe (Grid.delete key) (Grid.replace key)
                                       mbMonst' (acsMonsters acs) }
    EffSetTerrain updates -> do
      let terrain' = foldr (uncurry terrainSetTile) (acsTerrain acs) updates
      ars' <- arsUpdateVisibility $ set acs { acsTerrain = terrain' }
      return ((), ars')
    EffShakeCamera ampl duration -> do
      change acs { acsCamera = setCameraShake ampl duration (acsCamera acs) }
  where
    set :: AreaCommonState -> s
    set acs' = arsSetCommon ars acs'
    change :: AreaCommonState -> IO ((), s)
    change acs' = return ((), set acs')

-------------------------------------------------------------------------------
