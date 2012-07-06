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

{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types, TypeFamilies #-}

module Fallback.Scenario.Compile
  (-- * Reading the scenario
   ScenarioTriggers, scenarioInitialProgress,
   getAreaDevice, getAreaEntrance, getAreaExits, getAreaLinks, getAreaTerrain,
   getAreaTriggers, getMonsterScript, getRegionBackground,
   -- * Defining the scenario
   CompileScenario, compileScenario, newGlobalVar, compileRegion, compileArea,
   -- * Defining an area
   CompileArea, newPersistentVar, newTransientVar, makeExit, simpleMonster,
   simpleTownsperson,
   -- * Defining triggers
   DefineTrigger(..), onStartDaily, onStartOnce, daily, once,
   -- * Devices
   DefineDevice(..),
   -- * Variables
   Var, getVar, readVar, writeVar, modifyVar,
   -- * Trigger predicates
   Predicate, andP, orP, xorP, notP, getP, whenP,
   varTrue, varFalse, varEq, varNeq,
   walkOn, walkOff, walkIn,
   questUntaken, questActive)
where

import Control.Monad (unless, void, when)
import Control.Monad.Fix (MonadFix)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import qualified Fallback.Data.Grid as Grid (Entry)
import Fallback.Data.Point (Position, PRect, pZero, rectContains)
import qualified Fallback.Data.SparseMap as SM
import qualified Fallback.Data.TotalMap as TM
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Party (Party, partyQuests)
import Fallback.State.Progress
import Fallback.State.Simple
import Fallback.State.Tags (AreaTag, MonsterTag, QuestTag, RegionTag)
import Fallback.State.Town (TownState)

-------------------------------------------------------------------------------
-- Reading the scenario:

data ScenarioTriggers = ScenarioTriggers
  { scenarioAreas :: TM.TotalMap AreaTag AreaSpec,
    scenarioInitialProgress :: Progress,
    scenarioRegionBackgrounds :: TM.TotalMap RegionTag (Party -> String) }

data AreaSpec = AreaSpec
  { aspecDevices :: Map.Map DeviceId Device,
    aspecEntrances :: Map.Map AreaTag Position,
    aspecExits :: [AreaExit],
    aspecMonsterScripts :: Map.Map MonsterScriptId MonsterScript,
    aspecTerrain :: Party -> String,
    aspecTriggers :: [Trigger TownState TownEffect] }

getAreaDevice :: ScenarioTriggers -> AreaTag -> DeviceId -> Maybe Device
getAreaDevice scenario tag di =
  Map.lookup di $ aspecDevices $ TM.get tag $ scenarioAreas scenario

getAreaEntrance :: ScenarioTriggers -> AreaTag -> AreaTag -> Position
getAreaEntrance scenario tag from =
  Map.findWithDefault pZero from $ aspecEntrances $ TM.get tag $
  scenarioAreas scenario

getAreaExits :: ScenarioTriggers -> AreaTag -> [AreaExit]
getAreaExits scenario tag = aspecExits $ TM.get tag $ scenarioAreas scenario

getAreaLinks :: ScenarioTriggers -> AreaTag -> [AreaTag]
getAreaLinks scenario tag =
  Map.keys $ aspecEntrances $ TM.get tag $ scenarioAreas scenario

getAreaTerrain :: ScenarioTriggers -> Party -> AreaTag -> String
getAreaTerrain scenario party tag =
  aspecTerrain (TM.get tag (scenarioAreas scenario)) party

getAreaTriggers :: ScenarioTriggers -> AreaTag
                -> [Trigger TownState TownEffect]
getAreaTriggers scenario tag =
  aspecTriggers $ TM.get tag $ scenarioAreas scenario

getMonsterScript :: ScenarioTriggers -> AreaTag -> MonsterScriptId
                 -> Grid.Entry Monster -> Script TownEffect ()
getMonsterScript scenario tag scriptId =
  fromMaybe (fail ("no such monster script: " ++ show scriptId)) $
  Map.lookup scriptId $ aspecMonsterScripts $ TM.get tag $
  scenarioAreas scenario

getRegionBackground :: ScenarioTriggers -> Party -> RegionTag -> String
getRegionBackground scenario party tag =
  TM.get tag (scenarioRegionBackgrounds scenario) party

-------------------------------------------------------------------------------
-- Defining the scenario:

newtype CompileScenario a =
  CompileScenario (State.State CompileScenarioState a)
  deriving (Functor, Monad, MonadFix)

data CompileScenarioState = CompileScenarioState
  { cssAllVarSeeds :: Set.Set VarSeed,
    cssAreas :: Map.Map AreaTag AreaSpec,
    cssDevices :: Map.Map DeviceId Device,
    cssMonsterScripts :: Map.Map MonsterScriptId MonsterScript,
    cssProgress :: Progress,
    cssRegions :: Map.Map RegionTag (Party -> String) }

compileScenario :: CompileScenario () -> ScenarioTriggers
compileScenario (CompileScenario compile) =
  let css = State.execState compile CompileScenarioState
              { cssAllVarSeeds = Set.empty, cssAreas = Map.empty,
                cssDevices = Map.empty, cssMonsterScripts = Map.empty,
                cssProgress = emptyProgress, cssRegions = Map.empty }
      getArea tag = fromMaybe (error $ "Missing area: " ++ show tag) $
                    Map.lookup tag $ cssAreas css
                    -- TODO Verify symmetry of links
      getRegion tag = fromMaybe (error $ "Missing region: " ++ show tag) $
                      Map.lookup tag $ cssRegions css
  in ScenarioTriggers { scenarioAreas = TM.make getArea,
                        scenarioRegionBackgrounds = TM.make getRegion,
                        scenarioInitialProgress = cssProgress css }

newGlobalVar :: (VarType a) => VarSeed -> a -> CompileScenario (Var a)
newGlobalVar vseed value = do
  var <- newVar vseed
  CompileScenario $ do
    css <- State.get
    State.put css { cssProgress = progressSetVar var value $ cssProgress css }
    return var

compileRegion :: RegionTag -> (Party -> String) -> CompileScenario ()
compileRegion tag backgroundFn = CompileScenario $ do
  css <- State.get
  when (Map.member tag $ cssRegions css) $ do
    fail ("Repeated region: " ++ show tag)
  State.put css { cssRegions = Map.insert tag backgroundFn (cssRegions css) }

compileArea :: AreaTag -> Maybe (Party -> String) -> CompileArea ()
            -> CompileScenario ()
compileArea tag mbTerraFn (CompileArea compile) = CompileScenario $ do
  css <- State.get
  when (Map.member tag $ cssAreas css) $ do
    fail ("Repeated area: " ++ show tag)
  let cas = State.execState compile CompileAreaState
              { casAllVarSeeds = cssAllVarSeeds css,
                casDevices = cssDevices css, casEntrances = Map.empty,
                casMonsterScripts = cssMonsterScripts css,
                casProgress = cssProgress css, casTriggers = [] }
  let mkExit (dest, (prects, _)) =
        AreaExit { aeDestination = dest, aeRects = prects }
  let aspec = AreaSpec { aspecDevices = casDevices cas,
                         aspecEntrances = fmap snd $ casEntrances cas,
                         aspecExits = map mkExit $ Map.assocs $
                                      casEntrances cas,
                         aspecMonsterScripts = casMonsterScripts cas,
                         aspecTerrain = fromMaybe (const $ show tag) mbTerraFn,
                         aspecTriggers = casTriggers cas }
  State.put css { cssAllVarSeeds = casAllVarSeeds cas,
                  cssAreas = Map.insert tag aspec (cssAreas css),
                  cssProgress = casProgress cas }

-------------------------------------------------------------------------------
-- Defining an area:

newtype CompileArea a = CompileArea (State.State CompileAreaState a)
  deriving (Functor, Monad, MonadFix)

data CompileAreaState = CompileAreaState
  { casAllVarSeeds :: Set.Set VarSeed,
    casDevices :: Map.Map DeviceId Device,
    casEntrances :: Map.Map AreaTag ([PRect], Position),
    casMonsterScripts :: Map.Map MonsterScriptId MonsterScript,
    casProgress :: Progress,
    casTriggers :: [Trigger TownState TownEffect] }

newPersistentVar :: (VarType a) => VarSeed -> a -> CompileArea (Var a)
newPersistentVar vseed value = do
  var <- newVar vseed
  CompileArea $ do
    cas <- State.get
    State.put cas { casProgress = progressSetVar var value $ casProgress cas }
    return var

newTransientVar :: (VarType a) => VarSeed -> a -> CompileArea (Var a)
newTransientVar vseed value = do
  (vseed', vseed'') <- splitVarSeed vseed
  var <- newPersistentVar vseed' value
  onStartDaily vseed'' $ writeVar var value
  return var

makeExit :: AreaTag -> [PRect] -> Position -> CompileArea ()
makeExit tag rects pos = CompileArea $ do
  cas <- State.get
  let entrances = casEntrances cas
  when (Map.member tag entrances) $ do
    fail ("Repeated exit: " ++ show tag)
  State.put cas { casEntrances = Map.insert tag (rects, pos) entrances }

simpleMonster :: VarSeed -> MonsterTag -> Position -> MonsterTownAI
              -> CompileArea ()
simpleMonster vseed tag pos ai = do
  (vseed', vseed'') <- splitVarSeed vseed
  isDeadVar <- newPersistentVar vseed' False
  onStartDaily vseed'' $ do
    isDead <- readVar isDeadVar
    unless isDead $ do
      addBasicEnemyMonster pos tag (Just isDeadVar) ai

simpleTownsperson :: VarSeed -> MonsterTag -> Position -> MonsterTownAI
                  -> (Grid.Entry Monster -> Script TownEffect ())
                  -> CompileArea ()
simpleTownsperson vseed tag pos ai sfn = do
  (vseed', vseed'') <- splitVarSeed vseed
  scriptId <- newMonsterScript vseed' sfn
  onStartDaily vseed'' $ do
    void $ tryAddMonster pos (makeMonster tag)
      { monstIsAlly = True,
        monstScript = Just scriptId,
        monstTownAI = ai }

-------------------------------------------------------------------------------
-- Checking VarSeeds:

instance HasVarSeeds CompileScenario where
  useVarSeed vseed = CompileScenario $ do
    varSeeds <- State.gets cssAllVarSeeds
    when (Set.member vseed varSeeds) $ do
      fail ("Repeated VarSeed: " ++ show vseed)
    State.modify $ \css -> css { cssAllVarSeeds = Set.insert vseed varSeeds }

instance HasVarSeeds CompileArea where
  useVarSeed vseed = CompileArea $ do
    varSeeds <- State.gets casAllVarSeeds
    when (Set.member vseed varSeeds) $ do
      fail ("Repeated VarSeed: " ++ show vseed)
    State.modify $ \cas -> cas { casAllVarSeeds = Set.insert vseed varSeeds }

-------------------------------------------------------------------------------
-- Defining triggers:

class (HasVarSeeds m) => DefineTrigger m where
  type TriggerState m :: *
  type TriggerEffect m :: * -> *
  trigger :: VarSeed -> Predicate
          -> Script (TriggerEffect m) () -> m ()

instance DefineTrigger CompileArea where
  type TriggerState CompileArea = TownState
  type TriggerEffect CompileArea = TownEffect
  trigger vseed (Predicate predicate) action = do
    tid <- newTriggerId vseed
    CompileArea $ do
      cas <- State.get
      let trig = Trigger { triggerId = tid,
                           triggerPredicate = predicate,
                           triggerAction = action }
      State.put cas { casTriggers = trig : casTriggers cas }

onStartDaily :: VarSeed -> Script TownEffect () -> CompileArea ()
onStartDaily vseed = trigger vseed (Predicate $ const True)

onStartOnce :: VarSeed -> Script TownEffect () -> CompileArea ()
onStartOnce vseed = once vseed (Predicate $ const True)

daily :: VarSeed -> Predicate -> Script TownEffect () -> CompileArea ()
daily vseed predicate script = do
  (vseed', vseed'') <- splitVarSeed vseed
  canFire <- newTransientVar vseed' True
  trigger vseed'' (varTrue canFire `andP` predicate) $ do
    writeVar canFire False >> script

once :: VarSeed -> Predicate -> Script TownEffect () -> CompileArea ()
once vseed predicate script = do
  (vseed', vseed'') <- splitVarSeed vseed
  canFire <- newPersistentVar vseed' True
  trigger vseed'' (varTrue canFire `andP` predicate) $ do
    writeVar canFire False >> script

-------------------------------------------------------------------------------
-- Defining devices:

class (HasVarSeeds m) => DefineDevice m where
  newDevice :: VarSeed -> Int
            -> (Grid.Entry Device -> CharacterNumber -> Script AreaEffect ())
            -> m Device

instance DefineDevice CompileScenario where
  newDevice vseed radius sfn = do
    di <- newDeviceId vseed
    CompileScenario $ do
      css <- State.get
      when (Map.member di (cssDevices css)) $ do
        fail ("Internal error: Repeated device ID: " ++ show di)
      let device = Device { devId = di, devInteract = sfn, devRadius = radius }
      State.put css { cssDevices = Map.insert di device (cssDevices css) }
      return device

instance DefineDevice CompileArea where
  newDevice vseed radius sfn = do
    di <- newDeviceId vseed
    CompileArea $ do
      cas <- State.get
      when (Map.member di $ casDevices cas) $ do
        fail ("Internal error: Repeated device ID: " ++ show di)
      let device = Device { devId = di, devInteract = sfn, devRadius = radius }
      State.put cas { casDevices = Map.insert di device (casDevices cas) }
      return device

-------------------------------------------------------------------------------
-- Defining monster scripts:

type MonsterScript = Grid.Entry Monster -> Script TownEffect ()

class (HasVarSeeds m) => DefineMonsterScript m where
  newMonsterScript :: VarSeed -> MonsterScript -> m MonsterScriptId

instance DefineMonsterScript CompileArea where
  newMonsterScript vseed sfn = do
    msi <- newMonsterScriptId vseed
    CompileArea $ do
      cas <- State.get
      State.put cas { casMonsterScripts = Map.insert msi sfn
                                                     (casMonsterScripts cas) }
      return msi

-------------------------------------------------------------------------------
-- Variables:

getVar :: (VarType a, HasProgress s) => Var a -> s -> a
getVar var = progressGetVar var . getProgress

readVar :: (VarType a, FromAreaEffect f) => Var a -> Script f a
readVar var = areaGet $ getVar var

writeVar :: (VarType a, FromAreaEffect f) => Var a -> a -> Script f ()
writeVar var value = emitAreaEffect $ EffAreaParty $ EffSetVar var value

modifyVar :: (VarType a, FromAreaEffect f) => Var a -> (a -> a)
          -> Script f ()
modifyVar var fn = do
  value <- readVar var
  writeVar var (fn value)

-------------------------------------------------------------------------------
-- Trigger predicates:

data Predicate = Predicate (forall s. (AreaState s) => s -> Bool)

infixr 3 `andP`
andP :: Predicate -> Predicate -> Predicate
andP (Predicate fn1) (Predicate fn2) = Predicate (\s -> fn1 s && fn2 s)

infixr 2 `orP`
orP :: Predicate -> Predicate -> Predicate
orP (Predicate fn1) (Predicate fn2) = Predicate (\s -> fn1 s || fn2 s)

infixr 2 `xorP`
xorP :: Predicate -> Predicate -> Predicate
xorP (Predicate fn1) (Predicate fn2) = Predicate (\s -> fn1 s /= fn2 s)

notP :: Predicate -> Predicate
notP (Predicate fn) = Predicate (not . fn)

getP :: (FromAreaEffect f) => Predicate -> Script f Bool
getP (Predicate fn) = areaGet fn

whenP :: (FromAreaEffect f) => Predicate -> Script f () -> Script f ()
whenP predicate action = do
  bool <- getP predicate
  when bool action

varTrue :: Var Bool -> Predicate
varTrue var = Predicate (getVar var)

varFalse :: Var Bool -> Predicate
varFalse var = Predicate (not . getVar var)

varEq :: (Eq a, VarType a) => Var a -> a -> Predicate
varEq var value = Predicate ((value ==) . getVar var)

varNeq :: (Eq a, VarType a) => Var a -> a -> Predicate
varNeq var value = Predicate ((value /=) . getVar var)

walkOn :: Position -> Predicate
walkOn pos = Predicate (\s -> any (pos ==) (arsPartyPositions s))

walkOff :: Position -> Predicate
walkOff = notP . walkOn

walkIn :: PRect -> Predicate
walkIn rect = Predicate (\s -> any (rectContains rect) (arsPartyPositions s))

questUntaken :: QuestTag -> Predicate
questUntaken = questStatus (== QuestUntaken)

questActive :: QuestTag -> Predicate
questActive = questStatus (== QuestActive)

questStatus :: (QuestStatus -> Bool) -> QuestTag -> Predicate
questStatus fn tag = Predicate (fn . SM.get tag . partyQuests . arsParty)

-------------------------------------------------------------------------------
