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
   getAreaTriggers, getMonsterScript, getRegionBackground, getScriptedBattle,
   -- * Defining the scenario
   CompileScenario, compileScenario, newGlobalVar, compileRegion, compileArea,
   -- * Defining an area
   CompileArea, newPersistentVar, newTransientVar, makeExit,
   -- * Scripted battles
   CompileBattle, ScriptedBattle(..), newScriptedBattle,
   -- * Defining triggers
   DefineTrigger(..), onStartDaily, onStartOnce, daily, once, onBattleStart,
   -- * Devices
   DefineDevice(..),
   -- * Monster scripts
   DefineMonsterScript(..),
   -- * Variables
   Var, getVar, readVar, writeVar, modifyVar,
   -- * Trigger predicates
   Predicate, periodicP, andP, orP, xorP, notP, getP, whenP,
   varIs, varTrue, varFalse, varEq, varNeq,
   walkOn, walkOff, walkIn,
   questUntaken, questActive, areaCleared,
   monsterReady)
where

import Control.Applicative (Applicative)
import Control.Monad (when)
import Control.Monad.Fix (MonadFix)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Fallback.Constants (maxActionPoints, momentsPerActionPoint)
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point (rectContains)
import qualified Fallback.Data.SparseMap as SM
import qualified Fallback.Data.TotalMap as TM
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Combat (CombatState, csPeriodicTimer)
import Fallback.State.Party (Party, partyClearedAreas, partyQuests)
import Fallback.State.Progress
import Fallback.State.Simple
import Fallback.State.Tags (AreaTag, QuestTag, RegionTag)
import Fallback.State.Terrain (terrainMap, tmapLookupMark, tmapLookupRect)
import Fallback.State.Town (TownState)
import Fallback.State.Trigger (Trigger, makeTrigger)

-------------------------------------------------------------------------------
-- Reading the scenario:

data ScenarioTriggers = ScenarioTriggers
  { scenarioAreas :: TM.TotalMap AreaTag AreaSpec,
    scenarioInitialProgress :: Progress,
    scenarioRegionBackgrounds :: TM.TotalMap RegionTag (Party -> String) }

data AreaSpec = AreaSpec
  { aspecDevices :: Map.Map DeviceId Device,
    aspecEntrances :: Map.Map AreaTag String,
    aspecExits :: [AreaExit],
    aspecMonsterScripts :: Map.Map MonsterScriptId MonsterScript,
    aspecScriptedBattles :: Map.Map BattleId ScriptedBattle,
    aspecTerrain :: Party -> String,
    aspecTriggers :: [Trigger TownState TownEffect] }

getAreaDevice :: ScenarioTriggers -> AreaTag -> DeviceId -> Maybe Device
getAreaDevice scenario tag di =
  Map.lookup di $ aspecDevices $ TM.get tag $ scenarioAreas scenario

getAreaEntrance :: ScenarioTriggers -> AreaTag -> AreaTag -> String
getAreaEntrance scenario tag from =
  Map.findWithDefault err from $ aspecEntrances $ TM.get tag $
  scenarioAreas scenario
  where err = error ("getAreaEntrance: no entrance to " ++ show tag ++
                     " from " ++ show from)

getAreaExits :: ScenarioTriggers -> AreaTag -> [AreaExit]
getAreaExits scenario tag = aspecExits $ TM.get tag $ scenarioAreas scenario

getAreaLinks :: ScenarioTriggers -> AreaTag -> Set.Set AreaTag
getAreaLinks scenario tag =
  Map.keysSet $ aspecEntrances $ TM.get tag $ scenarioAreas scenario

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

getScriptedBattle :: ScenarioTriggers -> AreaTag -> BattleId -> ScriptedBattle
getScriptedBattle scenario areaTag battleId =
  fromMaybe (error ("getScriptedBattle: no such battle: " ++ show battleId)) $
  Map.lookup battleId $ aspecScriptedBattles $ TM.get areaTag $
  scenarioAreas scenario

-------------------------------------------------------------------------------
-- Defining the scenario:

newtype CompileScenario a =
  CompileScenario (State.State CompileScenarioState a)
  deriving (Applicative, Functor, Monad, MonadFix)

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
                casProgress = cssProgress css,
                casScriptedBattles = Map.empty,
                casTriggers = [] }
  let mkExit (dest, (rectKeys, _)) =
        AreaExit { aeDestination = dest, aeRectKeys = rectKeys }
  let aspec = AreaSpec { aspecDevices = casDevices cas,
                         aspecEntrances = fmap snd $ casEntrances cas,
                         aspecExits = map mkExit $ Map.assocs $
                                      casEntrances cas,
                         aspecMonsterScripts = casMonsterScripts cas,
                         aspecScriptedBattles = casScriptedBattles cas,
                         aspecTerrain = fromMaybe (const $ show tag) mbTerraFn,
                         aspecTriggers = reverse (casTriggers cas) }
  State.put css { cssAllVarSeeds = casAllVarSeeds cas,
                  cssAreas = Map.insert tag aspec (cssAreas css),
                  cssProgress = casProgress cas }

-------------------------------------------------------------------------------
-- Defining an area:

newtype CompileArea a = CompileArea (State.State CompileAreaState a)
  deriving (Applicative, Functor, Monad, MonadFix)

data CompileAreaState = CompileAreaState
  { casAllVarSeeds :: Set.Set VarSeed,
    casDevices :: Map.Map DeviceId Device,
    casEntrances :: Map.Map AreaTag ([String], String),
    casMonsterScripts :: Map.Map MonsterScriptId MonsterScript,
    casProgress :: Progress,
    casScriptedBattles :: Map.Map BattleId ScriptedBattle,
    casTriggers :: [Trigger TownState TownEffect] }

newPersistentVar :: (VarType a) => VarSeed -> a -> CompileArea (Var a)
newPersistentVar vseed value = do
  var <- newVar vseed
  CompileArea $ do
    cas <- State.get
    State.put cas { casProgress = progressSetVar var value $ casProgress cas }
    return var

newTransientVar :: (VarType a) => VarSeed -> Script TownEffect a
                -> CompileArea (Var a)
newTransientVar vseed initialize = do
  (vseed', vseed'') <- splitVarSeed vseed
  var <- newPersistentVar vseed' varDefaultValue
  onStartDaily vseed'' $ do
    writeVar var =<< initialize
  return var

makeExit :: AreaTag -> [String] -> String -> CompileArea ()
makeExit tag rectKeys markKey = CompileArea $ do
  cas <- State.get
  let entrances = casEntrances cas
  when (Map.member tag entrances) $ do
    fail ("Repeated exit: " ++ show tag)
  State.put cas { casEntrances = Map.insert tag (rectKeys, markKey) entrances }

-------------------------------------------------------------------------------

newtype CompileBattle a = CompileBattle (State.State CompileBattleState a)
  deriving (Applicative, Functor, Monad, MonadFix)

data CompileBattleState = CompileBattleState
  { cbsAllVarSeeds :: Set.Set VarSeed,
    cbsTriggers :: [Trigger CombatState CombatEffect] }

data ScriptedBattle = ScriptedBattle
  { sbId :: BattleId,
    sbRectKey :: String,
    sbTriggers :: [Trigger CombatState CombatEffect] }

newScriptedBattle :: VarSeed -> String -> CompileBattle ()
                  -> CompileArea BattleId
newScriptedBattle vseed rectKey (CompileBattle compile) = do
  battleId <- newBattleId vseed
  CompileArea $ do
    cas <- State.get
    let cbs = State.execState compile CompileBattleState
                { cbsAllVarSeeds = casAllVarSeeds cas,
                  cbsTriggers = [] }
    let battle = ScriptedBattle { sbId = battleId, sbRectKey = rectKey,
                                  sbTriggers = cbsTriggers cbs }
    State.put cas { casAllVarSeeds = cbsAllVarSeeds cbs,
                    casScriptedBattles = Map.insert battleId battle $
                                         casScriptedBattles cas }
    return battleId

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

instance HasVarSeeds CompileBattle where
  useVarSeed vseed = CompileBattle $ do
    varSeeds <- State.gets cbsAllVarSeeds
    when (Set.member vseed varSeeds) $ do
      fail ("Repeated VarSeed: " ++ show vseed)
    State.modify $ \cbs -> cbs { cbsAllVarSeeds = Set.insert vseed varSeeds }

-------------------------------------------------------------------------------
-- Defining triggers:

class (HasVarSeeds m) => DefineTrigger m where
  type TriggerState m :: *
  type TriggerEffect m :: * -> *
  trigger :: VarSeed -> Predicate (TriggerState m)
          -> Script (TriggerEffect m) () -> m ()

instance DefineTrigger CompileArea where
  type TriggerState CompileArea = TownState
  type TriggerEffect CompileArea = TownEffect
  trigger vseed (Predicate predicate) action = do
    tid <- newTriggerId vseed
    CompileArea $ do
      cas <- State.get
      State.put cas { casTriggers = makeTrigger tid predicate action :
                                    casTriggers cas }

instance DefineTrigger CompileBattle where
  type TriggerState CompileBattle = CombatState
  type TriggerEffect CompileBattle = CombatEffect
  trigger vseed (Predicate predicate) action = do
    tid <- newTriggerId vseed
    CompileBattle $ do
      cbs <- State.get
      State.put cbs { cbsTriggers = makeTrigger tid predicate action :
                                    cbsTriggers cbs }

onStartDaily :: VarSeed -> Script TownEffect () -> CompileArea ()
onStartDaily vseed = trigger vseed alwaysP

onStartOnce :: VarSeed -> Script TownEffect () -> CompileArea ()
onStartOnce vseed = once vseed alwaysP

daily :: VarSeed -> Predicate TownState -> Script TownEffect ()
      -> CompileArea ()
daily vseed predicate script = do
  (vseed', vseed'') <- splitVarSeed vseed
  canFire <- newTransientVar vseed' (return True)
  trigger vseed'' (varTrue canFire `andP` predicate) $ do
    writeVar canFire False >> script

once :: VarSeed -> Predicate TownState -> Script TownEffect ()
     -> CompileArea ()
once vseed predicate script = do
  (vseed', vseed'') <- splitVarSeed vseed
  canFire <- newPersistentVar vseed' True
  trigger vseed'' (varTrue canFire `andP` predicate) $ do
    writeVar canFire False >> script

onBattleStart :: VarSeed -> Script CombatEffect () -> CompileBattle ()
onBattleStart vseed = trigger vseed alwaysP

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

newtype Predicate s = Predicate (s -> Bool)

-- | A predicate that is always true.
alwaysP :: Predicate s
alwaysP = Predicate (const True)

-- | A predicate that is true just after each periodic combat tick (once per
-- round).
periodicP :: Predicate CombatState
periodicP = Predicate ((0 ==) . csPeriodicTimer)

infixr 3 `andP`
andP :: Predicate s -> Predicate s -> Predicate s
andP (Predicate fn1) (Predicate fn2) = Predicate (\s -> fn1 s && fn2 s)

infixr 2 `orP`
orP :: Predicate s -> Predicate s -> Predicate s
orP (Predicate fn1) (Predicate fn2) = Predicate (\s -> fn1 s || fn2 s)

infixr 2 `xorP`
xorP :: Predicate s -> Predicate s -> Predicate s
xorP (Predicate fn1) (Predicate fn2) = Predicate (\s -> fn1 s /= fn2 s)

notP :: Predicate s -> Predicate s
notP (Predicate fn) = Predicate (not . fn)

-- | Evaluate a predicate from within a script.
getP :: (FromAreaEffect f) => (forall s. (AreaState s) => Predicate s)
     -> Script f Bool
getP predicate = areaGet (case predicate of Predicate fn -> fn)

whenP :: (FromAreaEffect f) => (forall s. (AreaState s) => Predicate s)
      -> Script f () -> Script f ()
whenP predicate action = do
  bool <- getP predicate
  when bool action

varIs :: (VarType a, HasProgress s) => (a -> Bool) -> Var a -> Predicate s
varIs fn var = Predicate (fn . getVar var)

varTrue :: (HasProgress s) => Var Bool -> Predicate s
varTrue var = Predicate (getVar var)

varFalse :: (HasProgress s) => Var Bool -> Predicate s
varFalse var = Predicate (not . getVar var)

varEq :: (Eq a, VarType a, HasProgress s) => Var a -> a -> Predicate s
varEq var value = Predicate ((value ==) . getVar var)

varNeq :: (Eq a, VarType a, HasProgress s) => Var a -> a -> Predicate s
varNeq var value = Predicate ((value /=) . getVar var)

walkOn :: (AreaState s) => String -> Predicate s
walkOn key = Predicate $ \s ->
  any (flip Set.member (tmapLookupMark key $ terrainMap $ arsTerrain s)) $
  arsPartyPositions s

walkOff :: (AreaState s) => String -> Predicate s
walkOff = notP . walkOn

-- | True if the specified terrain rect exists and at least one character is
-- currently within it.
walkIn :: (AreaState s) => String -> Predicate s
walkIn key = Predicate $ \s ->
  maybe False (\r -> any (rectContains r) (arsPartyPositions s)) $
  tmapLookupRect key $ terrainMap $ arsTerrain s

questUntaken :: (AreaState s) => QuestTag -> Predicate s
questUntaken = questStatus (== QuestUntaken)

questActive :: (AreaState s) => QuestTag -> Predicate s
questActive = questStatus (== QuestActive)

questStatus :: (AreaState s) => (QuestStatus -> Bool) -> QuestTag
            -> Predicate s
questStatus fn tag = Predicate (fn . SM.get tag . partyQuests . arsParty)

-- | A predicate that is true when the given area has been cleared.
areaCleared :: (AreaState s) => AreaTag -> Predicate s
areaCleared tag = Predicate (Set.member tag . partyClearedAreas . arsParty)

monsterReady :: Var (Grid.Key Monster) -> Predicate CombatState
monsterReady var = Predicate $ \cs ->
  -- TODO: Right now, monster moment-gain and turn-taking are atomic, so this
  --   won't really work.  We need to change it so trigger testing happens in
  --   between increasing monster moments and checking if monsters are ready.
  maybe False ((maxActionPoints * momentsPerActionPoint <=) . monstMoments .
               Grid.geValue) $ Grid.lookup (getVar var cs) (arsMonsters cs)

-------------------------------------------------------------------------------
