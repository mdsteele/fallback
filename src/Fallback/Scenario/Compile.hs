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
   getAreaDevice, getAreaEntrance, getAreaLinks, getAreaTerrain,
   getAreaTriggers, getRegionBackground,
   -- * Defining the scenario
   CompileScenario, compileScenario, newGlobalVar, compileRegion, compileArea,
   -- * Defining an area
   CompileArea, newPersistentVar, newTransientVar, makeExit, simpleMonster,
   simpleTownsperson,
   -- * Defining triggers
   DefineTrigger(..), onStartDaily, onStartOnce, daily, once,
   -- * Devices
   DefineDevice(..), uniqueDevice,
   -- * Variables
   Var, getVar, readVar, writeVar, modifyVar,
   -- * Trigger predicates
   Predicate, andP, orP, notP, getP,
   varTrue, varFalse,
   walkOn, walkOff, walkIn)
where

import Control.Applicative ((<$))
import Control.Monad (unless, when)
import Control.Monad.Fix (MonadFix)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import qualified Fallback.Data.Grid as Grid (Entry)
import Fallback.Data.Point (Position, PRect, pZero, rectContains)
import Fallback.Data.TotalMap
import Fallback.Scenario.Monsters (getMonsterType)
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.Party (Party)
import Fallback.State.Progress
import Fallback.State.Simple
import Fallback.State.Status (initStatusEffects)
import Fallback.State.Tags (AreaTag, MonsterTag, RegionTag)
import Fallback.State.Town (TownState)

-------------------------------------------------------------------------------
-- Reading the scenario:

data ScenarioTriggers = ScenarioTriggers
  { scenarioAreas :: TotalMap AreaTag AreaSpec,
    scenarioInitialProgress :: Progress,
    scenarioRegionBackgrounds :: TotalMap RegionTag (Party -> String) }

data AreaSpec = AreaSpec
  { aspecDevices :: Map.Map DeviceId Device,
    aspecEntrances :: Map.Map AreaTag Position,
    aspecMonsterScripts :: Map.Map MonsterScriptId MonsterScript,
    aspecTerrain :: Party -> String,
    aspecTriggers :: [Trigger TownState TownEffect] }

getAreaDevice :: ScenarioTriggers -> AreaTag -> DeviceId -> Maybe Device
getAreaDevice scenario tag di =
  Map.lookup di $ aspecDevices $ tmGet tag $ scenarioAreas scenario

getAreaEntrance :: ScenarioTriggers -> AreaTag -> AreaTag -> Position
getAreaEntrance scenario tag from =
  Map.findWithDefault pZero from $ aspecEntrances $ tmGet tag $
  scenarioAreas scenario

getAreaLinks :: ScenarioTriggers -> AreaTag -> [AreaTag]
getAreaLinks scenario tag =
  Map.keys $ aspecEntrances $ tmGet tag $ scenarioAreas scenario

getAreaTerrain :: ScenarioTriggers -> Party -> AreaTag -> String
getAreaTerrain scenario party tag =
  aspecTerrain (tmGet tag (scenarioAreas scenario)) party

getAreaTriggers :: ScenarioTriggers -> AreaTag
                -> [Trigger TownState TownEffect]
getAreaTriggers scenario tag =
  aspecTriggers $ tmGet tag $ scenarioAreas scenario

getRegionBackground :: ScenarioTriggers -> Party -> RegionTag -> String
getRegionBackground scenario party tag =
  tmGet tag (scenarioRegionBackgrounds scenario) party

-------------------------------------------------------------------------------
-- Defining the scenario:

newtype CompileScenario a =
  CompileScenario (State.State CompileScenarioState a)
  deriving (Functor, Monad, MonadFix)

data CompileScenarioState = CompileScenarioState
  { cssAreas :: Map.Map AreaTag AreaSpec,
    cssDeviceIds :: Set.Set DeviceId,
    cssDevices :: Map.Map DeviceId Device,
    cssMonsterScriptIds :: Set.Set MonsterScriptId,
    cssMonsterScripts :: Map.Map MonsterScriptId MonsterScript,
    cssProgress :: Progress,
    cssRegions :: Map.Map RegionTag (Party -> String) }

compileScenario :: CompileScenario () -> ScenarioTriggers
compileScenario (CompileScenario compile) =
  let css = State.execState compile CompileScenarioState
              { cssAreas = Map.empty,
                cssDeviceIds = Set.empty, cssDevices = Map.empty,
                cssMonsterScriptIds = Set.empty, cssMonsterScripts = Map.empty,
                cssProgress = emptyProgress, cssRegions = Map.empty }
      getArea tag = fromMaybe (error $ "Missing area: " ++ show tag) $
                    Map.lookup tag $ cssAreas css
                    -- TODO Verify symmetry of links
      getRegion tag = fromMaybe (error $ "Missing region: " ++ show tag) $
                      Map.lookup tag $ cssRegions css
  in ScenarioTriggers { scenarioAreas = makeTotalMap getArea,
                        scenarioRegionBackgrounds = makeTotalMap getRegion,
                        scenarioInitialProgress = cssProgress css }

newGlobalVar :: (VarType a) => VarSeed -> a -> CompileScenario (Var a)
newGlobalVar vseed value = CompileScenario $ do
  css <- State.get
  let var = makeVar vseed
  maybe (fail $ "Repeated Var: " ++ show var)
        (\pu -> State.put css { cssProgress = pu })
        (progressAddVar var value $ cssProgress css)
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
              { casDeviceIds = cssDeviceIds css, casDevices = cssDevices css,
                casEntrances = Map.empty,
                casMonsterScripts = cssMonsterScripts css,
                casMonsterScriptIds = cssMonsterScriptIds css,
                casProgress = cssProgress css, casTriggers = [] }
  unless (Map.keysSet (casDevices cas) `Set.isSubsetOf` casDeviceIds cas) $ do
    fail ("Internal error: devices is not subset of device IDs: " ++ show tag)
  let aspec = AreaSpec { aspecDevices = casDevices cas,
                         aspecEntrances = casEntrances cas,
                         aspecMonsterScripts = casMonsterScripts cas,
                         aspecTerrain = fromMaybe (const $ show tag) mbTerraFn,
                         aspecTriggers = casTriggers cas }
  State.put css { cssAreas = Map.insert tag aspec (cssAreas css),
                  cssDeviceIds = casDeviceIds cas,
                  cssMonsterScriptIds = casMonsterScriptIds cas,
                  cssProgress = casProgress cas }

-------------------------------------------------------------------------------
-- Defining an area:

newtype CompileArea a = CompileArea (State.State CompileAreaState a)
  deriving (Functor, Monad, MonadFix)

data CompileAreaState = CompileAreaState
  { casDeviceIds :: Set.Set DeviceId,
    casDevices :: Map.Map DeviceId Device,
    casEntrances :: Map.Map AreaTag Position,
    casMonsterScriptIds :: Set.Set MonsterScriptId,
    casMonsterScripts :: Map.Map MonsterScriptId MonsterScript,
    casProgress :: Progress,
    casTriggers :: [Trigger TownState TownEffect] }

newPersistentVar :: (VarType a) => VarSeed -> a -> CompileArea (Var a)
newPersistentVar vseed value = CompileArea $ do
  cas <- State.get
  let var = makeVar vseed
  maybe (fail $ "Repeated Var: " ++ show var)
        (\pu -> State.put cas { casProgress = pu })
        (progressAddVar var value $ casProgress cas)
  return var

newTransientVar :: (VarType a) => VarSeed -> a -> CompileArea (Var a)
newTransientVar vseed value = do
  let (vseed', vseed'') = splitVarSeed vseed
  var <- newPersistentVar vseed' value
  onStartDaily vseed'' $ writeVar var value
  return var

makeExit :: VarSeed -> AreaTag -> PRect -> Position -> CompileArea ()
makeExit vseed tag rect pos =
  trigger vseed (walkIn rect) (exitTo tag) >> entrance where
    entrance = CompileArea $ do
      cas <- State.get
      when (Map.member tag $ casEntrances cas) $ do
        fail ("Repeated exit: " ++ show tag)
      State.put cas { casEntrances = Map.insert tag pos (casEntrances cas) }

simpleMonster :: VarSeed -> MonsterTag -> Position -> MonsterTownAI
              -> CompileArea ()
simpleMonster vseed tag pos ai = do
  let (vseed', vseed'') = splitVarSeed vseed
  isDeadVar <- newPersistentVar vseed' False
  onStartDaily vseed'' $ do
    isDead <- readVar isDeadVar
    unless isDead $ do
      addBasicEnemyMonster pos tag (Just isDeadVar) ai

simpleTownsperson :: VarSeed -> MonsterTag -> Position -> MonsterTownAI
                  -> (Grid.Entry Monster -> Script TownEffect ())
                  -> CompileArea ()
simpleTownsperson vseed tag pos ai sfn = do
  let (vseed', vseed'') = splitVarSeed vseed
  mscript <- newMonsterScript vseed' sfn
  onStartDaily vseed'' $ do
    let mtype = getMonsterType tag
    () <$ tryAddMonster pos Monster
      { monstAnim = NoAnim,
        monstAdrenaline = 0,
        monstDeadVar = Nothing,
        monstFaceDir = FaceLeft,
        monstHealth = mtMaxHealth mtype,
        monstIsAlly = True,
        monstMoments = 0,
        monstName = mtName mtype,
        monstScript = Just mscript,
        monstStatus = initStatusEffects,
        monstTag = tag,
        monstTownAI = ai,
        monstType = mtype }

-------------------------------------------------------------------------------
-- Defining triggers:

class DefineTrigger m where
  type TriggerState m :: *
  type TriggerEffect m :: * -> *
  trigger :: VarSeed -> Predicate
          -> Script (TriggerEffect m) () -> m ()

instance DefineTrigger CompileArea where
  type TriggerState CompileArea = TownState
  type TriggerEffect CompileArea = TownEffect
  trigger vseed (Predicate predicate) action = CompileArea $ do
    cas <- State.get
    let trig = Trigger { triggerId = makeTriggerId vseed,
                         triggerPredicate = predicate, triggerAction = action }
    -- TODO verify uniqueness of trigger ID
    State.put cas { casTriggers = trig : casTriggers cas }

-- trigger :: VarSeed -> (TownState -> Bool) -> Script TownEffect ()
--         -> CompileArea ()
-- trigger vseed predicate action = CompileArea $ do
--   cas <- State.get
--   let trig = Trigger { triggerId = makeTriggerId vseed,
--                        triggerPredicate = predicate, triggerAction = action }
--   -- TODO verify uniqueness of trigger ID
--   State.put cas { casTriggers = trig : casTriggers cas }

onStartDaily :: VarSeed -> Script TownEffect () -> CompileArea ()
onStartDaily vseed = trigger vseed (Predicate $ const True)

onStartOnce :: VarSeed -> Script TownEffect () -> CompileArea ()
onStartOnce vseed = once vseed (Predicate $ const True)

daily :: VarSeed -> Predicate -> Script TownEffect ()
      -> CompileArea ()
daily vseed predicate script = do
  let (vseed', vseed'') = splitVarSeed vseed
  canFire <- newTransientVar vseed' True
  trigger vseed'' (varTrue canFire `andP` predicate) $ do
    writeVar canFire False >> script

once :: VarSeed -> Predicate -> Script TownEffect ()
     -> CompileArea ()
once vseed predicate script = do
  let (vseed', vseed'') = splitVarSeed vseed
  canFire <- newPersistentVar vseed' True
  trigger vseed'' (varTrue canFire `andP` predicate) $ do
    writeVar canFire False >> script

-------------------------------------------------------------------------------
-- Defining devices:

class DefineDevice m where
  newDevice :: VarSeed -> Int
            -> (Grid.Entry Device -> CharacterNumber -> Script AreaEffect ())
            -> m Device

instance DefineDevice CompileScenario where
  newDevice vseed radius sfn = CompileScenario $ do
    css <- State.get
    let di = makeDeviceId vseed
    when (Set.member di (cssDeviceIds css)) $ do
      fail ("Repeated device ID: " ++ show di)
    let device = Device { devId = di, devInteract = sfn, devRadius = radius }
    State.put css { cssDeviceIds = Set.insert di (cssDeviceIds css),
                    cssDevices = Map.insert di device (cssDevices css) }
    return device

instance DefineDevice CompileArea where
  newDevice vseed radius sfn = CompileArea $ do
    cas <- State.get
    let di = makeDeviceId vseed
    when (Set.member di $ casDeviceIds cas) $ do
      fail ("Repeated device ID: " ++ show di)
    let device = Device { devId = di, devInteract = sfn, devRadius = radius }
    State.put cas { casDeviceIds = Set.insert di (casDeviceIds cas),
                    casDevices = Map.insert di device (casDevices cas) }
    return device

uniqueDevice :: VarSeed -> Position -> Int
             -> (Grid.Entry Device -> CharacterNumber -> Script AreaEffect ())
             -> CompileArea ()
uniqueDevice vseed position radius sfn = do
  let (vseed', vseed'') = splitVarSeed vseed
  device <- newDevice vseed' radius sfn
  onStartDaily vseed'' $ addDevice_ device position

-------------------------------------------------------------------------------
-- Defining monster scripts:

class DefineMonsterScript m where
  newMonsterScript :: VarSeed -> (Grid.Entry Monster -> Script TownEffect ())
                   -> m MonsterScript

instance DefineMonsterScript CompileArea where
  newMonsterScript vseed sfn = CompileArea $ do
    cas <- State.get
    let msi = makeMonsterScriptId vseed
    when (Set.member msi $ casMonsterScriptIds cas) $ do
      fail ("Repeated monster script ID: " ++ show msi)
    let mscript = MonsterScript { mscriptId = msi, mscriptScriptFn = sfn }
    State.put cas { casMonsterScriptIds =
                      Set.insert msi (casMonsterScriptIds cas),
                    casMonsterScripts =
                      Map.insert msi mscript (casMonsterScripts cas) }
    return mscript

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

notP :: Predicate -> Predicate
notP (Predicate fn) = Predicate (not . fn)

getP :: (FromAreaEffect f) => Predicate -> Script f Bool
getP (Predicate fn) = areaGet fn

varTrue :: Var Bool -> Predicate
varTrue var = Predicate (getVar var)

varFalse :: Var Bool -> Predicate
varFalse var = Predicate (not . getVar var)

walkOn :: Position -> Predicate
walkOn pos = Predicate (\s -> any (pos ==) (arsPartyPositions s))

walkOff :: Position -> Predicate
walkOff = notP . walkOn

walkIn :: PRect -> Predicate
walkIn rect = Predicate (\s -> any (rectContains rect) (arsPartyPositions s))

{-
walkOn :: (AreaState s) => Position -> s -> Bool
walkOn pos ars = any (pos ==) (arsPartyPositions ars)

walkOff :: (AreaState s) => Position -> s -> Bool
walkOff pos ars = not (walkOn pos ars)

walkIn :: (AreaState s) => PRect -> s -> Bool
walkIn rect ars = any (rectContains rect) (arsPartyPositions ars)

infixr 3 #&&#
(#&&#) :: (s -> Bool) -> (s -> Bool) -> (s -> Bool)
fn1 #&&# fn2 = \s -> fn1 s && fn2 s

infixr 2 #||#
(#||#) :: (s -> Bool) -> (s -> Bool) -> (s -> Bool)
fn1 #||# fn2 = \s -> fn1 s || fn2 s
-}
-------------------------------------------------------------------------------
