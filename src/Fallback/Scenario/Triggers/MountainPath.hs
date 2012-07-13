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

module Fallback.Scenario.Triggers.MountainPath
  (compileMountainPath, startingPosition)
where

import Control.Applicative ((<$>))
import Control.Monad (forM, zipWithM_)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Fallback.Data.Point
import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.State.Area
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Party (chrClass)
import Fallback.State.Resources
import Fallback.State.Simple
import Fallback.State.Tags
import Fallback.Utility (firstJust, flip3, maybeM)

-------------------------------------------------------------------------------

startingPosition :: Position
startingPosition = Point 14 6

compileMountainPath :: Globals -> CompileScenario ()
compileMountainPath globals = compileArea MountainPath Nothing $ do

  makeExit Corenglen [Rect 38 38 2 8, Rect 31 44 7 2] (Point 36 42)

  let isFirstTimeThroughLongvale = varEq (gTimesThroughLongvale globals) 0
  let isSecondTimeThroughLongvale = varEq (gTimesThroughLongvale globals) 1
  let isLastTimeThroughLongvale = varEq (gTimesThroughLongvale globals) 2

  trigger 908784 (walkIn "UpperPath1" `orP` walkIn "UpperPath2") $ do
    whenP isFirstTimeThroughLongvale $ do
      narrate "You look back up the mountain, towards the way you came\
        \ from.  Then, you turn around and head back down.  There's nothing\
        \ of interest on the path that led you here, but you feel sure,\
        \ somehow, that adventure awaits you down in Coringlen."
    whenP isSecondTimeThroughLongvale $ do
      narrate "You're still not sure what's going on, but you doubt you're\
        \ going to find any answers by heading back the way you came.  You'd\
        \ better continue on into Corenglen.  Again."
    whenP isLastTimeThroughLongvale $ do
      narrate "Just when you thought everything made sense, nothing makes\
        \ sense anymore.  You have to end this, and that means returning to\
        \ Corenglen one more time.  Squaring your shoulders, you turn to\
        \ descend through the valley."
    pos <- getPartyPosition
    partyWalkTo (pos `plusDir` DirSE)

  once 085585 (isFirstTimeThroughLongvale `andP` walkOff "Start") $ do
    narrate "You're finally here!  A brand new band of adventures, on a\
      \ journey to seek out your very first quest.  Without a doubt, the\
      \ days ahead will be filled with countless monsters, and hopefully,\
      \ even more countless treasures.\n\n\
      \After buying supplies and several days of hiking over the mountains,\
      \ you find yourselves descending through a narrow valley towards the\
      \ village of Corenglen.  The air smells fresh and the sun is just\
      \ beginning to set over the western peaks.\n\n\
      \You know very little about this village, but it's a remote town in\
      \ dangerous territory; surely {i}they'll{_} have some quests for you!"

  once 741589 (isSecondTimeThroughLongvale `andP` walkOff "Start") $ do
    narrate "You blink.  Your head feels fuzzy.  Looking around, you find\
      \ yourselves back on the mountain path leading down into Corenglen. \
      \ The sun is just beginning to set over the peaks to the west, just as\
      \ it was when you were hiking into Corenglen before.\n\n\
      \This is bizarre.  Could you really have gone back in time by half a\
      \ day?  Surely that's impossible--you've never heard of such magic. \
      \ But the only way you can think of to find out for sure is to head\
      \ down towards Corenglen again and see if that revenant shows up.  And\
      \ if the town throws another party for you, you're going to be asking\
      \ a lot more questions this time."

  loneRevenantDead <- newPersistentVar 960522 False
  once 582362 (isFirstTimeThroughLongvale `andP`
               (walkIn "MidPath1" `orP` walkIn "MidPath2")) $ do
    narrate "As you are walking down the valley, you suddenly hear a noise\
      \ and turn to see a small rock bouncing down the path from above. \
      \ Skip, skip, skip.  Someone or something else is up there, behind\
      \ you.  Then you hear an unearthly shriek.\n\n\
      \You freeze.  You know what that shriek is; you've heard it before. \
      \ That's the sound of a revenant: a dangerous undead creature of pure\
      \ magic, usually made by a wizard.  Usually an evil wizard.  They are\
      \ quite nasty.\n\n\
      \Presently you see it hurtle around the bend above you, making a\
      \ beeline down to the town below and screaming all the way.  It\
      \ doesn't look like a terribly powerful specimen, but still big enough\
      \ that it would probably kill a few townspeople if you didn't stop it,\
      \ even given that they can probably already hear it coming.  You ready\
      \ your weapons and move to attack it.  You just hope it won't kill a\
      \ few of {i}you{_}..."
    addBasicEnemyMonster (Point 33 13) Revenant (Just loneRevenantDead)
                         ChaseAI
  once 520543 (isFirstTimeThroughLongvale `andP`
               varTrue loneRevenantDead) $ do
    wait 40
    narrate "Whew...one less horror in the world.  Looks like the village\
      \ below is safe for now.  Not that they wouldn't have been mostly fine\
      \ without you--their militia, unlike you, have probably actually\
      \ fought off monsters before--but still, you may have saved a life or\
      \ two.  Maybe they'll give you free drinks at the pub or\
      \ something.\n\n\
      \Now, time to get back to that adventure you've been meaning to get\
      \ started on."

  once 309037 (isSecondTimeThroughLongvale `andP`
               (walkIn "MidPath1" `orP` walkIn "MidPath2")) $ do
    narrate "As you are walking down the valley, you suddenly hear a noise\
      \ and turn to see a small rock bouncing down the path from above. \
      \ Skip, skip, skip.  You frown.  You could have sworn that that exact\
      \ same rock bounced in the {i}exact{_} same way when the revenant\
      \ attacked you yesterday.  In fact, everything here been the same as\
      \ it was yesterday, from the look of the sunset to the smell of the\
      \ air.  This can't all be a coincidence.  It seems that this really\
      \ {i}is{_} yesterday?\n\n\
      \You wince preemptively for the shrill cry of the revenant that you\
      \ know is about to appear.  But suddenly, you are nearly deafened by\
      \ what sounds like hundreds of unearthly screams, as though the gates\
      \ of hell themselves had been thrown open on the path above you."
    narrate "The blood drains from your faces as a huge group of revenants\
      \ stampede around the bend above.  There must be dozens of them, the\
      \ weakest of which puts the one you fought before to shame.  The\
      \ leader of the pack is twenty feet tall and blazing with sickly-blue\
      \ light; it looks like it could kill a dozen soldiers just by\
      \ coughing.\n\n\
      \You've heard tales of this before--packs of revenants attacking\
      \ villages, wreaking horrible deaths upon them and leaving no\
      \ survivors--though you've never heard of any so large as this one,\
      \ and anyway you didn't think the tales were true.  You feel sick as\
      \ you realize that the town below can almost certainly already see and\
      \ hear these revenants coming from afar, but will be unable to escape\
      \ in time, walled in as they are by the valley.  They will know that\
      \ they are doomed, but will be able to do little more than cower in\
      \ absolute terror for the next ten minutes or so, waiting for their\
      \ inevitable deaths.\n\n\
      \Then you feel even sicker as you realize, also, that you are right in\
      \ between the pack and the village, and the valley walls offer you no\
      \ escape."
    -- TODO create revenant pack, give astral weapons to party
    numsAndClasses <- forM [minBound .. maxBound] $ \charNum -> do
      (,) charNum . chrClass <$> areaGet (arsGetCharacter charNum)
    flip State.evalStateT (Map.fromList numsAndClasses,
                           Set.fromList [Sunrod, Starspear, Moonbow,
                                         Lifeblade]) $ do
      let tryAssign (item, cls) = do
            (chars, items) <- State.get
            if Set.notMember item items then return () else do
            let match (n, c) = if c == cls then Just n else Nothing
            maybeM (firstJust match $ Map.assocs chars) $ \charNum -> do
              State.lift $ grantAndEquipWeapon item charNum
              State.put (Map.delete charNum chars, Set.delete item items)
      mapM_ tryAssign [(Sunrod, MagusClass),
                       (Moonbow, HunterClass),
                       (Lifeblade, WarriorClass),
                       (Starspear, AlchemistClass),
                       (Sunrod, ClericClass),
                       (Sunrod, AlchemistClass),
                       (Moonbow, RogueClass),
                       (Moonbow, MagusClass),
                       (Moonbow, ClericClass),
                       (Sunrod, HunterClass),
                       (Lifeblade, RogueClass),
                       (Sunrod, RogueClass),
                       (Starspear, WarriorClass),
                       (Starspear, RogueClass),
                       (Lifeblade, ClericClass),
                       (Moonbow, AlchemistClass),
                       (Starspear, MagusClass)]
      (chars, items) <- State.get
      flip3 zipWithM_ (Map.keys chars) (Set.elems items) $ \charNum tag -> do
        State.lift $ grantAndEquipWeapon tag charNum
    narrate "You reach for your weapons, knowing that you don't stand a\
      \ snowball's chance in hell against these monsters.  Maybe you can at\
      \ least slow them down enough for some of the villagers to escape? \
      \ No, that's a joke.  You're all going to be dead in about thirty\
      \ seconds, tops.  Still, you have no choice but to try."
    -- TODO start combat (makes "schwing!" noise) before showing this next
    -- message.
    narrate "But when you pull out your weapons, you are shocked to find not\
      \ the meager armaments that you were barely able to afford when\
      \ packing for your journey out here, but four powerful magical\
      \ artifacts--a sword, a bow, a spear, and a wand--the likes of which\
      \ you have never seen, or even {i}heard{_} of.  These are no ordinary\
      \ enchanted weapons, like the sort that veteran adventurers are always\
      \ bragging about winning from daring quests.  You can actually\
      \ {i}feel{_} the power flowing through you the moment you lay your\
      \ hands on them.\n\n\
      \Each of the four bears a different inscription:\n\n\
      \    {c}Lifeblade, the fount of strength and light.{_}\n\n\
      \    {c}Moonbow, the master of the night.{_}\n\n\
      \    {c}Starspear, the guard that hell hath dread.{_}\n\n\
      \    {c}Sunrod, the bane of all undead.{_}\n\n\
      \Whoa.  With weapons like these...you might actually survive this.\n\n\
      \But where could they have come from?"
    startMusic MusicMovementProposition

-------------------------------------------------------------------------------
