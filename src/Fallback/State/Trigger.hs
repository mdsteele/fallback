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

-- | A trigger is a script that runs when a certain condition is met.  For
-- example, a trigger could be created to pop up a message when the party walks
-- into a certain area.  This module provides functions for managing lists of
-- triggers, and determining which trigger should run at a given time.

module Fallback.State.Trigger
  (-- * @Trigger@ type
   Trigger, makeTrigger,
   -- * @Triggers@ type
   Triggers, makeUnfiredTriggers, fireTrigger,
   distillTriggers, reconstituteTriggers)
where

import Control.Arrow (first)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, member)

import Fallback.Control.Script (Script)
import Fallback.State.Progress (TriggerId)

-------------------------------------------------------------------------------

-- | A trigger is an action (with effect type @f@) that takes place when a
-- certain condition of the input state (of type @s@) is met.
data Trigger s f = Trigger
  { triggerId :: TriggerId s f,
    triggerCondition :: s -> Bool,
    triggerAction :: Script f () }

-- | Create a trigger with the given ID, condition, and action.
makeTrigger :: TriggerId s f -> (s -> Bool) -> Script f () -> Trigger s f
makeTrigger = Trigger

-------------------------------------------------------------------------------

-- | An ordered list of triggers, keeping track of which triggers in the list
-- are marked as \"fired\".  See the 'fireTrigger' function for details.
newtype Triggers s f = Triggers [(Trigger s f, Bool)]

-- | Given a list of 'Trigger' objects, create a 'Triggers' object with all the
-- triggers marked as not fired.
makeUnfiredTriggers :: [Trigger s f] -> Triggers s f
makeUnfiredTriggers = reconstituteTriggers Set.empty

-- | Check to see if there is a trigger to fire; return the updated list of
-- triggers, and the action to execute (if any).  If at least once trigger is
-- eligible to fire (that is, its condition is true and it is not currently
-- marked as fired), then the /first/ such trigger in the list is marked as
-- fired, and its action is returned; other eligible triggers are unchanged.
-- Additionally, all triggers currently marked as fired whose conditions are no
-- longer true are unmarked.
fireTrigger :: s -> Triggers s f -> (Triggers s f, Maybe (Script f ()))
fireTrigger state (Triggers ts) = first Triggers $ testTriggers ts where
  resetTriggers = map $ \(trigger, fired) ->
    (trigger, fired && triggerCondition trigger state)
  testTriggers [] = ([], Nothing)
  testTriggers ((trigger, fired) : rest) =
    case (fired, triggerCondition trigger state) of
      (False, True) ->
        ((trigger, True) : resetTriggers rest, Just (triggerAction trigger))
      (True, False) ->
        first ((trigger, False) :) $ testTriggers rest
      (_, _) -> first ((trigger, fired) :) $ testTriggers rest

-- | Return the set of IDs of triggers that are marked as fired.
distillTriggers :: Triggers s f -> Set (TriggerId s f)
distillTriggers (Triggers ts) =
  Set.fromList $ map (triggerId . fst) $ filter snd ts

-- | Given the set returned from 'distillTriggers', and the original list of
-- triggers, recreate the 'Triggers' object with the appropriate triggers
-- marked as fired.
reconstituteTriggers :: Set (TriggerId s f) -> [Trigger s f] -> Triggers s f
reconstituteTriggers ids = Triggers . map fn where
  fn trigger = (trigger, Set.member (triggerId trigger) ids)

-------------------------------------------------------------------------------
