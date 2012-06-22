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

module Fallback.Scenario.MonsterAI.Script
  (getMonsterFieldOfView, getMonsterOpponentPositions)
where

import Control.Applicative ((<$>))
import qualified Data.Set as Set

import Fallback.Constants (sightRangeSquared)
import qualified Fallback.Data.Grid as Grid
import Fallback.Data.Point
import Fallback.Scenario.Script
import Fallback.State.Area
import Fallback.State.Creature
import Fallback.State.FOV (fieldOfView)
import Fallback.State.Party (chrStatus)
import Fallback.State.Simple (Invisibility(..))
import Fallback.State.Status (seInvisibility)
import Fallback.State.Terrain (terrainSize)
import Fallback.Utility (forMaybeM)

-------------------------------------------------------------------------------

-- | Return the set of positions visible to the specified monster.
getMonsterFieldOfView :: (FromAreaEffect f) => Grid.Key Monster
                      -> Script f (Set.Set Position)
getMonsterFieldOfView key = do
  maybeMonsterEntry key Set.empty $ \entry -> do
  size <- terrainSize <$> areaGet arsTerrain
  isOpaque <- areaGet arsIsOpaque
  return (foldr (fieldOfView size isOpaque sightRangeSquared) Set.empty $
          prectPositions $ Grid.geRect entry)

-- | Return the list of positions occupied by foes of the given monster,
-- excepting those that are invisible to the monster.  The first argument
-- indicates whether the monster is charmed; if it is true, this will return
-- positions of the monster's friends instead.
getMonsterOpponentPositions :: (FromAreaEffect f) => Bool -> Grid.Key Monster
                            -> Script f [Position]
getMonsterOpponentPositions charmed key = do
  maybeMonsterEntry key [] $ \entry -> do
  let isAlly = charmed /= monstIsAlly (Grid.geValue entry)
  let eyePrect = expandPrect $ Grid.geRect entry
  positions1 <- if isAlly then return [] else do
    charNums <- getAllConsciousCharacters
    forMaybeM charNums $ \charNum -> do
      let getPosition = areaGet (arsCharacterPosition charNum)
      if charmed then Just <$> getPosition else do
      invis <- areaGet (seInvisibility . chrStatus . arsGetCharacter charNum)
      case invis of
        NoInvisibility -> Just <$> getPosition
        MinorInvisibility -> do
          pos <- getPosition
          return $ if rectContains eyePrect pos then Just pos else Nothing
        MajorInvisibility -> return Nothing
  positions2 <- do
    entries <- if isAlly then getAllEnemyMonsters else getAllAllyMonsters
    return $ flip concatMap entries $ \entry' ->
      if Grid.geKey entry' == key then [] else
        let prect' = Grid.geRect entry'
            positions = prectPositions prect'
        in if charmed then positions else
             case monstInvisibility $ Grid.geValue entry' of
               NoInvisibility -> positions
               MinorInvisibility ->
                 if rectIntersects eyePrect prect' then positions else []
               MajorInvisibility -> []
  return (positions1 ++ positions2)

-------------------------------------------------------------------------------
