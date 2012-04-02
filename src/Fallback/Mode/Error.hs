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

module Fallback.Mode.Error
  (popupIfErrors)
where

import Data.List (intercalate)

import Fallback.Control.Error (IOEO, runEO, runIOEO)
import Fallback.Mode.Base
import Fallback.Mode.Narrate (newNarrateMode)
import Fallback.State.Resources (Resources)
import Fallback.View (View)

-------------------------------------------------------------------------------

popupIfErrors :: Resources -> View a b -> a -> IO Mode -> IOEO c
              -> (c -> IO NextMode) -> IO NextMode
popupIfErrors resources bgView bgInput onError ioeo onSuccess = do
  eo <- runIOEO ioeo
  case runEO eo of
    Left errors -> fmap ChangeMode $
                   newNarrateMode resources bgView bgInput
                                  (intercalate "\n\n" errors) onError
    Right value -> onSuccess value

-------------------------------------------------------------------------------
