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

module Fallback.Resource (tryGetResourcePath, getResourcePath) where

import qualified System.FilePath as FilePath (combine)
import qualified System.MacOSX.Bundle as Bundle (tryGetResourcePath)

import Fallback.Control.Error (IOEO, onlyIO)

-------------------------------------------------------------------------------

-- | Return the path of a game resource with the given name in the given
-- directory.  Return an error on failure.
tryGetResourcePath :: String {-^directory-} -> String {-^filename-}
                   -> IOEO FilePath
tryGetResourcePath dir name =
  fmap (maybe (fail "getResourcePath failed") id) $
  onlyIO $ Bundle.tryGetResourcePath (FilePath.combine dir name)

-- | Return the path of a game resource with the given name in the given
-- directory.  Throw an exception on failure.
getResourcePath :: String {-^directory-} -> String {-^filename-}
                -> IO FilePath
getResourcePath dir name =
  fmap (maybe (fail "getResourcePath failed") id) $
  Bundle.tryGetResourcePath (FilePath.combine dir name)

-------------------------------------------------------------------------------
