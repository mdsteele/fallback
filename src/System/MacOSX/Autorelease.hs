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

{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module System.MacOSX.Autorelease
  (AutoreleasePool, newAutoreleasePool)
where

import Foreign.C (CString, withCAString)
import Foreign.Ptr (Ptr)

-------------------------------------------------------------------------------

data AutoreleasePool

newAutoreleasePool :: IO (Ptr AutoreleasePool)
newAutoreleasePool = do
  poolClass <- withCAString "NSAutoreleasePool" objc_lookUpClass
  allocSel <- withCAString "alloc" sel_getUid
  pool <- objc_msgSend poolClass allocSel
  initSel <- withCAString "init" sel_getUid
  objc_msgSend pool initSel

-------------------------------------------------------------------------------

data Class
data Sel

foreign import ccall unsafe objc_lookUpClass :: CString -> IO (Ptr Class)

foreign import ccall unsafe objc_msgSend :: Ptr a -> Ptr Sel -> IO (Ptr b)

foreign import ccall unsafe sel_getUid :: CString -> IO (Ptr Sel)

-------------------------------------------------------------------------------
