{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_DPLL (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/casparfriedrich/Library/Haskell/bin"
libdir     = "/Users/casparfriedrich/Library/Haskell/ghc-8.4.2-x86_64/lib/DPLL-1.0"
dynlibdir  = "/Users/casparfriedrich/Library/Haskell/ghc-8.4.2-x86_64/lib/x86_64-osx-ghc-8.4.2"
datadir    = "/Users/casparfriedrich/Library/Haskell/share/ghc-8.4.2-x86_64/DPLL-1.0"
libexecdir = "/Users/casparfriedrich/Library/Haskell/libexec/x86_64-osx-ghc-8.4.2/DPLL-1.0"
sysconfdir = "/Users/casparfriedrich/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DPLL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DPLL_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "DPLL_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "DPLL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DPLL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DPLL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
