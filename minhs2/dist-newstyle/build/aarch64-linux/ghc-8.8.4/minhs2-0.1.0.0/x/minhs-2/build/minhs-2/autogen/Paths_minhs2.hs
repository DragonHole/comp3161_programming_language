{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_minhs2 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ubuntu/.cabal/bin"
libdir     = "/home/ubuntu/.cabal/lib/aarch64-linux-ghc-8.8.4/minhs2-0.1.0.0-inplace-minhs-2"
dynlibdir  = "/home/ubuntu/.cabal/lib/aarch64-linux-ghc-8.8.4"
datadir    = "/home/ubuntu/.cabal/share/aarch64-linux-ghc-8.8.4/minhs2-0.1.0.0"
libexecdir = "/home/ubuntu/.cabal/libexec/aarch64-linux-ghc-8.8.4/minhs2-0.1.0.0"
sysconfdir = "/home/ubuntu/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "minhs2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "minhs2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "minhs2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "minhs2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minhs2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minhs2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
