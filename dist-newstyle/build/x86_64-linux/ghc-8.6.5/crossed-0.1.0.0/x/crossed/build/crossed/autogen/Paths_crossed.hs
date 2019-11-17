{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_crossed (
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

bindir     = "/home/josh/.cabal/bin"
libdir     = "/home/josh/.cabal/lib/x86_64-linux-ghc-8.6.5/crossed-0.1.0.0-inplace-crossed"
dynlibdir  = "/home/josh/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/josh/.cabal/share/x86_64-linux-ghc-8.6.5/crossed-0.1.0.0"
libexecdir = "/home/josh/.cabal/libexec/x86_64-linux-ghc-8.6.5/crossed-0.1.0.0"
sysconfdir = "/home/josh/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "crossed_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "crossed_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "crossed_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "crossed_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "crossed_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "crossed_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
