{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_porthos (
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

bindir     = "/Users/adrian/Library/Haskell/bin"
libdir     = "/Users/adrian/Library/Haskell/ghc-8.0.2-x86_64/lib/porthos-0.1.0.0"
dynlibdir  = "/Users/adrian/Library/Haskell/ghc-8.0.2-x86_64/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/adrian/Library/Haskell/share/ghc-8.0.2-x86_64/porthos-0.1.0.0"
libexecdir = "/Users/adrian/Library/Haskell/libexec"
sysconfdir = "/Users/adrian/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "porthos_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "porthos_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "porthos_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "porthos_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "porthos_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "porthos_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
