{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Pancake (
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

bindir     = "/home/david/Documents/Haskell/Pancake/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/bin"
libdir     = "/home/david/Documents/Haskell/Pancake/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/lib/x86_64-linux-ghc-8.4.4/Pancake-0.1.0.0-A16bPMzPAYq62C5YN4qnn2-Pancake"
dynlibdir  = "/home/david/Documents/Haskell/Pancake/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/david/Documents/Haskell/Pancake/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/share/x86_64-linux-ghc-8.4.4/Pancake-0.1.0.0"
libexecdir = "/home/david/Documents/Haskell/Pancake/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/libexec/x86_64-linux-ghc-8.4.4/Pancake-0.1.0.0"
sysconfdir = "/home/david/Documents/Haskell/Pancake/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Pancake_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Pancake_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Pancake_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Pancake_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Pancake_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Pancake_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
