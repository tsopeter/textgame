{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_textgame (
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
version = Version [0,2,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/petertso/.cabal/bin"
libdir     = "/home/petertso/.cabal/lib/x86_64-linux-ghc-8.6.5/textgame-0.2.0.0-GDCBAuhAglMJ315d3NTeyB-textgame"
dynlibdir  = "/home/petertso/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/petertso/.cabal/share/x86_64-linux-ghc-8.6.5/textgame-0.2.0.0"
libexecdir = "/home/petertso/.cabal/libexec/x86_64-linux-ghc-8.6.5/textgame-0.2.0.0"
sysconfdir = "/home/petertso/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "textgame_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "textgame_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "textgame_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "textgame_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "textgame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "textgame_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
