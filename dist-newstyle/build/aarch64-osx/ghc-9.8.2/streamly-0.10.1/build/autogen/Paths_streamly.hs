{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_streamly (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,10,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/jose-joro/.cabal/bin"
libdir     = "/Users/jose-joro/.cabal/lib/aarch64-osx-ghc-9.8.2/streamly-0.10.1-inplace"
dynlibdir  = "/Users/jose-joro/.cabal/lib/aarch64-osx-ghc-9.8.2"
datadir    = "/Users/jose-joro/.cabal/share/aarch64-osx-ghc-9.8.2/streamly-0.10.1"
libexecdir = "/Users/jose-joro/.cabal/libexec/aarch64-osx-ghc-9.8.2/streamly-0.10.1"
sysconfdir = "/Users/jose-joro/.cabal/etc"

getBinDir     = catchIO (getEnv "streamly_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "streamly_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "streamly_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "streamly_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "streamly_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "streamly_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
