{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_expressionbook (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\github\\xunilrj-sandbox\\sources\\haskell\\expressionbook\\.stack-work\\install\\eb32346d\\bin"
libdir     = "C:\\github\\xunilrj-sandbox\\sources\\haskell\\expressionbook\\.stack-work\\install\\eb32346d\\lib\\x86_64-windows-ghc-8.0.1\\expressionbook-0.1.0.0-LlBwiZN2GcABqwDhckJdFj"
datadir    = "C:\\github\\xunilrj-sandbox\\sources\\haskell\\expressionbook\\.stack-work\\install\\eb32346d\\share\\x86_64-windows-ghc-8.0.1\\expressionbook-0.1.0.0"
libexecdir = "C:\\github\\xunilrj-sandbox\\sources\\haskell\\expressionbook\\.stack-work\\install\\eb32346d\\libexec"
sysconfdir = "C:\\github\\xunilrj-sandbox\\sources\\haskell\\expressionbook\\.stack-work\\install\\eb32346d\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "expressionbook_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "expressionbook_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "expressionbook_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "expressionbook_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "expressionbook_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
