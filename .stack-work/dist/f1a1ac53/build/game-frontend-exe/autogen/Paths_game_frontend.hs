{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_game_frontend (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "D:\\game-frontend\\.stack-work\\install\\1eee3797\\bin"
libdir     = "D:\\game-frontend\\.stack-work\\install\\1eee3797\\lib\\x86_64-windows-ghc-9.4.8\\game-frontend-0.1.0.0-HKjbdnMQ3387dTAlK2SqbP-game-frontend-exe"
dynlibdir  = "D:\\game-frontend\\.stack-work\\install\\1eee3797\\lib\\x86_64-windows-ghc-9.4.8"
datadir    = "D:\\game-frontend\\.stack-work\\install\\1eee3797\\share\\x86_64-windows-ghc-9.4.8\\game-frontend-0.1.0.0"
libexecdir = "D:\\game-frontend\\.stack-work\\install\\1eee3797\\libexec\\x86_64-windows-ghc-9.4.8\\game-frontend-0.1.0.0"
sysconfdir = "D:\\game-frontend\\.stack-work\\install\\1eee3797\\etc"

getBinDir     = catchIO (getEnv "game_frontend_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "game_frontend_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "game_frontend_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "game_frontend_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "game_frontend_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "game_frontend_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
