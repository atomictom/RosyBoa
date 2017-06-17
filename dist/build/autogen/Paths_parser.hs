module Paths_parser (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/thomasmanning/.cabal/bin"
libdir     = "/home/thomasmanning/.cabal/lib/parser-0.0.0.1/ghc-7.6.3"
datadir    = "/home/thomasmanning/.cabal/share/parser-0.0.0.1"
libexecdir = "/home/thomasmanning/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "parser_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parser_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)