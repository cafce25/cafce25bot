{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_chatbot (
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

bindir     = "/mnt/e/work/chatbot/.stack-work/install/x86_64-linux/5f16ea143f59972321329454261323f5b78c872f49bae0ccef65557e115c5005/8.8.4/bin"
libdir     = "/mnt/e/work/chatbot/.stack-work/install/x86_64-linux/5f16ea143f59972321329454261323f5b78c872f49bae0ccef65557e115c5005/8.8.4/lib/x86_64-linux-ghc-8.8.4/chatbot-0.1.0.0-JUrfY3nOyd2JMTuJyoCOAG-chatbot"
dynlibdir  = "/mnt/e/work/chatbot/.stack-work/install/x86_64-linux/5f16ea143f59972321329454261323f5b78c872f49bae0ccef65557e115c5005/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/mnt/e/work/chatbot/.stack-work/install/x86_64-linux/5f16ea143f59972321329454261323f5b78c872f49bae0ccef65557e115c5005/8.8.4/share/x86_64-linux-ghc-8.8.4/chatbot-0.1.0.0"
libexecdir = "/mnt/e/work/chatbot/.stack-work/install/x86_64-linux/5f16ea143f59972321329454261323f5b78c872f49bae0ccef65557e115c5005/8.8.4/libexec/x86_64-linux-ghc-8.8.4/chatbot-0.1.0.0"
sysconfdir = "/mnt/e/work/chatbot/.stack-work/install/x86_64-linux/5f16ea143f59972321329454261323f5b78c872f49bae0ccef65557e115c5005/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chatbot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chatbot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chatbot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chatbot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chatbot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chatbot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
