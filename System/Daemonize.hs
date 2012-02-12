{-# LANGUAGE ForeignFunctionInterface #-}
module System.Daemonize (
                         DaemonOptions(..),
                         defaultDaemonOptions,
                         daemonize
                        )
  where

import qualified Control.Exception as Exception
import Foreign
import Foreign.C
import System.Exit
import System.IO
import qualified System.Posix as POSIX

import Control.Concurrent


data DaemonOptions = DaemonOptions {
    daemonShouldChangeDirectory :: Bool,
    daemonShouldRedirectStandardStreams :: Bool,
    daemonShouldCloseStandardStreams :: Bool,
    daemonShouldIgnoreSignals :: Bool,
    daemonUserToChangeTo :: Maybe String,
    daemonGroupToChangeTo :: Maybe String
  }


defaultDaemonOptions :: DaemonOptions
defaultDaemonOptions = DaemonOptions {
                         daemonShouldChangeDirectory = True,
                         daemonShouldRedirectStandardStreams = False,
                         daemonShouldCloseStandardStreams = True,
                         daemonShouldIgnoreSignals = True,
                         daemonUserToChangeTo = Nothing,
                         daemonGroupToChangeTo = Nothing
                       }


foreign import ccall "daemon" c_daemon :: CInt -> CInt -> IO CInt


daemonize :: DaemonOptions -> IO () -> IO ()
daemonize options action = do
  case daemonGroupToChangeTo options of
    Nothing -> return ()
    Just groupName -> do
      groupEntry <- POSIX.getGroupEntryForName groupName
      POSIX.setGroupID $ POSIX.groupID groupEntry
  case daemonUserToChangeTo options of
    Nothing -> return ()
    Just userName -> do
      userEntry <- POSIX.getUserEntryForName userName
      POSIX.setUserID $ POSIX.userID userEntry
  _ <- POSIX.forkProcess $ daemonize' options action
  POSIX.exitImmediately ExitSuccess


daemonize' :: DaemonOptions -> IO () -> IO ()
daemonize' options action = do
  let c_shouldChangeDirectory
        = if daemonShouldChangeDirectory options
            then 0
            else 1
      c_shouldRedirectStandardStreams
        = if daemonShouldRedirectStandardStreams options
            then 0
            else 1
  throwErrnoIfMinus1 "daemonize"
                     $ c_daemon c_shouldChangeDirectory
                                c_shouldRedirectStandardStreams
  if daemonShouldIgnoreSignals options
    then do
      POSIX.installHandler POSIX.sigTTOU POSIX.Ignore Nothing
      POSIX.installHandler POSIX.sigTTIN POSIX.Ignore Nothing
      POSIX.installHandler POSIX.sigTSTP POSIX.Ignore Nothing
      return ()
    else return ()
  if daemonShouldCloseStandardStreams options
    then mapM_ hClose [stdin, stdout, stderr]
    else return ()
  action
