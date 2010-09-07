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
import System.IO
import qualified System.Posix as POSIX


data DaemonOptions = DaemonOptions {
    daemonShouldChangeDirectory :: Bool,
    daemonShouldRedirectStandardStreams :: Bool,
    daemonShouldCloseAllStreams :: Bool,
    daemonFileDescriptorsToLeaveOpen :: [POSIX.Fd],
    daemonShouldIgnoreSignals :: Bool,
    daemonUserToChangeTo :: Maybe String,
    daemonGroupToChangeTo :: Maybe String
  }


defaultDaemonOptions :: DaemonOptions
defaultDaemonOptions = DaemonOptions {
                         daemonShouldChangeDirectory = True,
                         daemonShouldRedirectStandardStreams = False,
                         daemonShouldCloseAllStreams = True,
                         daemonFileDescriptorsToLeaveOpen = [],
                         daemonShouldIgnoreSignals = True,
                         daemonUserToChangeTo = Nothing,
                         daemonGroupToChangeTo = Nothing
                       }


foreign import ccall "daemon" c_daemon :: CInt -> CInt -> IO CInt


daemonize :: DaemonOptions -> IO ()
daemonize options = do
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
  if daemonShouldCloseAllStreams options
    then do
      mapM hClose [stdin, stdout, stderr]
      let closeLoop i | i == 65536 = return ()
                      | otherwise = do
                          if not $ elem (POSIX.Fd i)
                                        $ daemonFileDescriptorsToLeaveOpen options
                            then Exception.catch (POSIX.closeFd $ POSIX.Fd i)
                                   (\e -> do
                                      return (e :: Exception.SomeException)
                                      return ())
                            else return ()
                          closeLoop $ i + 1
      closeLoop 0
    else return ()
