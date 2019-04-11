{-# LANGUAGE OverloadedStrings #-}
{-| Command running functions for build scripts.

TODO: Use the pathtype package to accept typed relative paths for the
      working directories.
-}
module BuildRunWatch.Runner
    ( run
    , runInteractive
    , installDependency
    , removeFiles
    )
where

import           Control.Monad                  ( void )
import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import           GHC.IO.Handle                  ( Handle )
import           System.Exit                    ( ExitCode )
import           System.Process.Typed           ( proc
                                                , setWorkingDir
                                                , getStdout
                                                , setStdout
                                                , getStderr
                                                , setStderr
                                                , setDelegateCtlc
                                                , createPipe
                                                , withProcess
                                                , waitExitCode
                                                )
import           UnliftIO                       ( MonadUnliftIO
                                                , withRunInIO
                                                )
import           UnliftIO.Async                 ( Async
                                                , async
                                                , cancel
                                                )

import           BuildRunWatch.Logging          ( MonadLoggable(..)
                                                , LogType(..)
                                                , logExitStatus
                                                )


-- | Run a command with some arguemnts, a working directory, and an
-- asynchronous output logger.
--
-- The process is started in the working directory and it's stdout and
-- stderr are piped into the output logger function. When the process
-- exits, we stop the asynchronous output loggers and return the process's
-- exit code.
run
    :: MonadUnliftIO m
    => FilePath
    -- ^ The command to run
    -> [String]
    -- ^ Arguments to pass to the command
    -> FilePath
    -- ^ The working directory to run the command in
    -> (Handle -> m (Async a))
    -- ^ The output logging function
    -> m ExitCode
run cmd args workingDir outputLogger =
    let processConfig =
            proc cmd args
                & setWorkingDir workingDir
                & setStdout createPipe
                & setStderr createPipe
    in  withRunInIO $ \runner -> withProcess processConfig $ \process -> do
            outputThread <- runner . outputLogger $ getStdout process
            errorThread  <- runner . outputLogger $ getStderr process
            exitCode     <- waitExitCode process
            cancel outputThread
            cancel errorThread
            return exitCode


-- | Run an interactive process, passing through all input/output streams.
runInteractive
    :: MonadUnliftIO m
    => FilePath
    -- ^ The command to run
    -> [String]
    -- ^ Arguments to pass to the command
    -> FilePath
    -- ^ The working directory to run the command in
    -> m ExitCode
runInteractive cmd args workingDir =
    let processConfig =
            proc cmd args & setWorkingDir workingDir & setDelegateCtlc True
    in  withRunInIO $ \_ -> withProcess processConfig waitExitCode

-- | Install a dependency via a command, arguments, and working directory.
--
-- Both the command output & it's exit status is logged. The dependency
-- description is used in the logging output. E.g., with a description of
-- @NPM Dependencies@, a successful install will log an 'Info' message of
-- @Installing NPM Dependencies@ & a 'Success' message of @NPM Dependencies
-- Installed@.
installDependency
    :: (MonadUnliftIO m, MonadLoggable m)
    => FilePath
    -- ^ The command to run
    -> [String]
    -- ^ Arguments to pass to the command
    -> FilePath
    -- ^ The Working Directory
    -> (Handle -> m (Async a))
    -- ^ The output logging function
    -> Text
    -- ^ A description of the dependency
    -> m ()
installDependency cmd args workingDir outputLogger description = do
    logMessage Info $ "Installing " <> description
    exitCode <- run cmd args workingDir outputLogger
    logExitStatus (description <> " Installed")
                  (description <> " Installation Failed")
                  exitCode


-- | Remove some files/directories from the given working directory.
--
-- With a description of @Client Files@, this function will log an 'Info'
-- message of @Removing Client Files@.
--
-- Warning: This runs @rm -rf <paths>@! Use it at your own risk.
--
-- TODO: There's probably a removeFile/removeDirectory function that we
-- could use instead of relying on the existence of an @rm@ command.
removeFiles
    :: (MonadUnliftIO m, MonadLoggable m)
    => FilePath
    -- ^ The directory to remove files from
    -> [FilePath]
    -- ^ The files & directories to remove
    -> Text
    -- ^ A description of the files being removed
    -> m ()
removeFiles workingDir paths description = do
    let nullLogger = const . async $ return ()
    logMessage Info $ "Removing " <> description
    void $ run "rm" ("-rf" : paths) workingDir nullLogger
