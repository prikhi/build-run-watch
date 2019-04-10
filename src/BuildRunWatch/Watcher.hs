{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-| Watch source files, re-building & re-running executables when they
change.

-}
module BuildRunWatch.Watcher
    ( watch
    )
where


import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( void
                                                , forever
                                                , when
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Text                      ( Text )
import           UnliftIO                       ( MonadUnliftIO
                                                , withRunInIO
                                                )
import           UnliftIO.Async                 ( Async
                                                , async
                                                , cancel
                                                )
import           UnliftIO.STM                   ( TVar
                                                , atomically
                                                , newTVarIO
                                                , readTVarIO
                                                , writeTVar
                                                )
import           System.Exit                    ( ExitCode(..) )
import           System.FSNotify                ( Debounce(Debounce)
                                                , WatchConfig(confDebounce)
                                                , WatchManager
                                                , defaultConfig
                                                , eventPath
                                                , withManagerConf
                                                , watchTree
                                                )

import           BuildRunWatch.Logging          ( MonadLoggable(..)
                                                , LogType(..)
                                                )


-- | This function is the basic "watcher" that builds & runs an executable
-- everytime a source file changes. It takes three monadic actions, one for
-- initializing a project(e.g., installing dependencies), one for building
-- an executable, and one for running an executable.
--
-- The build & run actions are run asynchronously and the passed "FilePath" is
-- watched for file additions/removals/modifications. Every file change is
-- checked by the two @FilePath -> Bool@ functions - the first checks to
-- see if the file change means we need to re-initialize the project and
-- the other checks to see if the file change means we need to re-build
-- & re-run the project.
--
-- When a file change requires some action from us, a new build is kicked
-- off. If the build is successful, the running executable is killed and
-- the new executable is started. When a file change occurs during the
-- build process, the build is restarted.
--
-- The description argument is used for logging various stages - "Watching
-- Server", "Server Build Successful", "Server Restarted", etc.
watch
    :: (MonadUnliftIO m, MonadLoggable m)
    => Text
    -- ^ Description of the watcher
    -> FilePath
    -- ^ The directory to watch
    -> (FilePath -> Bool)
    -- ^ Does the file require re-initialization?
    -> (FilePath -> Bool)
    -- ^ Does the file require re-running?
    -> m ()
    -- ^ The project initialization action
    -> m ExitCode
    -- ^ The executable's build action
    -> m ExitCode
    -- ^ The executable's run action
    -> m ()
watch description watchDirectory initializeFilter fileFilter initializeAction buildAction runAction
    = watchInIO $ \runner manager -> do
        runner initializeAction
        buildResult <- runner buildAction
        asyncBuild  <- newTVarIO Nothing
        asyncRun    <- newTVarIO =<< case buildResult of
            ExitSuccess   -> Just <$> async (runner runAction)
            ExitFailure _ -> return Nothing
        runner $ logMessage Info $ "Watching " <> description
        void
            . watchTree manager watchDirectory (isWatchedFile . eventPath)
            $ runner
            . reinitOrRerun asyncBuild asyncRun
            . eventPath
        forever $ threadDelay maxBound
  where
    -- Are we watching the path?
    isWatchedFile :: FilePath -> Bool
    isWatchedFile path = initializeFilter path || fileFilter path
    -- Lift a monadic watcher into IO, debouncing events occuring within
    -- 100ms of each other.
    watchInIO
        :: MonadUnliftIO m
        => ((forall a . m a -> IO a) -> WatchManager -> IO b)
        -> m b
    watchInIO f =
        let watcherConfig = defaultConfig { confDebounce = Debounce 100 }
        in  withRunInIO $ \runner -> withManagerConf watcherConfig $ f runner
    -- Re-build & re-run the executable, reinitializing if necessary
    -- If the build action fails, we keep the previous instance of the
    -- executable running.
    reinitOrRerun builderTVar runnerTVar path = do
        when (initializeFilter path) $ do
            logMessage Info $ description <> " Reinitialization Triggered"
            initializeAction

        cancelAndClearTVar builderTVar
            $  "File Changed During Build, Cancelling "
            <> description
            <> " Build"
        runAndSetTVar builderTVar $ buildAction >>= \case
            ExitSuccess -> do
                logMessage Success $ description <> " Build Successful"
                cancelAndClearTVar runnerTVar $ "Restarting " <> description
                runAndSetTVar runnerTVar runAction
            ExitFailure _ ->
                logMessage Failure $ description <> " Build Failure"
    runAndSetTVar :: MonadUnliftIO m => TVar (Maybe (Async a)) -> m a -> m ()
    runAndSetTVar tvar action = do
        asyncAction <- async action
        atomically $ writeTVar tvar $ Just asyncAction
    cancelAndClearTVar
        :: (MonadUnliftIO m, MonadLoggable m)
        => TVar (Maybe (Async a))
        -> Text
        -> m ()
    cancelAndClearTVar tvar message = readTVarIO tvar >>= traverse_
        (\val -> cancel val >> logMessage Info message >> atomically
            (writeTVar tvar Nothing)
        )
