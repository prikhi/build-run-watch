{-# LANGUAGE OverloadedStrings #-}
{-| Runner functions for Clients using Purescript's @spago@ dependency
manager.

-}
module BuildRunWatch.Spago
    ( runCommand
    , install
    , build
    , bundle
    , docs
    )
where

import           Data.Text                      ( Text )
import           UnliftIO                       ( MonadUnliftIO )
import           System.Exit                    ( ExitCode )

import           BuildRunWatch.Logging          ( MonadLoggable
                                                , logClientOutput
                                                )
import qualified BuildRunWatch.NPM             as NPM
import           BuildRunWatch.Runner           ( installDependency )


-- | Run a spago command in the given directory. Leverages @npx@ via the
-- 'NPM.exec' function.
runCommand
    :: (MonadUnliftIO m, MonadLoggable m)
    => Text
    -- ^ The spago command to run
    -> [Text]
    -- ^ Arguments to pass to the spago command
    -> FilePath
    -- ^ Path to the Purescript project
    -> m ExitCode
runCommand cmd args = NPM.exec "spago" (cmd : args)


-- | Install all dependencies specified in the @spago.dhall@ file.
--
-- Analogous to @npx spago install@.
install
    :: (MonadUnliftIO m, MonadLoggable m)
    => FilePath
    -- ^ Path to the Purescript project
    -> m ()
install workingDir = installDependency "npx"
                                       ["spago", "install"]
                                       workingDir
                                       logClientOutput
                                       "Purescript Dependencies"


-- | Build the Purescript project.
--
-- Analogous to @npx spago build@.
build
    :: (MonadUnliftIO m, MonadLoggable m)
    => FilePath
    -- ^ Path to the Purescript project
    -> m ExitCode
build = runCommand "build" []


-- | Bundle the Purescript project.
--
-- Analogous to @npx spago bundle@.
bundle
    :: (MonadUnliftIO m, MonadLoggable m)
    => FilePath
    -- ^ Path to the Purescript project
    -> m ExitCode
bundle = runCommand "bundle" []


-- | Generate the documentation for the Purescript project.
--
-- Analogous to @npx spago docs@.
docs
    :: (MonadUnliftIO m, MonadLoggable m)
    => FilePath
    -- ^ Path to the Purescript project
    -> m ExitCode
docs = runCommand "docs" []
