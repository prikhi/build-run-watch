{-# LANGUAGE OverloadedStrings #-}
{-| Runner function for Servers/Clients that use Stack.

You'll probably want to use a qualified import for this module:

> import qualified BuildRunWatch.Stack as Stack
>
> installGhcForServer = Stack.setup [] "./server/" logServerOutput

-}
module BuildRunWatch.Stack
    ( setup
    , buildDependencies
    , build
    , exec
    , docs
    )
where

import           GHC.IO.Handle                  ( Handle )
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Async                 ( Async )
import           System.Exit                    ( ExitCode )

import           BuildRunWatch.Logging          ( HasLogQueue )
import           BuildRunWatch.Runner           ( installDependency
                                                , run
                                                )


-- | Ensure that the GHC version for a Stack project is installed.
--
-- Analogous to @stack setup@.
setup
    :: (MonadUnliftIO m, HasLogQueue m)
    => [String]
    -- ^ Additional arguments to pass to @stack setup@
    -> FilePath
    -- ^ The working directory of the stack project
    -> (Handle -> m (Async a))
    -- ^ The output logging function
    -> m ()
setup extraArgs workingDir outputLogger = installDependency
    "stack"
    (["setup", "--color=always"] <> extraArgs)
    workingDir
    outputLogger
    "GHC"


-- | Build only the dependencies for a Stack project.
--
-- Analogous to @stack build --only-dependencies@.
buildDependencies
    :: (MonadUnliftIO m, HasLogQueue m)
    => [String]
    -- ^ Additional arguments to pass to @stack build@
    -> FilePath
    -- ^ The working directory of the stack project
    -> (Handle -> m (Async a))
    -- ^ The output logging function
    -> m ()
buildDependencies extraArgs workingDir outputLogger = installDependency
    "stack"
    (["build", "--only-dependencies", "--color=always"] <> extraArgs)
    workingDir
    outputLogger
    "Haskell Dependencies"


-- | Build the entire Stack project.
--
-- Analogous to @stack build@.
build
    :: MonadUnliftIO m
    => [String]
    -- ^ Additional arguments to pass to @stack build@
    -> FilePath
    -- ^ The working directory of the stack project
    -> (Handle -> m (Async a))
    -- ^ The output logging function
    -> m ExitCode
build extraArgs = run "stack" (["build", "--color=always"] <> extraArgs)


-- | Run an executable built from the Stack project.
--
-- Analogous to @stack exec@.
exec
    :: MonadUnliftIO m
    => String
    -- ^ The command to run
    -> [String]
    -- ^ Arguments to pass to the command
    -> FilePath
    -- ^ The working directory of the stack project
    -> (Handle -> m (Async a))
    -- ^ The output logging function
    -> m ExitCode
exec cmd args = run "stack" (["exec", cmd, "--"] <> args)


-- | Build the documentation for a Stack project. You may optionally open
-- the built docs in a browser, for a specific package if desired.
--
-- Analogous to @stack haddock --open \<pkg-name>@.
docs
    :: MonadUnliftIO m
    => Bool
    -- ^ Open the documentation in the default web browser?
    -> Maybe String
    -- ^ Build the documentation for a specific package. With the @--open@
    -- flag, the documentation for this specific package will be opened.
    -> [String]
    -- ^ Additional arguments to pass to @stack haddock@
    -> FilePath
    -- ^ The working directory of the stack project
    -> (Handle -> m (Async a))
    -- ^ The output logging function
    -> m ExitCode
docs openDocs packageName extraArgs =
    let openFlag = [ "--open" | openDocs ]
        args     = case packageName of
            Nothing      -> openFlag <> extraArgs
            Just ""      -> openFlag <> extraArgs
            Just package -> openFlag <> [package] <> extraArgs
    in  run "stack" ("haddock" : args)
