{-# LANGUAGE OverloadedStrings #-}
{-| Runner functions for Clients that use NPM.

You'll probably want to use a qualified import for this module:

> import qualified BuildRunWatch.NPM as NPM
>
> buildClient = NPM.install "./client/" >> NPM.script "build" [] "./client/"

-}
module BuildRunWatch.NPM
    ( install
    , script
    , exec
    )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           UnliftIO                       ( MonadUnliftIO )
import           System.Exit                    ( ExitCode(..) )

import           BuildRunWatch.Logging          ( MonadLoggable
                                                , logClientOutput
                                                )
import           BuildRunWatch.Runner           ( run
                                                , installDependency
                                                )


-- | Install the NPM dependencies specified in the @package.json@.
--
-- Analogous to @npm install@.
install
    :: (MonadUnliftIO m, MonadLoggable m)
    => FilePath
    -- ^ The client project's working directory
    -> m ()
install workingDir = installDependency "npm"
                                       ["install"]
                                       workingDir
                                       logClientOutput
                                       "NPM Dependencies"


-- | Run one of the @scripts@ specified in the @package.json@ file.
--
-- Analogous to @npm run \<cmd> -- \<args>@.
script
    :: (MonadUnliftIO m, MonadLoggable m)
    => Text
    -- ^ The script to run
    -> [Text]
    -- ^ Any arguments to pass to the script
    -> FilePath
    -- ^ The client project's working directory
    -> m ExitCode
script cmd rawArgs workingDir =
    let args = if null rawArgs
            then ["run", T.unpack cmd]
            else map T.unpack $ "run" : cmd : "--" : rawArgs
    in  run "npm" args workingDir logClientOutput


-- | Run a binary installed with NPM.
--
-- Analogous to @npx \<cmd> -- \<args>@.
exec
    :: (MonadUnliftIO m, MonadLoggable m)
    => Text
    -- ^ The command to run
    -> [Text]
    -- ^ Any arguments to pass to the command
    -> FilePath
    -- ^ The client project's working directory
    -> m ExitCode
exec cmd rawArgs workingDir =
    let args = if null rawArgs
            then [T.unpack cmd]
            else map T.unpack $ cmd : "--" : rawArgs
    in  run "npx" args workingDir logClientOutput
