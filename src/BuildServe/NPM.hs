{-# LANGUAGE OverloadedStrings #-}
{-| Runner functions for Clients that use NPM.

You'll probably want to use a qualified import for this module:


> import qualified BuildServe.NPM as NPM
>
> buildClient = NPM.install "./client/" >> NPM.script "build" [] "./client/"


-}
module BuildServe.NPM
    ( install
    , script
    , exec
    )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           UnliftIO                       ( MonadUnliftIO )
import           System.Exit                    ( ExitCode(..) )

import           BuildServe.Logging             ( HasLogQueue
                                                , logClientOutput
                                                )
import           BuildServe.Runner              ( run
                                                , installDependency
                                                )


-- | Install the NPM dependencies specified in the @package.json@.
--
-- Essentially, @npm install@.
install
    :: (MonadUnliftIO m, HasLogQueue m)
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
-- Essentially, @npm run \<cmd> -- \<args>@.
script
    :: (MonadUnliftIO m, HasLogQueue m)
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
-- Essentially, @npx \<cmd> -- \<args>@.
exec
    :: (MonadUnliftIO m, HasLogQueue m)
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
