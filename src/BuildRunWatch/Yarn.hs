{-# LANGUAGE OverloadedStrings #-}
{-| Runner functions for Clients that use Yarn.

You'll probably want to use a qualified import for this module:

> import qualified BuildRunWatch.Yarn as Yarn
>
> buildClient = Yarn.install "./client/" >> Yarn.script "build" [] "./client/"

-}
module BuildRunWatch.Yarn
    ( install
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


-- | Install the Yarn dependencies specified in the @package.json@.
--
-- Analogous to @yarn install@.
install
    :: (MonadUnliftIO m, MonadLoggable m)
    => FilePath
    -- ^ The client project's working directory
    -> m ()
install workingDir = installDependency "yarn"
                                       ["install"]
                                       workingDir
                                       logClientOutput
                                       "Yarn Dependencies"


-- | Run a binary installed with Yarn or a script defined in
-- @packages.json@.
--
-- Analogous to @yarn \<cmd> -- \<args>@.
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
    in  run "yarn" args workingDir logClientOutput
