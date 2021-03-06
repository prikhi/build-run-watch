{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Logging-related types & functions for build scripts.

Child processes will dump their output into a 'TQueue'. The queue should be
read from a separate thread which is responsible for printing messages to
the console or rendering them in a UI.

-}
module BuildRunWatch.Logging
    ( -- * Logging Types
      LogQueue
    , newLogQueue
    , LogMessage(..)
    , LogType(..)
    , maximumLogTypeLength
    , paddingLength
    , HasLogQueue(..)
      -- * Logging Messages
    , MonadLoggable(..)
    , logHandle
    , logServerOutput
    , logClientOutput
    , logExitStatus
      -- * Printing Logs
    , printLogMessage
    , forkOutputLogger
    )
where

import           Control.Monad                  ( when
                                                , forever
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           GHC.IO.Handle                  ( Handle
                                                , hIsEOF
                                                )
import           UnliftIO                       ( MonadUnliftIO
                                                , withRunInIO
                                                , liftIO
                                                )
import           UnliftIO.Async                 ( Async
                                                , async
                                                )
import           UnliftIO.STM                   ( atomically
                                                , TQueue
                                                , newTQueue
                                                , readTQueue
                                                , writeTQueue
                                                )
import           System.Console.ANSI            ( Color(..)
                                                , ColorIntensity(Vivid)
                                                , ConsoleIntensity
                                                    ( BoldIntensity
                                                    )
                                                , ConsoleLayer(Foreground)
                                                , SGR(..)
                                                , setSGR
                                                )
import           System.Exit                    ( ExitCode(..) )


-- TYPES

-- | A queue for scripts to dump messages into.
type LogQueue = TQueue LogMessage

-- | Create a new 'LogQueue'.
newLogQueue :: MonadUnliftIO m => m LogQueue
newLogQueue = atomically newTQueue


-- | A message along with it's logging type.
data LogMessage
    = LogMessage
        { lmType :: LogType
        , lmMessage :: Text
        }
    deriving (Show, Read)


-- | The source or status of a log message.
data LogType
    = Server
    -- ^ Server Output
    | Client
    -- ^ Client Output
    | Info
    -- ^ General Info
    | Failure
    -- ^ Command Failure
    | Success
    -- ^ Command Success
    deriving (Show, Read, Eq, Enum, Bounded)

-- | The maximum size of a rendered 'LogType'.
maximumLogTypeLength :: Int
maximumLogTypeLength =
    maximum $ map (length . show) [minBound .. maxBound :: LogType]

-- | The amount of padding a LogType needs to reach the
-- 'maximumLogTypeLength'.
paddingLength :: LogType -> Int
paddingLength logType = maximumLogTypeLength - length (show logType)


-- | Grab a 'LogQueue' from some environment.
class HasLogQueue env where
    -- | Get the 'LogQueue'
    getLogQueue :: env -> LogQueue

instance HasLogQueue (TQueue LogMessage) where
    getLogQueue = id


-- | A monad with the ability to log messages.
class Monad m => MonadLoggable m where
    -- | Log a message tagged with a 'LogType'.
    logMessage :: LogType -> Text -> m ()

-- | Write the message to a 'LogQueue'.
instance (MonadUnliftIO m, HasLogQueue env) => MonadLoggable (ReaderT env m) where
    logMessage logType message = do
        queue <- asks getLogQueue
        atomically . writeTQueue queue $ LogMessage
            { lmType    = logType
            , lmMessage = message
            }


-- LOGGING

-- | Log each line of output from a 'Handle' with the given 'LogType' until
-- the handle is closed.
logHandle
    :: (MonadUnliftIO m, MonadLoggable m) => LogType -> Handle -> m (Async ())
logHandle logType handle =
    async $ withRunInIO $ \runner -> whileM_ (not <$> hIsEOF handle) $ do
        line <- T.hGetLine handle
        runner $ logMessage logType line
  where
    whileM_ :: Monad m => m Bool -> m () -> m ()
    whileM_ predicateM actionM = do
        predicate <- predicateM
        when predicate $ actionM >> whileM_ predicateM actionM

-- | Log output from a 'Server' 'Handle'.
logServerOutput :: (MonadUnliftIO m, MonadLoggable m) => Handle -> m (Async ())
logServerOutput = logHandle Server

-- | Log output from a 'Client' 'Handle'.
logClientOutput :: (MonadUnliftIO m, MonadLoggable m) => Handle -> m (Async ())
logClientOutput = logHandle Client


-- | Depending on the ExitCode of a command, either log some success text
-- or failure text.
logExitStatus
    :: MonadLoggable m
    => Text
    -- ^ Success Message
    -> Text
    -- ^ Failure Message
    -> ExitCode
    -> m ()
logExitStatus successText failureText exitCode = case exitCode of
    ExitSuccess   -> logMessage Success successText
    ExitFailure _ -> logMessage Failure failureText



-- OUTPUT

-- | Read the next 'LogMessage' from the queue, format it, & print it to
-- stdout.
--
-- The current format is @[LOGTYPE] LOGMESSAGE@, where the 'LogType' is
-- bolded and colored.
--
-- TODO: Config value for custom formatting function.
printLogMessage :: MonadUnliftIO m => LogQueue -> m ()
printLogMessage q = do
    msg <- atomically $ readTQueue q
    liftIO $ do
        setSGR [Reset]
        T.putStr "["
        setSGR
            [ SetConsoleIntensity BoldIntensity
            , SetColor Foreground Vivid $ color msg
            ]
        T.putStr . text $ lmMessage msg
        setSGR [Reset]
        T.putStr $ "]" <> T.replicate (paddingLength $ lmType msg) " "
        T.putStrLn $ lmMessage msg
  where
    color :: LogMessage -> Color
    color msg = case lmType msg of
        Info    -> Blue
        Success -> Green
        Failure -> Red
        Client  -> Cyan
        Server  -> Magenta
    text :: Text -> Text
    text = T.toUpper . T.pack . show


-- | Forks an Async computation that constantly reads from the 'LogQueue'
-- and prints messages to stdout.
forkOutputLogger
    :: (MonadUnliftIO m, MonadReader env m, HasLogQueue env) => m (Async ())
forkOutputLogger = do
    queue <- asks getLogQueue
    async $ forever $ printLogMessage queue
