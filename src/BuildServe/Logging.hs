{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Logging-related types & functions for build scripts.

Child processes will dump their output into a 'TQueue'. The queue should be
read from a separate thread which is responsible for printing messages to
the console or rendering them in a UI.

-}
module BuildServe.Logging
    ( -- * Logging Types
      LogQueue
    , newLogQueue
    , LogMessage(..)
    , LogType(..)
    , maximumLogTypeLength
    , paddingLength
    , HasLogQueue(..)
      -- * Logging Messages
    , logMessage
    , logHandle
    , logServerOutput
    , logClientOutput
    , logExitStatus
      -- * Printing Logs
    , printLogMessage
    , forkOutputLogger
    )
where

import           Control.Concurrent.Async.Lifted.Safe
                                                ( Async
                                                , Forall
                                                , Pure
                                                , async
                                                )
import           Control.Concurrent.STM         ( atomically )
import           Control.Concurrent.STM.TQueue  ( TQueue
                                                , newTQueue
                                                , readTQueue
                                                , writeTQueue
                                                )
import           Control.Monad                  ( when
                                                , forever
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Control    ( MonadBaseControl
                                                , control
                                                , StM
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           GHC.IO.Handle                  ( Handle
                                                , hIsEOF
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
newLogQueue :: MonadIO m => m LogQueue
newLogQueue = liftIO $ atomically newTQueue


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


-- | A monad with a LogQueue in it's context.
class Monad m => HasLogQueue m where
    -- | Get the 'LogQueue'
    getLogQueue :: m LogQueue



-- LOGGING

-- | Add a message to the 'LogQueue'.
logMessage :: (MonadIO m, HasLogQueue m) => LogType -> Text -> m ()
logMessage logType message = do
    queue <- getLogQueue
    liftIO . atomically . writeTQueue queue $ LogMessage
        { lmType    = logType
        , lmMessage = message
        }

-- | Log each line of output from a 'Handle' with the given 'LogType' until
-- the handle is closed..
logHandle
    :: ( MonadBaseControl IO m
       , Forall (Pure m)
       , MonadIO m
       , HasLogQueue m
       , StM m () ~ ()
       )
    => LogType
    -> Handle
    -> m (Async ())
logHandle logType handle =
    async $ control $ \runInBase -> whileM_ (not <$> hIsEOF handle) $ do
        line <- T.hGetLine handle
        runInBase $ logMessage logType line
  where
    whileM_ :: Monad m => m Bool -> m () -> m ()
    whileM_ predicateM actionM = do
        predicate <- predicateM
        when predicate $ actionM >> whileM_ predicateM actionM

-- | Log output from a 'Server' 'Handle'.
logServerOutput
    :: ( MonadBaseControl IO m
       , Forall (Pure m)
       , MonadIO m
       , HasLogQueue m
       , StM m () ~ ()
       )
    => Handle
    -> m (Async ())
logServerOutput = logHandle Server

-- | Log output from a 'Client' 'Handle'.
logClientOutput
    :: ( MonadBaseControl IO m
       , Forall (Pure m)
       , MonadIO m
       , HasLogQueue m
       , StM m () ~ ()
       )
    => Handle
    -> m (Async ())
logClientOutput = logHandle Client


-- | Depending on the ExitCode of a command, either log some success text
-- or failure text.
logExitStatus
    :: (MonadIO m, HasLogQueue m)
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
printLogMessage :: MonadIO m => LogQueue -> m ()
printLogMessage q = do
    msg <- liftIO . atomically $ readTQueue q
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
    :: (MonadBaseControl IO m, Forall (Pure m), HasLogQueue m, MonadIO m)
    => m (Async ())
forkOutputLogger = do
    queue <- getLogQueue
    async $ forever $ printLogMessage queue
