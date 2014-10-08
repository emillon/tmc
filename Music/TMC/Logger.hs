-- | Something to log messages with a level, because other libs are way too
-- overengineered.

module Music.TMC.Logger
    ( LogLevel(..)
    , moreVerbose
    , MonadLogger(..)
    , noticeM
    , infoMsg
    , isLoggerActive
    ) where

import Control.Monad.Reader

-- | How important is a message.
data LogLevel = LogDebug
              | LogInfo
              | LogNotice
              | LogWarning
              | LogError
    deriving (Eq, Ord)

-- | Return the 'LogLevel' that is just more verbose.
moreVerbose :: LogLevel -> LogLevel
moreVerbose LogDebug = LogDebug
moreVerbose LogInfo = LogDebug
moreVerbose LogNotice = LogInfo
moreVerbose LogWarning = LogNotice
moreVerbose LogError = LogWarning

-- | Monads that can log stuff.
class Monad m => MonadLogger m where
    getLogLevel :: m LogLevel

-- | Determine if the current level is lower than this level.
isLoggerActive :: MonadLogger m => LogLevel -> m Bool
isLoggerActive targetLevel = do
    ll <- getLogLevel
    return $ ll <= targetLevel

-- | Log a message at the 'LogNotice' level.
noticeM :: (MonadLogger m, MonadIO m) => String -> m ()
noticeM msg = do
    act <- isLoggerActive LogNotice
    when act $ liftIO $ putStrLn msg

-- | Build an optional message. Empty if log level is above 'LogInfo'.
infoMsg :: MonadLogger m => String -> m String
infoMsg msg = do
    act <- isLoggerActive LogInfo
    return $ if act then msg else ""
