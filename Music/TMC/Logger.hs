-- | Something to log messages with a level, because other libs are way too
-- overengineered.

module Music.TMC.Logger
    ( LogLevel(..)
    , MonadLogger(..)
    , noticeM
    ) where

import Control.Monad.Reader

-- | How important is a message.
data LogLevel = LogInfo
              | LogNotice
              | LogWarning
              | LogError
    deriving (Eq, Ord)

-- | Monads that can log stuff.
class MonadLogger m where
    getLogLevel :: m LogLevel

-- | Log a message at the 'LogNotice' level.
noticeM :: (MonadLogger m, MonadIO m) => String -> m ()
noticeM msg = do
    ll <- getLogLevel
    when (ll <= LogNotice) $ liftIO $ putStrLn msg
