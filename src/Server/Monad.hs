{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module Server.Monad where

import Server.Prelude

import Server.Config (Config(..), LogConfig(..), toLogLevel)

import Control.Monad.Except (ExceptT(..))
import System.Log.FastLogger.LoggerSet (LoggerSet, newStderrLoggerSet, pushLogStr)
import Control.Monad.Logger.CallStack (defaultLogStr, toLogStr)
import qualified Servant.Server as Servant

-- | The 'Context' is the stuff that we will construct from the 'Config' and
-- use throughout our 'App' logic. This may include a database connection pool,
-- a client to some service, or an in-memory data structure, etc.
data Context = Context
  { config :: Config
  , loggerSet :: LoggerSet
  }

-- | Create the 'Context' from the 'Config'.
createContext :: Config -> IO Context
createContext config = do
  loggerSet <- newStderrLoggerSet 4096
  pure Context
    { config
    , loggerSet
    }

-- | The monad in which our server's logic will take place.
newtype App x = App
  { unApp :: Context -> IO x
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT Context IO

-- | A function which dispatches our 'App' monad given some 'Context'.
runApp :: Context -> App x -> Servant.Handler x
runApp ctx app = do
  Servant.Handler $ ExceptT $ liftIO $ catches (fmap Right $ unApp app ctx)
    [ Handler \(e :: Servant.ServerError) -> pure (Left e)
    , Handler \(_ :: SomeException) -> pure (Left Servant.err500)
    ]

-- | Retrieve the 'Context' in the 'App' monad.
context :: App Context
context = App pure

instance MonadLogger App where
  monadLoggerLog loc logSource logLevel msg = do
    ctx <- context
    if logLevel >= toLogLevel (ctx & config & logConfig & minLogLevel) then
      defaultLogStr loc logSource logLevel (toLogStr msg)
        & pushLogStr (ctx & loggerSet)
        & liftIO
    else pure ()

instance MonadLoggerIO App where
  askLoggerIO = do
    ls <- fmap loggerSet $ context
    pure (\loc logSource logLevel msg -> pushLogStr ls $ defaultLogStr loc logSource logLevel msg)
