{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{- |
Module: Context
Description: The context which our server will maintain, and a monad which is a reader on that context.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Context
( Context(..)
, EKGContext(..)
, createContext
, Config(..)
, App
, runApp
, ioToHandler
, later
) where

import Config (Config(..), LogConfig(..), toLogLevel, EKGConfig(..), HTTPConfig(..))

import Control.Monad.Except (ExceptT(..), lift)
import System.Log.FastLogger.LoggerSet (LoggerSet, newStderrLoggerSet, pushLogStr)
import Control.Monad.Logger.CallStack (defaultLogStr, toLogStr)
import Control.Concurrent.Actor (Actor, act, receive, send, hoistActionT)
import qualified System.Remote.Monitoring as EKG
import qualified System.Remote.Counter as EKG
import qualified Servant.Server as Servant
import qualified Data.HashMap.Strict as HashMap

-- | The 'Context' is the stuff that we will construct from the 'Config' and
-- use throughout our 'App' logic. This may include a database connection pool,
-- a client to some service, or an in-memory data structure, etc.
data Context = Context
  { config :: Config
  , loggerSet :: LoggerSet
  , ekgContext :: Maybe EKGContext
  , laterQueue :: Actor (IO ())
  }
  
data EKGContext = EKGContext
  { ekgServer :: EKG.Server
  , ekgEndpointCounters :: MVar (HashMap Text EKG.Counter)
  }

-- | Create the 'Context' from the 'Config'.
createContext :: Config -> IO Context
createContext config = do
  loggerSet <- newStderrLoggerSet 4096
  ekgContext <- case config & ekgConfig of
    Nothing -> pure Nothing
    Just EKGConfig{ ekgHTTPConfig } -> do
      ekgServer <- EKG.forkServer (hostByteString $ ekgHTTPConfig & host) (portInt $ ekgHTTPConfig & port)
      ekgEndpointCounters <- newMVar HashMap.empty
      pure . Just $ EKGContext
        { ekgServer
        , ekgEndpointCounters
        }
  let
    ctx = Context
      { config
      , loggerSet
      , ekgContext
      , laterQueue = error "The later queue worker must not send itself messages"
      }
  laterQueue <- act . hoistActionT (runApp ctx) . forever $ do
    try @_ @SomeException (receive liftIO) >>= either (lift . logDebug . pack . show) pure
  pure ctx { laterQueue = laterQueue }

-- | The monad in which our server's logic will take place.
newtype App x = App
  { unApp :: Context -> IO x
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT Context IO

-- | A transformation from 'IO' into 'Servant.Handler'.
ioToHandler :: IO x -> Servant.Handler x
ioToHandler io =
  Servant.Handler $ ExceptT $ liftIO $ catches (fmap Right io)
    [ Handler \(e :: Servant.ServerError) -> pure (Left e)
    , Handler \(_ :: SomeException) -> pure (Left Servant.err500)
    ]

-- | A function which dispatches our 'App' monad given some 'Context'.
runApp :: Context -> App x -> IO x
runApp ctx app = unApp app ctx

-- | Retrieve the 'Context' in the 'App' monad.
context :: App Context
context = App pure

-- | Sends an 'IO' action into the queue to be done at a leisurely pace.
later :: App () -> App ()
later activity = do
  ctx <- context
  atomically (send (ctx & laterQueue) (unApp activity ctx))

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
