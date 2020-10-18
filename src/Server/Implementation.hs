{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
{- |
Module: Server.Implementation
Description: The implementation of our server's endpoints.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Server.Implementation
( theAPI
, server
, health
, ready
, createApplication
, createMiddleware
) where

import Server.Prelude
import Server.API

import Server.Context (App, Context(..), runApp, EKGContext(..), ioToHandler)
import Servant (ServerT, serve, hoistServer)
import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSONWithHeaders)
import Network.Wai.Middleware.RequestLogger (outputFormat, mkRequestLogger, OutputFormat(CustomOutputFormatWithDetailsAndHeaders))
import Network.Wai.Middleware.Autohead (autohead)
import qualified Network.Wai as Wai
import qualified Data.HashMap.Strict as HashMap
import qualified System.Remote.Counter as EKG
import qualified System.Remote.Monitoring as EKG
import qualified Data.Text

-- | The actual implementation of the 'API' as a servant server.
server :: ServerT API App
server =
  health
  :<|> ready

-- | The implementation of the health check. We are considered live if we
-- can respond to this message.
health :: ServerT Health App
health = do
  logDebug "Checking server health"
  pure NoContent

-- | Implementation of the readiness check. We are considered live if we
-- can respond to this message, but in a more complex app we may need to
-- have populated some in-memory cache or be able to connect to some
-- external service.
ready :: ServerT Ready App
ready = do
  logDebug "Checking server readiness"
  pure NoContent

-- | Create the 'Application' given the 'Context'.
createApplication :: Context -> Application
createApplication context = serve theAPI $ hoistServer theAPI (ioToHandler . runApp context) server

-- | Create the 'Middleware' given the 'Context'.
createMiddleware :: Context -> IO Middleware
createMiddleware context = do
  requestLogger <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders formatAsJSONWithHeaders                
    }
  let
    ekgMiddleware' = case context & ekgContext of
      Nothing -> id
      Just ekgContext' -> ekgMiddleware ekgContext'
  pure $ ekgMiddleware' . requestLogger . autohead

ekgMiddleware :: EKGContext -> Middleware
ekgMiddleware ekgContext app req respond = do
  let key = Data.Text.concat ["Endpoint Counter: \"", intercalate "/" (Wai.pathInfo req), "\""]
  counters <- readMVar (ekgContext & ekgEndpointCounters)
  counter <- case HashMap.lookup key counters of
    Nothing -> modifyMVar (ekgContext & ekgEndpointCounters) \counters' -> do
      case HashMap.lookup key counters' of
        Nothing -> do
          counter <- EKG.getCounter key (ekgContext & ekgServer)
          pure (HashMap.insert key counter counters', counter)
        Just counter -> do
          pure (counters', counter)
    Just counter -> do
      pure counter
  EKG.inc counter
  app req respond
