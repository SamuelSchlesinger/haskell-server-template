{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
module Server.Implementation
( server
) where

import Server.Prelude
import Server.API

import Server.Monad (App)
import Servant (ServerT, NoContent(..))

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
