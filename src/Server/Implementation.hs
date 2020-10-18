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
server = health :<|> ready

-- | The implementation of the 'Health' endpoint.
health :: ServerT Health App
health = do
  logDebug "Checking server health"
  pure NoContent

ready :: ServerT Ready App
ready = do
  logDebug "Checking server readiness"
  pure NoContent
