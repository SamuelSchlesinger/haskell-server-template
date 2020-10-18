{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Server.Config
( Config(..)
, HTTPConfig(..)
, TLSConfig(..)
, LogConfig(..)
, testConfig
, toLogLevel
) where

import Server.Prelude

import Server.Config.HTTP
import Server.Config.TLS
import Server.Config.Log

-- | The configuration for our server. This will grow to include all of the
-- values not baked into the server.
data Config = Config
  { httpConfig :: HTTPConfig
    -- ^ The configuration of the HTTP server
  , tlsConfig :: Maybe TLSConfig
    -- ^ The configuration for TLS, if present
  , logConfig :: LogConfig
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON Config

-- | A reasonable 'Config' to use during testing of any kind.
testConfig :: Config
testConfig = Config
  { httpConfig = HTTPConfig
    { port = Port 8080
    , host = "localhost"
    }
  , tlsConfig = Nothing
  , logConfig = LogConfig
    { minLogLevel = Error
    }
  }
