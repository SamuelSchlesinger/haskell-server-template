{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
Module: Server.Config
Description: Our server's configuration.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Server.Config
( Config(..)
, testConfig
, HTTPConfig(..)
, TLSConfig(..)
, LogConfig(..)
, MinLogLevel(..)
, toLogLevel
, EKGConfig(..)
) where

import Server.Prelude

import Server.Config.HTTP
import Server.Config.EKG
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
    -- ^ The configuration for our logger
  , ekgConfig :: Maybe EKGConfig
    -- ^ The configuration for EKG, if present
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON Config

-- | A reasonable 'Config' to use during testing.
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
  , ekgConfig = Nothing
  }
