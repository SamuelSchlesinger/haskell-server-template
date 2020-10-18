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
) where

import Server.Prelude

import Server.Config.HTTP
import Server.Config.TLS

-- | The configuration for our server. This will grow to include all of the
-- values not baked into the server.
data Config = Config
  { httpConfig :: HTTPConfig
    -- ^ The configuration of the HTTP server
  , tlsConfig  :: Maybe TLSConfig
    -- ^ The configuration for TLS, if present
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON Config
