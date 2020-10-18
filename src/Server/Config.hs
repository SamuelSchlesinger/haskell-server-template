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

-- | The configuration for our server. This will grow to include all of the
-- values not baked into the server.
data Config = Config
  { httpConfig :: HTTPConfig
    -- ^ The configuration of the HTTP server
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON Config

-- | A sub-configuration for the HTTP server settings.
data HTTPConfig = HTTPConfig
  { port :: Port
    -- ^ The port to bind the server to
  , host :: Host
    -- ^ The host to bind the server to
  , tlsConfig  :: Maybe TLSConfig
    -- ^ The configuration for TLS, if present
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON HTTPConfig

-- | A sub-configuration for the TLS configuration. 
data TLSConfig = TLSConfig
  { certificate :: FilePath
    -- ^ The filepath where we've stored the server's certificate
  , key :: FilePath
    -- ^ The filepath where we've stored the server's private key
  , chain :: Maybe [FilePath]
    -- ^ A certificate chain
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON TLSConfig
