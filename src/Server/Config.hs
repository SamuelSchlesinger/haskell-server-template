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
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON Config

-- | A sub-configuration for the HTTP server settings.
data HTTPConfig = HTTPConfig
  { port :: Port
  , host :: Host
  , tlsConfig  :: Maybe TLSConfig
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON HTTPConfig

-- | A sub-configuration for the TLS configuration. 
data TLSConfig = TLSConfig
  { certificate :: FilePath
  , key :: FilePath
  , chain :: Maybe [FilePath]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON TLSConfig
