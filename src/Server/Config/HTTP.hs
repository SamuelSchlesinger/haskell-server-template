{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
module Server.Config.HTTP
( HTTPConfig(..)
) where

import Server.Prelude

-- | A sub-configuration for the HTTP server settings.
data HTTPConfig = HTTPConfig
  { port :: Port
    -- ^ The port to bind the server to
  , host :: Host
    -- ^ The host to bind the server to
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON HTTPConfig
