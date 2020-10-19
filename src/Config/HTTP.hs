{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{- |
Module: Config.HTTP
Description: Our server's HTTP configuration.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Config.HTTP
( HTTPConfig(..)
) where

-- | A sub-configuration for the HTTP server settings.
data HTTPConfig = HTTPConfig
  { port :: Port
    -- ^ The port to bind the server to
  , host :: Host
    -- ^ The host to bind the server to
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON HTTPConfig
