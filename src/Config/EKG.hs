{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{- |
Module: Config.EKG
Description: Our server's EKG configuration.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows

EKG is a monitoring server for arbitrary Haskell processes.
-}
module Config.EKG
( EKGConfig(..)
) where

import Config.HTTP

-- | A sub-configuration for the EKG server settings.
data EKGConfig = EKGConfig
  { ekgHTTPConfig :: HTTPConfig
    -- ^ The HTTP configuration for the EKG server
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON EKGConfig
