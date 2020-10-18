{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
module Server.Config.Log
( LogConfig(..)
, MinLogLevel(..)
, toLogLevel
) where

import Server.Prelude

import Control.Monad.Logger (LogLevel(..))

-- | A sub-configuration for the server logging settings.
data LogConfig = LogConfig
  { minLogLevel :: MinLogLevel
    -- ^ The host to bind the server to
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON LogConfig

data MinLogLevel = Debug | Info | Warn | Error
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON MinLogLevel

toLogLevel :: MinLogLevel -> LogLevel
toLogLevel Debug = LevelDebug
toLogLevel Info = LevelInfo
toLogLevel Warn = LevelWarn
toLogLevel Error = LevelError
