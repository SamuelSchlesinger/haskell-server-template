{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{- |
Module: Config.Log
Description: Our server's logging configuration.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Config.Log
( LogConfig(..)
, MinLogLevel(..)
, toLogLevel
) where

import Control.Monad.Logger (LogLevel(..))

-- | A sub-configuration for the server logging settings.
data LogConfig = LogConfig
  { minLogLevel :: MinLogLevel
    -- ^ The host to bind the server to
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON LogConfig

-- | The various log levels we can set as our minimum acceptable one.
data MinLogLevel = Debug | Info | Warn | Error
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via GenericJSON MinLogLevel

-- | Convert a 'MinLogLevel', which we use only for 'LogConfig', into a
-- 'LogLevel', which is used by 'MonadLogger'.
toLogLevel :: MinLogLevel -> LogLevel
toLogLevel Debug = LevelDebug
toLogLevel Info = LevelInfo
toLogLevel Warn = LevelWarn
toLogLevel Error = LevelError
