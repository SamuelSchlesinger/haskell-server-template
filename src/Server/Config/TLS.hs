{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{- |
Module: Server.Config.TLS
Description: Our server's TLS configuration.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Server.Config.TLS
( TLSConfig(..)
) where

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
