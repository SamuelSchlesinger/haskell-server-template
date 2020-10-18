{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
module Server.Config.TLS
( TLSConfig(..)
) where

import Server.Prelude

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
