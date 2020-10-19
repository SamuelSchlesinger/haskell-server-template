{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
Module: Prelude
Description: A collection of common imports throughout the codebase.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Prelude
  ( Text
  , intercalate
  , ToJSON(..)
  , FromJSON(..)
  , Read(..)
  , Show(..)
  , Eq(..)
  , either
  , Ord(..)
  , Enum(..)
  , Num(..)
  , Integer
  , Integral(..)
  , Real(..)
  , Fractional(..)
  , fromIntegral
  , Maybe(..)
  , Category(..)
  , Functor(..)
  , Applicative(..)
  , String
  , Monad(..)
  , Bool(..)
  , HashMap
  , ReaderT(..)
  , Proxy(..)
  , Either(..)
  , (&)
  , ($)
  , IO
  , putStrLn
  , pack
  , MonadIO(..)
  , MonadUnliftIO(..)
  , MonadLogger(..)
  , MonadLoggerIO(..)
  , logDebug
  , logError
  , logInfo
  , logWarn
  , Monoid(..)
  , Semigroup(..)
  , Generic
  , Rep
  , flip
  , Handler(..)
  , Port(Port, unPort)
  , portInt
  , Host(Host, unHost)
  , hostPreference
  , hostByteString
  , ByteString
  , LazyByteString
  , FilePath(FilePath, unFilePath)
  , filePathString
  , GenericJSON(GenericJSON, unGenericJSON)
  , Int
  , void
  , def
  , forever
  , (=<<)
  , module UnliftIO
  ) where

import Data.Default
import Control.Monad (void, forever)
import Control.Monad.Logger.CallStack (MonadLogger(..), MonadLoggerIO(..), logDebug, logWarn, logError, logInfo)
import Options.Commander (Unrender)
import Data.Proxy (Proxy(..))
import Data.Text.IO (putStrLn)
import GHC.Generics (Generic, Rep)
import Data.Text (Text, intercalate, pack)
import Data.Aeson
  ( ToJSON(..), FromJSON(..) )
import Data.String (IsString, String)
import Text.Read (Read(..))
import Text.Show (Show(..))
import Data.Eq (Eq(..))
import Data.Ord (Ord(..))
import GHC.Enum (Enum(..))
import Data.Int (Int)
import GHC.Num (Num(..))
import GHC.Integer (Integer)
import GHC.Real (Integral(..), Real(..), Fractional(..), fromIntegral)
import Data.Functor (Functor(..))
import Data.Bool (Bool(..))
import Data.Maybe (Maybe(..))
import Control.Applicative (Applicative(..))
import Control.Monad (Monad(..))
import Data.Either (Either(..), either)
--import Prelude
--  ( Eq(..), Ord(..), Read(..), Enum(..)
--  , Maybe(..), Functor(..), Bool(..), IO, Either(..)
--  , ($), Monoid(..), Semigroup(..), fromIntegral, String
--  , Monad(..), Applicative(..), flip, Int
--  )
import UnliftIO
import Data.Function
  ( (&), ($), flip )
import System.IO (IO)
import Data.Monoid (Monoid(..))
import Data.Semigroup(Semigroup(..))
import Data.Word (Word16)
import Control.Category
  ( Category(..)
  )
import Control.Monad.Reader (ReaderT(..))
import Network.Wai.Handler.Warp (HostPreference)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.HashMap.Strict (HashMap)
import Control.Monad ((=<<))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson
import qualified Data.Text
import qualified Data.String

type LazyByteString = LBS.ByteString

newtype Port = Port { unPort :: Word16 }
  deriving newtype (Show, Read, Eq, Ord, Enum, ToJSON, FromJSON)

portInt :: Port -> Int
portInt = fromIntegral . unPort

newtype Host = Host { unHost :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString, ToJSON, FromJSON)

hostByteString :: Host -> ByteString
hostByteString = encodeUtf8 . unHost

hostPreference :: Host -> HostPreference
hostPreference = Data.String.fromString . Data.Text.unpack . unHost

newtype FilePath = FilePath { unFilePath :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString, ToJSON, FromJSON, Unrender)

filePathString :: FilePath -> String 
filePathString = Data.Text.unpack . unFilePath

newtype GenericJSON a = GenericJSON { unGenericJSON :: a }

instance
  ( Generic a
  , Data.Aeson.GToJSON' Data.Aeson.Value Data.Aeson.Zero (Rep a)
  , Data.Aeson.GToJSON' Data.Aeson.Encoding Data.Aeson.Zero (Rep a)
  ) => ToJSON (GenericJSON a)
  where
  toJSON = Data.Aeson.genericToJSON serverAesonOptions . unGenericJSON
  toEncoding = Data.Aeson.genericToEncoding serverAesonOptions . unGenericJSON

instance
  ( Generic a
  , Data.Aeson.GFromJSON Data.Aeson.Zero (Rep a)
  ) => FromJSON (GenericJSON a)
  where
  parseJSON = fmap GenericJSON . Data.Aeson.genericParseJSON serverAesonOptions

serverAesonOptions :: Data.Aeson.Options
serverAesonOptions = Data.Aeson.defaultOptions
  { Data.Aeson.omitNothingFields  = True
  , Data.Aeson.sumEncoding = Data.Aeson.ObjectWithSingleField
  }
