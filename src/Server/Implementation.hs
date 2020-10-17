{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
module Server.Implementation
( theAPI
, server
, createContext
, runApp
) where

import Server.Prelude

import Server.API (API, Healthz, theAPI)
import Server.Config (Config)
import Servant (ServerT, NoContent(..))
import Control.Monad.Except (ExceptT(..))
import qualified Servant.Server as Servant

-- | The 'Context' is the stuff that we will construct from the 'Config' and
-- use throughout our 'App' logic. This may include a database connection pool,
-- a client to some service, or an in-memory data structure, etc.
data Context = Context
  { contextConfig :: Config
  }

-- | Create the 'Context' from the 'Config'. Right now, this doesn't have to be
-- in 'IO', but it 
createContext :: Config -> IO Context
createContext config = pure $ Context
  { contextConfig = config
  }

-- | The monad in which our server's logic will take place.
newtype App x = App
  { unApp :: Context -> IO x
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT Context IO

-- | A function which dispatches our 'App' monad given some 'Context'.
runApp :: Context -> App x -> Servant.Handler x
runApp ctx app = do
  Servant.Handler $ ExceptT $ liftIO $ catches (fmap Right $ unApp app ctx)
    [ Handler \(e :: Servant.ServerError) -> pure (Left e)
    , Handler \(_ :: SomeException) -> pure (Left Servant.err500)
    ]

-- | The actual implementation of the 'API' as a servant server.
server :: ServerT API App
server = healthz

-- | The implementation of the 'Healthz' endpoint.
healthz :: ServerT Healthz App
healthz = pure NoContent
