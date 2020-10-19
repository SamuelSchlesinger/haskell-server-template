{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{- |
Module: Server.API
Description: The API of our server.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Server.API
( theAPI
, API
, Health
, Ready
, (:<|>)(..)
, NoContent(..)
) where

import Servant.API (GetNoContent, Description, Summary, (:>), (:<|>)(..), NoContent(..))
import Prelude


-- | A 'Proxy' of our 'API'. Due to servant's design, this is helpful to
-- export from here.
theAPI :: Proxy API
theAPI = Proxy

-- | The API of our service as a Haskell type, currently consisting of
-- just the 'Health' endpoint.
type API =
  Health
  :<|> Ready

-- | Checks whether or not the service is alive.
type Health =
  Summary
    "Check the health of the server."
  :> Description
    "The server will reply with a 204 status code if it is live. \
    \This status code means that we should not restart this service, though \
    \it may not be yet available for all functionality."
  :> "health" :> GetNoContent

-- | A readiness check. This could, in a more complete application, make
-- sure we can connect to the database or some cache server, or some other
-- application specific service.
type Ready =
  Summary
    "Check the readiness of the server."
  :> Description
    "The server will reply with a 204 status code if it is ready to receive \
    \traffic."
  :> "ready" :> GetNoContent
