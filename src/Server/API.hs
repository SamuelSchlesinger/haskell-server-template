{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server.API
( theAPI
, API
, Health
, Ready
, (:<|>)(..)
) where

import Servant.API (GetNoContent, Description, Summary, (:>), (:<|>)(..))
import Server.Prelude


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
  Description
    "Check the health of the server. \
    \The server will reply with a 204 status code if it is live. \
    \This form of health means that we should not restart this service, though \
    \it may not be available for all functionality."
  :> Summary
    "Check the health of the server."
  :> "health" :> GetNoContent

-- | A readiness check. This could, in a more complete application, make
-- sure we can connect to the database or some cache server, or some other
-- application specific service.
type Ready =
  Description
    "Check the readiness of the server. \
    \The server will reply with a 204 status code if it is ready to receive \
    \traffic."
  :> Summary
    "Check the readiness of the server."
  :> "ready" :> GetNoContent
