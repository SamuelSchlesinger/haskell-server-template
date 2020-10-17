{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server.API
( theAPI
, API
, Healthz
) where

import Servant.API (GetNoContent, (:>))
import Server.Prelude

-- | A 'Proxy' of our 'API'. Due to servant's design, this is helpful to
-- export from here.
theAPI :: Proxy API
theAPI = Proxy

-- | The API of our service as a Haskell type, currently consisting of
-- just the 'Healthz' endpoint.
type API =
    Healthz

-- | An individual health check endpoint.
type Healthz = "healthz" :> GetNoContent
