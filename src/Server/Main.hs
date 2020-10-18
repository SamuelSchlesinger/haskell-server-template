{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
module Server.Main
( main
, createApplication
) where

import Server.Prelude

import Server.Config (Config(..), HTTPConfig(..), TLSConfig(..))
import Server.Implementation (server, theAPI)
import Server.Monad (createContext, runApp, Context)

import Options.Commander (command_, toplevel, optDef, raw)
import Data.Aeson (eitherDecodeFileStrict')
import Network.Wai (Middleware, Application)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger, outputFormat, OutputFormat(CustomOutputFormatWithDetailsAndHeaders))
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSONWithHeaders)
import Network.Wai.Middleware.Autohead (autohead)
import Data.Default (def)
import Servant (serve, hoistServer)
import qualified Data.Text
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

-- | The entry point for our server.
main :: IO ()
main = command_ . toplevel @"server" $ program where
  program = optDef @"config" @"configuration-file" (FilePath "config.json") $ \configFilePath -> raw $ do
    -- Parse our configuration from the filepath provided by the user, or
    -- the default if none was provided.
    eitherConfig <- eitherDecodeFileStrict' (filePathString configFilePath)
    case eitherConfig of
      -- If we cannot parse our config, tell the user and show the error
      -- from aeson.
      Left err -> do
        putStrLn $ "Could not parse configuration from " <> unFilePath configFilePath
        putStrLn $ Data.Text.pack err
      -- If we can parse our config, use it to create context to interpret
      -- our server logic as well as construct the HTTP server runner,
      -- either using TLS or not.
      Right config -> do
        let
          run = case config & tlsConfig of
            Nothing -> Warp.runSettings
            Just tls -> WarpTLS.runTLS
              ( case tls & chain of
                  Just certificateChain -> WarpTLS.tlsSettingsChain
                    (filePathString (tls & certificate))
                    (fmap filePathString certificateChain)
                    (filePathString (tls & key))
                  Nothing -> WarpTLS.tlsSettings
                    (filePathString (tls & certificate))
                    (filePathString (tls & key))
              )
          warpSettings = Warp.defaultSettings
            & Warp.setPort
                (portInt (config & httpConfig & port))
            & Warp.setHost
                (hostPreference (config & httpConfig & host))
        context <- createContext config
        middleware <- createMiddleware config  
        run warpSettings (createApplication middleware context)

createApplication :: Middleware -> Context -> Application
createApplication middleware context = middleware . serve theAPI $ hoistServer theAPI (runApp context) server

createMiddleware :: Config -> IO Middleware
createMiddleware _config = do
  requestLogger <- mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders formatAsJSONWithHeaders                
    }
  pure $ requestLogger . autohead
