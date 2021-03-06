{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{- |
Module: Entrypoint
Description: The entrypoint for our server.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Entrypoint
( main
) where

import Config (Config(..), HTTPConfig(..), TLSConfig(..), readConfigFile)
import Server (createApplication)
import Context (createContext)
import qualified Docs as Docs

import Options.Commander (command_, toplevel, optDef, raw, sub, (<+>), description, annotated)
import qualified Data.Text
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

-- | The entry point for our server.
main :: IO ()
main = command_ . toplevel @"server" $
  sub @"run" (description @"runs the server using the configuration provided" runServer)
  <+> sub @"docs" (description @"prints out documentation for the server" $ raw Docs.main)
  <+> raw (putStrLn "Try: server help") where
  runServer =
    annotated @"filepath of configuration, defaults to config.json"
    . optDef @"config" @"configuration-file" (FilePath "config.json") $ \configFilePath -> raw $ do
    -- Parse our configuration from the filepath provided by the user, or
    -- the default if none was provided.
    eitherConfig <- readConfigFile configFilePath
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
        run warpSettings =<< createApplication context
