{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Server.Prelude
import Test.Hspec

import Server.Monad (createContext, unApp)
import Server.Config (testConfig, readConfigFile)
import Server.Implementation (health, ready)
import Server.API (NoContent(..), theAPI)
import System.FilePath.TH (fileRelativeToAbsolute)
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.Text

main :: IO ()
main = do
  context <- createContext testConfig
  hspec do
    describe "endpoints" do
      it "is ready" do
        flip unApp context health `shouldReturn` NoContent
      it "is healthy" do
        flip unApp context ready `shouldReturn` NoContent
    describe "config" do
      it "can be parsed" do
        readConfigFile (FilePath . Data.Text.pack $ $(fileRelativeToAbsolute "../config.json"))
          `shouldReturn` Right testConfig 
