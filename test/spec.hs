{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Test.Hspec

import Context (createContext, Config(ekgConfig), runApp, later)
import Config (testConfig, readConfigFile)
import Server (health, ready)
import API (NoContent(..), theAPI)
import System.FilePath.TH (fileRelativeToAbsolute)
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.Text

main :: IO ()
main = do
  ctx <- createContext testConfig { ekgConfig = Nothing }
  hspec do
    describe "endpoints" do
      it "is ready" do
        runApp ctx health `shouldReturn` NoContent
      it "is healthy" do
        runApp ctx ready `shouldReturn` NoContent
    describe "config" do
      it "can be parsed" do
        readConfigFile (FilePath . Data.Text.pack $ $(fileRelativeToAbsolute "../config.json"))
          `shouldReturn` Right testConfig 
    describe "later" do
      it "lets us postpone actions" do
        x <- newEmptyMVar
        runApp ctx (later (liftIO $ putMVar x "HELLO"))
        takeMVar x
          `shouldReturn` "HELLO"
