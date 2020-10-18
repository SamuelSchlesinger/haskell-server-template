{-# LANGUAGE BlockArguments #-}
module Main where

import Server.Prelude
import Test.Hspec

import Server.Monad (createContext, unApp)
import Server.Config (testConfig)
import Server.Implementation (health, ready)
import Server.API (NoContent(..))

main :: IO ()
main = do
  context <- createContext testConfig
  hspec do
    describe "server" do
      it "is ready" do
        flip unApp context health `shouldReturn` NoContent
      it "is healthy" do
        flip unApp context ready `shouldReturn` NoContent

