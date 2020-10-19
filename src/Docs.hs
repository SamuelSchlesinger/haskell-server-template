{- |
Module: Docs
Description: Automatically generate documentation for our API.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Docs where

import API

import Servant.Docs

-- | Prints a markdown documentation text automatically generated for the
-- servant 'API'.
main :: IO ()
main = putStrLn (pack apiString)

-- | The markdown documentation text automatically generated for the
-- servant 'API'.
apiString :: String
apiString = markdown (docs theAPI)
