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

import Servant.Docs
import API
import qualified System.IO

-- | Prints a markdown documentation text automatically generated for the
-- servant 'API'.
main :: IO ()
main = System.IO.putStrLn apiString

-- | The markdown documentation text automatically generated for the
-- servant 'API'.
apiString :: String
apiString = markdown (docs theAPI)
