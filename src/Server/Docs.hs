{- |
Module: Server.Docs
Description: Automatically generate documentation for our API.
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Server.Docs where

import Servant.Docs
import Server.API

-- | Prints a markdown documentation text automatically generated for the
-- servant 'API'.
main :: IO ()
main = putStrLn apiString

-- | The markdown documentation text automatically generated for the
-- servant 'API'.
apiString :: String
apiString = markdown (docs theAPI)
