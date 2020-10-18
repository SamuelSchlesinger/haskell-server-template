module Server.Docs where

import Servant.Docs
import Server.API

main :: IO ()
main = putStrLn (markdown (docs theAPI))
