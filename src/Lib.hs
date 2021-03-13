module Lib
    ( runApp
    ) where

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)

runApp :: IO ()
runApp = simpleHTTP nullConf $ ok "Hello, World!"
