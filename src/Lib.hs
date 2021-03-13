module Lib
    ( runApp
    ) where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server (anyPath, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse,  method, ok, dir, Method(GET), Conf(..))

runApp :: IO ()
runApp = do
    simpleHTTP nullConf { port = 8081 } $ do
        method GET
        uriRest (\('/':filePath) -> do
            liftIO $ putStrLn filePath
            serveFileFrom "static/" (guessContentTypeM mimeTypes) filePath)
