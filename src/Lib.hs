module Lib
    ( runApp
    ) where

import Control.Monad (msum, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (takeMVar)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import Happstack.Server (Response, ServerPartT, BodyPolicy, RqBody, takeRequestBody, unBody, rqBody, decodeBody, askRq, defaultBodyPolicy, toResponse, nullDir, path, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse,  method, ok, dir, Method(GET, POST), Conf(..))

runApp :: IO ()
runApp = do
    simpleHTTP nullConf { port = 8081 } $ msum [ noteController
                                               ,serveStaticResource
                                               , mzero
                                               ]
        

noteController :: ServerPartT IO Response
noteController = do
    dir "note" $ do
        msum [ getNote
             , postNote
             , deleteNote
             , putNote
             ]

getNote :: ServerPartT IO Response
getNote = mzero

postNote :: ServerPartT IO Response
postNote = do
    nullDir
    method POST
    req <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
        Just rqBody -> handleBody rqBody
        Nothing -> ok $ toResponse "NoBody"
    where
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let bodyStr = BL.unpack $ unBody rqBody
            ok $ toResponse bodyStr

deleteNote :: ServerPartT IO Response
deleteNote = mzero

putNote :: ServerPartT IO Response
putNote = mzero


serveStaticResource :: ServerPartT IO Response
serveStaticResource = do
    method GET
    path (\filePath -> do
        nullDir
        serveFileFrom "static/" (guessContentTypeM mimeTypes) filePath)
