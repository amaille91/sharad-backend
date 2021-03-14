{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( runApp
    ) where

import Control.Monad (msum, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (takeMVar)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import Data.Aeson (decode, encode)
import Happstack.Server (Response, ServerPartT, BodyPolicy, RqBody, takeRequestBody, unBody, rqBody, decodeBody, askRq, defaultBodyPolicy, toResponse, nullDir, path, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse,  method, ok, badRequest, internalServerError, dir, Method(GET, POST), Conf(..))

import Model (NoteContent)
import qualified NoteService as NoteService

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
            let noteContent :: Maybe NoteContent = decode $ unBody rqBody
            fmap createNoteContent noteContent `orElse` internalServerError (toResponse ())
            --fmap (ok . toResponse . createNote) noteContent `orElse` badRequest (toResponse ())

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

orElse :: Maybe a -> a -> a
(Just a) `orElse` _ = a
_        `orElse` b = b

createNoteContent :: NoteContent -> ServerPartT IO Response
createNoteContent noteContent = do
    maybeStorageId <- liftIO $ NoteService.createNote noteContent
    fmap (ok . toResponse .encode) maybeStorageId `orElse` internalServerError (toResponse ())
