{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( runApp
    ) where

import Control.Monad (msum, mzero, join)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Either (either)
import Control.Concurrent.MVar (takeMVar)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import Data.Aeson (decode, encode)
import Happstack.Server (Response, ServerPartT, BodyPolicy, RqBody, takeRequestBody, unBody, rqBody, decodeBody, askRq, defaultBodyPolicy, toResponse, nullDir, path, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse,  method, ok, badRequest, internalServerError, notFound, dir, Method(GET, POST, DELETE, PUT), Conf(..))

import Model (NoteContent, Note, NoteUpdate)
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
getNote = do
    nullDir
    method GET
    recoverWith (const $ genericInternalError "Unexpected problem during retrieving all notes")
                (fmap (ok . toResponse . encode) NoteService.getAllNotes)

postNote :: ServerPartT IO Response
postNote = do
    nullDir
    method POST
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let noteContent :: Maybe NoteContent = decode $ unBody rqBody
            fmap createNoteContent noteContent `orElse` genericInternalError "Unexpected problem during note creation"
    fmap handleBody body `orElse` (ok $ toResponse "NoBody")

deleteNote :: ServerPartT IO Response
deleteNote = do
    method DELETE
    path (\pathId -> do
        nullDir
        maybeError <- liftIO $ NoteService.deleteNote pathId
        (fmap toGenericError maybeError) `orElse` (ok $ toResponse ()))

putNote :: ServerPartT IO Response
putNote = do
    nullDir
    method PUT
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let noteUpdate :: Maybe NoteUpdate = decode $ unBody rqBody
            fmap handleUpdate noteUpdate `orElse` genericInternalError "Unable to parse body as a NoteUpdate"
    fmap handleBody body `orElse` (ok $ toResponse "NoBody")

handleUpdate :: NoteUpdate -> ServerPartT IO Response
handleUpdate update =  do
    recoverWith (const $ notFound $ toResponse "Unable to find storage dir")
        (fmap (ok . toResponse . encode) (NoteService.modifyNote update))

serveStaticResource :: ServerPartT IO Response
serveStaticResource = do
    method GET
    path (\filePath -> do
        nullDir
        serveFileFrom "static/" (guessContentTypeM mimeTypes) filePath)

toGenericError :: (Show e) => e -> ServerPartT IO Response
toGenericError e = badRequest $ toResponse $ show e

orElse :: Maybe a -> a -> a
(Just a) `orElse` _ = a
_        `orElse` b = b

createNoteContent :: NoteContent -> ServerPartT IO Response
createNoteContent noteContent = do
    withDefaultIO emptyInternalError
                  (fmap (ok . toResponse .encode) $ NoteService.createNote noteContent)

recoverIO :: (MonadIO m, Monad m) => ExceptT e IO (m a) -> (e -> (m a)) -> m a
recoverIO exceptT f = join $ liftIO $ fmap (either f id) (runExceptT exceptT)

recoverWith :: (MonadIO m, Monad m) => (e -> (m a)) -> ExceptT e IO (m a) -> m a
recoverWith = flip recoverIO

orElseIO :: (MonadIO m, Monad m) => MaybeT IO (m a) -> m a -> m a
orElseIO maybe alt = do
    tmp <- liftIO $ runMaybeT maybe
    tmp `orElse` alt

withDefaultIO :: (MonadIO m, Monad m) => m a -> MaybeT IO (m a) -> m a
withDefaultIO = flip orElseIO

genericInternalError :: String -> ServerPartT IO Response
genericInternalError = internalServerError . toResponse

emptyInternalError :: ServerPartT IO Response
emptyInternalError = internalServerError $ toResponse ()

