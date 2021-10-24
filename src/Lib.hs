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

import Model (NoteContent, Content, Identifiable(..))
import qualified NoteService as NoteService

data CRUDType = CRUDNote | CRUDChecklist

defaultCRUDServiceConf :: CRUDType -> NoteService.DiskFileStorageConfig
defaultCRUDServiceConf CRUDNote = NoteService.DiskFileStorageConfig { NoteService.rootPath = ".sharad/data/note" }
defaultCRUDServiceConf CRUDChecklist = NoteService.DiskFileStorageConfig { NoteService.rootPath = ".sharad/data/checklist" }

runApp :: IO ()
runApp = do
    putStrLn "hitting server"
    simpleHTTP nullConf { port = 8081 } $ msum [ noteController
                                               , checklistController
                                               , serveStaticResource
                                               , mzero
                                               ]


noteController :: ServerPartT IO Response
noteController = do
    lift $ putStrLn "hitting NoteController"
    dir "note" $ msum [ crudGet CRUDChecklist
                      , crudPost CRUDNote
                      , crudDelete CRUDNote
                      , crudPut CRUDNote
                      ]

checklistController :: ServerPartT IO Response
checklistController = do
  lift $ putStrLn "hitting lift controller"
  dir "checklist" $ msum [ crudGet CRUDChecklist
                         , crudPost CRUDChecklist
                         , crudDelete CRUDChecklist
                         , crudPut CRUDChecklist
                         ]

crudGet :: CRUDType -> ServerPartT IO Response
crudGet crudType = do
    nullDir
    method GET
    recoverWith (const $ genericInternalError $ "Unexpected problem during retrieving all " ++ crudTypeDenomination crudType)
                (fmap (ok . toResponse . encode) (NoteService.getAllItems $ defaultCRUDServiceConf crudType))
  where
    crudTypeDenomination CRUDNote = "notes"
    crudTypeDenomination CRUDChecklist = "checklists"
    

crudPost :: CRUDType -> ServerPartT IO Response
crudPost crudType = do
    nullDir
    method POST
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let noteContent :: Content a => Maybe a 
                noteContent = decode $ unBody rqBody
            fmap (createContent crudType) noteContent `orElse` genericInternalError "Unexpected problem during note creation"
    fmap handleBody body `orElse` ok (toResponse "NoBody")


createContent :: Content a => CRUDType -> a -> ServerPartT IO Response
createContent crudType noteContent = do
    withDefaultIO emptyInternalError
                  (fmap (ok . toResponse .encode) $ NoteService.createItem noteServiceConfig noteContent)
    where noteServiceConfig = defaultCRUDServiceConf crudType 

crudDelete :: CRUDType -> ServerPartT IO Response
crudDelete crudType = do
    method DELETE
    path (\pathId -> do
        nullDir
        maybeError <- liftIO $ NoteService.deleteItem noteServiceConf pathId
        fmap toGenericError maybeError `orElse` ok (toResponse ()))
        where noteServiceConf = defaultCRUDServiceConf crudType

crudPut :: CRUDType -> ServerPartT IO Response
crudPut crudType = do
    nullDir
    method PUT
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let noteUpdate :: Content a => Maybe (Identifiable a)
                noteUpdate = decode $ unBody rqBody
            fmap (handleUpdate crudType) noteUpdate `orElse` genericInternalError "Unable to parse body as a NoteUpdate"
    fmap handleBody body `orElse` ok (toResponse "NoBody")

handleUpdate :: CRUDType -> Identifiable NoteContent -> ServerPartT IO Response
handleUpdate crudType update =  do
    recoverWith (const.notFound.toResponse $ "Unable to find storage dir")
        (fmap (ok.toResponse.encode) (NoteService.modifyNote crudServiceConf update))
        where crudServiceConf = defaultCRUDServiceConf crudType

serveStaticResource :: ServerPartT IO Response
serveStaticResource = do
    lift $ putStrLn "hitting static resource"
    method GET
    path (\filePath -> do
        lift $ putStrLn ("trying to get resource " ++ filePath)
        nullDir
        serveFileFrom "static/" (guessContentTypeM mimeTypes) filePath)

toGenericError :: (Show e) => e -> ServerPartT IO Response
toGenericError e = badRequest $ toResponse $ show e

orElse :: Maybe a -> a -> a
(Just a) `orElse` _ = a
_        `orElse` b = b

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

