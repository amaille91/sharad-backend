{-# LANGUAGE MultiParamTypeClasses #-}

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

import Model (NoteContent, ChecklistContent, Content, Identifiable(..))
import qualified NoteService as NoteService
import Crud
import NoteCrud (NoteServiceConfig(..))

data ChecklistServiceConfig = ChecklistServiceConfig

instance DiskFileStorageConfig ChecklistServiceConfig where
    rootPath ChecklistServiceConfig = ".sharad/data/note"

instance CRUDEngine ChecklistServiceConfig ChecklistContent where
  getItems = NoteService.getAllItems
  postItem = NoteService.createItem
  delItem = NoteService.deleteItem
  putItem = NoteService.modifyItem
  crudTypeDenomination ChecklistServiceConfig = "checklist"
    
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
    dir "note" $ msum [ crudGet NoteServiceConfig
                      , crudPost NoteServiceConfig
                      , crudDelete NoteServiceConfig
                      , crudPut NoteServiceConfig
                      ]

checklistController :: ServerPartT IO Response
checklistController = do
  lift $ putStrLn "hitting lift controller"
  dir "checklist" $ msum [ crudGet ChecklistServiceConfig
                         , crudPost ChecklistServiceConfig
                         , crudDelete ChecklistServiceConfig
                         , crudPut ChecklistServiceConfig
                         ]

crudGet ::CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudGet crudConfig = do
    nullDir
    method GET
    recoverWith (const $ genericInternalError $ "Unexpected problem during retrieving all " ++ crudTypeDenomination crudConfig)
                (fmap (ok . toResponse . encode) $ getItems crudConfig)

crudPost ::CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudPost crudConfig = do
    nullDir
    method POST
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let noteContent = decode $ unBody rqBody :: Content a => Maybe a 
            fmap (createNoteContent crudConfig) noteContent `orElse` genericInternalError "Unexpected problem during note creation"
    fmap handleBody body `orElse` ok (toResponse "NoBody")

createNoteContent :: CRUDEngine crudType a => crudType -> a -> ServerPartT IO Response
createNoteContent crudConfig noteContent = do
    withDefaultIO emptyInternalError
                  (fmap (ok . toResponse .encode) $ NoteService.createItem crudConfig noteContent)

crudDelete :: CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudDelete crudConfig = do
    method DELETE
    path (\pathId -> do
        nullDir
        maybeError <- liftIO $ NoteService.deleteItem crudConfig pathId
        fmap toGenericError maybeError `orElse` ok (toResponse ()))

crudPut :: CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudPut crudConfig = do
    nullDir
    method PUT
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let noteUpdate = decode $ unBody rqBody
            fmap (handleUpdate crudConfig) noteUpdate `orElse` genericInternalError "Unable to parse body as a NoteUpdate"
    fmap handleBody body `orElse` ok (toResponse "NoBody")

handleUpdate :: CRUDEngine crudType a => crudType -> Identifiable a -> ServerPartT IO Response
handleUpdate crudConfig update =  do
    recoverWith (const.notFound.toResponse $ "Unable to find storage dir")
        (fmap (ok.toResponse.encode) (NoteService.modifyItem crudConfig update))

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

