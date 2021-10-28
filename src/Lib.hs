{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( runApp
    ) where

import Prelude hiding (log)
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
    rootPath ChecklistServiceConfig = ".sharad/data/checklist"

instance CRUDEngine ChecklistServiceConfig ChecklistContent where
  getItems = NoteService.getAllItems
  postItem = NoteService.createItem
  delItem = NoteService.deleteItem
  putItem = NoteService.modifyItem
  crudTypeDenomination ChecklistServiceConfig = "checklist"
    
runApp :: IO ()
runApp = do
    putStrLn "running server"
    simpleHTTP nullConf { port = 8081 } $ do
        askRq >>= (\rq -> log (show rq))
        log "=========================END REQUEST===================="
        msum [ noteController
             , checklistController
             , serveStaticResource
             , mzero
             ]


noteController :: ServerPartT IO Response
noteController = do
    dir "note" $ msum [ crudGet noteServiceConfig
                      , crudPost noteServiceConfig
                      , crudDelete noteServiceConfig
                      , crudPut noteServiceConfig
                      ]
    where noteServiceConfig = NoteServiceConfig ".sharad/data/note"

checklistController :: ServerPartT IO Response
checklistController = do
  dir "checklist" $ msum [ crudGet ChecklistServiceConfig
                         , crudPost ChecklistServiceConfig
                         , crudDelete ChecklistServiceConfig
                         , crudPut ChecklistServiceConfig
                         ]

crudGet ::CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudGet crudConfig = do
    nullDir
    method GET
    log ("crud GET on " ++ crudTypeDenomination crudConfig)
    recoverWith (const $ genericInternalError $ "Unexpected problem during retrieving all " ++ crudTypeDenomination crudConfig)
               logAndSendBackItems
    where
        items = getItems crudConfig
        logAndSendBackItems = do
            items >>= \i -> log ("Retrieved items: " ++ show i)
            fmap (ok . toResponse . encode) items

crudPost ::CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudPost crudConfig = do
    nullDir
    method POST
    log ("crud POST on " ++ crudTypeDenomination crudConfig)
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let bodyBS = unBody rqBody
                noteContent = decode bodyBS :: Content a => Maybe a 
            log ("Getting body bytestrings: " ++ show bodyBS)
            log ("Getting deserialized content: " ++ show noteContent)
            fmap (createNoteContent crudConfig) noteContent `orElse` genericInternalError "Unexpected problem during note creation"
    fmap handleBody body `orElse` ok (toResponse "NoBody")

createNoteContent :: CRUDEngine crudType a => crudType -> a -> ServerPartT IO Response
createNoteContent crudConfig noteContent = do
    withDefaultIO emptyInternalError
                  (fmap (ok . toResponse .encode) $ NoteService.createItem crudConfig noteContent)

crudDelete :: CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudDelete crudConfig = do
    method DELETE
    log ("crud DELETE on " ++ crudTypeDenomination crudConfig)
    path (\pathId -> do
        nullDir
        maybeError <- liftIO $ NoteService.deleteItem crudConfig pathId
        fmap (handleError pathId) maybeError `orElse` ok (toResponse ()))

handleError :: String -> Error -> ServerPartT IO Response
handleError pathId err = do
    logError pathId err 
    emptyInternalError

logError pathId s = log ("Error while deleting item " ++ pathId ++ ": " ++ show s)

crudPut :: CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudPut crudConfig = do
    nullDir
    method PUT
    log ("crud PUT on " ++ crudTypeDenomination crudConfig)
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let bodyBS = unBody rqBody
            let noteUpdate = decode bodyBS
            log ("Getting body bytestrings: " ++ show bodyBS)
            log ("Getting deserialized content: " ++ show noteUpdate)
            fmap (handleUpdate crudConfig) noteUpdate `orElse` genericInternalError "Unable to parse body as a NoteUpdate"
    fmap handleBody body `orElse` ok (toResponse "NoBody")

handleUpdate :: CRUDEngine crudType a => crudType -> Identifiable a -> ServerPartT IO Response
handleUpdate crudConfig update =  do
    recoverWith (const.notFound.toResponse $ "Unable to find storage dir")
        (fmap (ok.toResponse.encode) (NoteService.modifyItem crudConfig update))

serveStaticResource :: ServerPartT IO Response
serveStaticResource = do
    log "hitting static resource"
    method GET
    path (\filePath -> do
        lift $ putStrLn ("trying to get resource " ++ filePath)
        nullDir
        serveFileFrom "static/" (guessContentTypeM mimeTypes) filePath)

toBadRequest :: (Show e) => e -> ServerPartT IO Response
toBadRequest e = badRequest $ toResponse $ show e

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
genericInternalError s = do
    lift $ putStrLn ("Internal error: \n\t" ++ s)
    internalServerError $ toResponse s

emptyInternalError :: ServerPartT IO Response
emptyInternalError = internalServerError $ toResponse ()

log :: (MonadTrans t) => String -> t IO () 
log = lift . putStrLn
