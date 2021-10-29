{-# LANGUAGE ScopedTypeVariables #-}

module NoteService (createItem, getAllItems, deleteItem, modifyItem) where

import Prelude hiding (id, writeFile, readFile, log)
import Data.Maybe (fromJust)
import Control.Exception (catch)
import Control.Monad.Except (catchError, throwError, mapExceptT, withExceptT)
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (mzero)
import System.Directory (removePathForcibly, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import Model (NoteContent(..), StorageId(..), ChecklistContent(..), Content, hash, Identifiable(..))
import Crud (DiskFileStorageConfig(..), CRUDEngine(..), Error(..), CrudReadException(..), CrudModificationException(..), CrudWriteException(..), FromCrudReadException(..), FromCrudWriteException(..))
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified Data.UUID as UUID (toString)
import Data.ByteString.Lazy.Char8 as BL hiding (map, filter, putStrLn, appendFile)
import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)

-- DATA
type Id = String

-- PRIVATE
-- A fileName with its extension but without its containing directory path (e.g. "26dfc71b-63aa-479d-b1c0-3d90eec1083c.txt")
type SimpleFileName = String

-- FUNCTIONS
createItem :: CRUDEngine crudConfig a => crudConfig -> a -> ExceptT CrudWriteException IO StorageId
createItem config nc = do
    uuid <- liftIO $ fmap UUID.toString UUID.nextRandom
    writeContentToFile config nc uuid

getAllItems :: CRUDEngine crudConfig a => crudConfig -> ExceptT CrudReadException IO [ExceptT CrudReadException IO (Identifiable a)]
getAllItems config = do
    simpleFileNames <- catchIO (listDirectory (rootPath config))
                               (IOReadException)
    return $ fmap (readItemFromFile config) simpleFileNames

deleteItem :: DiskFileStorageConfig confType => confType -> String -> ExceptT CrudWriteException IO ()
deleteItem config id = catchIO (removePathForcibly $ fileName config id) (IOWriteException)

modifyItem :: CRUDEngine crudConfig a => crudConfig -> Identifiable a -> ExceptT CrudModificationException IO StorageId
modifyItem config (Identifiable targetStorageId new) = do
    liftIO $ log ("modifying file with id " ++ show targetStorageId)
    (Identifiable retrievedStorageId content) <- withExceptT fromCrudReadException $ readItemFromFile config (id targetStorageId ++ txtExtension)
    if retrievedStorageId == targetStorageId
        then withExceptT fromCrudWriteException $ writeContentToFile config new (id targetStorageId)
        else throwError $ NotCurrentVersion targetStorageId


writeContentToFile :: CRUDEngine crudConfig a => crudConfig -> a -> String -> ExceptT CrudWriteException IO StorageId
writeContentToFile storageConfig content fileId = do
    let storeId = StorageId { id = fileId, version = hash content }
    catchIO (createDirectoryIfMissing True (rootPath storageConfig))
            (IOWriteException)
    catchIO (writeFile (fileName storageConfig fileId) (encode $ (Identifiable storeId content)))
            (IOWriteException)
    return storeId

fromMaybes :: (Eq a) => [Maybe a] -> [a]
fromMaybes = map fromJust . filter (/= Nothing)

readItemFromFile :: CRUDEngine crudConfig a => crudConfig -> SimpleFileName -> ExceptT CrudReadException IO (Identifiable a)
readItemFromFile config file = do 
    stringToParse <- (readFileImpl . prefixWithStorageDir config) file
    withExceptT (parsingExceptionToCrudException file) $ parseString config stringToParse

parseString :: CRUDEngine crudConfig a => crudConfig -> ByteString -> ExceptT ParsingException IO (Identifiable a)
parseString config strToParse = withExceptT (ParsingException strToParse)
                                            (ExceptT $ eitherDecodeM strToParse)
    where eitherDecodeM = return . eitherDecode

data ParsingException = ParsingException ParsedString ErrorMessage
type ErrorMessage = String
type ParsedString = ByteString

parsingExceptionToCrudException :: FilePath -> ParsingException -> CrudReadException
parsingExceptionToCrudException file (ParsingException parsedString errorMessage) =
    CrudParsingException parsedString errorMessage file

readFileImpl :: FilePath -> ExceptT CrudReadException IO ByteString
readFileImpl file = catchIO (readFile file)
                            (IOReadException)

fileName :: DiskFileStorageConfig crudConfig => crudConfig -> Id -> FilePath
fileName storageConfig id = prefixWithStorageDir storageConfig id ++ txtExtension

txtExtension :: String
txtExtension = ".txt"

prefixWithStorageDir :: DiskFileStorageConfig crudConfig => crudConfig -> SimpleFileName -> String
prefixWithStorageDir storageConfig s = postFixWithIfNeeded '/' (rootPath storageConfig) ++ s
    where
        postFixWithIfNeeded c [] = c:[] 
        postFixWithIfNeeded c (x:[]) = if c == x then c:[] else x:c:[] 
        postFixWithIfNeeded c (x:xs) = x:postFixWithIfNeeded c xs 

log :: MonadIO m => String -> m ()
log s = liftIO $ appendFile  "./server.log" $ s ++ "\n"

catchIO :: IO b -> (IOError -> a) -> ExceptT a IO b
catchIO action errorHandler = ExceptT $ catch (fmap Right $ action)
                                              (return . Left . errorHandler)
