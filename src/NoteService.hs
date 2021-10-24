{-# LANGUAGE ScopedTypeVariables #-}

module NoteService (DiskFileStorageConfig(..), Error(..), createItem, getAllItems, deleteItem, modifyNote) where

import Prelude hiding (id, writeFile, readFile, log)
import Data.Maybe (fromJust)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad (mzero)
import System.Directory (removePathForcibly, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import Model (NoteContent(..), StorageId(..), ChecklistContent(..), Content, hash, Identifiable(..))
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.ByteString.Lazy.Char8 as BL hiding (map, filter, putStrLn, appendFile)
import Data.Aeson (ToJSON, FromJSON, encode, decode)

-- DATA
type Id = String
newtype DiskFileStorageConfig = DiskFileStorageConfig { rootPath :: String }
data Error = NotFound FilePath | NotCurrentVersion StorageId | FatalError String deriving(Eq, Show)

    -- PRIVATE
-- A fileName with its extension but without its containing directory path (e.g. "26dfc71b-63aa-479d-b1c0-3d90eec1083c.txt")
type SimpleFileName = String

-- FUNCTIONS
createItem :: Content a => DiskFileStorageConfig -> a -> MaybeT IO StorageId
createItem config nc = do
    uuid <- liftIO $ fmap toString nextRandom
    liftIO $ writeContentOnDisk config nc uuid

getAllItems :: (Eq a, FromJSON a) => DiskFileStorageConfig -> ExceptT Error IO [a]
getAllItems config = do
    storageDirExist <- lift $ doesDirectoryExist (rootPath config)
    if not storageDirExist
        then return []
        else do
            simpleFileNames <- lift $ listDirectory (rootPath config)
            contents <- lift $ mapM (readItemInFile config) simpleFileNames
            return $ fromMaybes contents

deleteItem :: DiskFileStorageConfig -> String -> IO (Maybe Error)
deleteItem config id = runMaybeT $ do
    storageDirExist <- lift $ doesFileExist $ fileName config id
    if not storageDirExist
        then return $ NotFound id
        else do
            lift $ removePathForcibly $ fileName config id
            mzero

modifyNote :: Content a => DiskFileStorageConfig -> Identifiable a -> ExceptT Error IO StorageId
modifyNote config (Identifiable requestedStorageId new) = do
    noteExists <- lift $ doesFileExist $ fileName config (id requestedStorageId)
    if not noteExists
        then throwE $ NotFound (id requestedStorageId)
        else do
            Just (Identifiable retrievedStorageId (content :: NoteContent)) <- lift $ readItemInFile config (id requestedStorageId ++ noteExtension)
            if retrievedStorageId == requestedStorageId
                then lift $ writeContentOnDisk config new (id requestedStorageId)
                else throwE $ NotCurrentVersion requestedStorageId


writeContentOnDisk :: Content a => DiskFileStorageConfig -> a -> String -> IO StorageId
writeContentOnDisk storageConfig content fileId = do
    let storeId = StorageId { id = fileId, version = hash content }
    log $ "Creating "  ++ (rootPath storageConfig)
    createDirectoryIfMissing True (rootPath storageConfig)
    log $ "Writing file " ++ (fileName storageConfig fileId)
    writeFile (fileName storageConfig fileId) (encode $ (Identifiable storeId content))
    return storeId

fromMaybes :: (Eq a) => [Maybe a] -> [a]
fromMaybes = map fromJust . filter (/= Nothing)

fromMaybesM :: (Eq a, Monad m) => [Maybe a] -> m [a]
fromMaybesM = return . fromMaybes

readItemInFile :: FromJSON a => DiskFileStorageConfig -> SimpleFileName -> IO (Maybe a)
readItemInFile config = fmap decode . readFile . prefixWithStorageDir config

fileName :: DiskFileStorageConfig -> Id -> FilePath
fileName storageConfig id = prefixWithStorageDir storageConfig id ++ noteExtension

noteExtension :: String
noteExtension = ".txt"

prefixWithStorageDir :: DiskFileStorageConfig -> String -> String
prefixWithStorageDir storageConfig s = rootPath storageConfig ++ s

hello = "hello"

log s = appendFile  "./server.log" $ s ++ "\n"
