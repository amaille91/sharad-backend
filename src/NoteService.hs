{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module NoteService (DiskFileStorageConfig(..), Error(..), createNote, getAllNotes, deleteNote, modifyNote) where 

import Prelude hiding (id, writeFile, readFile)
import Data.Maybe (fromJust)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad (mzero)
import System.Directory (removePathForcibly, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import Model (NoteContent(..), StorageId(..), Note(..), NoteUpdate(..))
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Digest.Pure.SHA (sha256, bytestringDigest)
import Data.ByteString.Lazy.Char8 as BL hiding(map, filter)
import Data.ByteString.Lazy.Base64 (encodeBase64)
import Data.Text.Lazy as T hiding(map, filter)
import Data.Aeson (ToJSON, encode, decode)

-- DATA
type Id = String
data DiskFileStorageConfig = DiskFileStorageConfig { rootPath :: String }
data Error = NotFound FilePath | NotCurrentVersion StorageId | FatalError String deriving(Eq, Show)

    -- PRIVATE
-- A fileName with its extension but without its containing directory path (e.g. "26dfc71b-63aa-479d-b1c0-3d90eec1083c.txt")
type SimpleFileName = String

-- FUNCTIONS
createNote :: DiskFileStorageConfig -> NoteContent -> MaybeT IO StorageId
createNote config nc@(NoteContent { content  = contentStr }) = do
    uuid <- liftIO $ fmap toString nextRandom
    liftIO $ writeContentOnDisk config nc uuid

base64Sha256 :: String -> String
base64Sha256 contentToHash = T.unpack . encodeBase64 . bytestringDigest . sha256 $ (BL.pack contentToHash)

getAllNotes :: DiskFileStorageConfig -> ExceptT Error IO [Note]
getAllNotes config = do
    storageDirExist <- lift $ doesDirectoryExist (rootPath config)
    if not storageDirExist
        then return []
        else do
            simpleFileNames <- lift $ (listDirectory (rootPath config))
            contents <- lift $ mapM (readNote config) simpleFileNames
            return $ fromMaybes contents

deleteNote :: DiskFileStorageConfig -> String -> IO (Maybe Error)
deleteNote config id = runMaybeT $ do
    storageDirExist <- lift $ doesFileExist $ fileName config id
    if not storageDirExist
        then return $ NotFound id
        else do
            lift $ removePathForcibly $ fileName config id
            mzero

modifyNote :: DiskFileStorageConfig -> NoteUpdate -> ExceptT Error IO StorageId
modifyNote config NoteUpdate { targetId = requestedStorageId, newContent = new } = do
    noteExists <- lift $ doesFileExist $ fileName config (id requestedStorageId)
    if not noteExists
        then throwE $ NotFound (id requestedStorageId)
        else do
            Just (Note { storageId = retrievedStorageId, noteContent = content }) <- lift $ readNote config (id requestedStorageId ++ noteExtension)
            if retrievedStorageId == requestedStorageId
                then lift $ writeContentOnDisk config new (id requestedStorageId)
                else throwE $ NotCurrentVersion requestedStorageId
            

writeContentOnDisk :: DiskFileStorageConfig -> NoteContent -> String -> IO StorageId
writeContentOnDisk storageConfig noteContent fileId = do
    let storeId = StorageId { id = fileId, version = base64Sha256 (content noteContent) }
    createDirectoryIfMissing True (rootPath storageConfig)
    writeFile (fileName storageConfig fileId) (encode $ Note { noteContent = noteContent, storageId = storeId })
    return storeId

fromMaybes :: (Eq a) => [Maybe a] -> [a]
fromMaybes = (map fromJust) . (filter (/= Nothing))

fromMaybesM :: (Eq a, Monad m) => [Maybe a] -> m [a]
fromMaybesM = return . fromMaybes

readNote :: DiskFileStorageConfig -> SimpleFileName -> IO (Maybe Note)
readNote config = (fmap decode) . readFile . (prefixWithStorageDir config)

fileName :: DiskFileStorageConfig -> Id -> FilePath
fileName storageConfig id = prefixWithStorageDir storageConfig id ++ noteExtension

noteExtension :: String
noteExtension = ".txt"

prefixWithStorageDir :: DiskFileStorageConfig -> String -> String
prefixWithStorageDir storageConfig s = rootPath storageConfig ++ s

