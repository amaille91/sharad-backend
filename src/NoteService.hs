{-# LANGUAGE NamedFieldPuns #-}

module NoteService (Error(NotFound, FatalError), createNote, getAllNotes, deleteNote) where 

import Prelude hiding (id, writeFile, readFile)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import System.Directory (removePathForcibly, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import Model (NoteContent(..), StorageId(..), Note(..))
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Digest.Pure.SHA (sha256, bytestringDigest)
import Data.ByteString.Lazy.Char8 as BL hiding(map, filter)
import Data.ByteString.Lazy.Base64 (encodeBase64)
import Data.Text.Lazy as T hiding(map, filter)
import Data.Aeson (ToJSON, encode, decode)

createNote :: NoteContent -> IO (Maybe StorageId)
createNote nc@(NoteContent { content  = contentStr }) = do
    uuid <- fmap toString $ nextRandom
    let storeId = StorageId { id = uuid, version = base64Sha256 contentStr }
    liftIO $ do
        createDirectoryIfMissing True "target/.sharad/data"
        writeFile (fileName uuid) (encode $ Note { noteContent = nc, storageId = storeId })
    return $ Just storeId

base64Sha256 :: String -> String
base64Sha256 contentToHash = T.unpack . encodeBase64 . bytestringDigest . sha256 $ (BL.pack contentToHash)

getAllNotes :: IO (Either Error [Note])
getAllNotes = do
    storageDirExist <- doesDirectoryExist "target/.sharad/data/"
    if not storageDirExist
        then return $ Right []
        else do
            simpleFileNames <- (listDirectory "target/.sharad/data/")
            contents <- mapM readNote simpleFileNames
            return $ fromMaybesM contents

deleteNote :: String -> IO (Maybe Error)
deleteNote id = do
    storageDirExist <- doesFileExist $ fileName id
    if not storageDirExist
        then return $ Just (FatalError ("File " ++ fileName id ++ " does not exist"))
        else do
            removePathForcibly $ fileName id
            return Nothing

data Error = NotFound FilePath | FatalError String deriving(Show)

fromMaybes :: (Eq a) => [Maybe a] -> [a]
fromMaybes = (map fromJust) . (filter (/= Nothing))

fromMaybesM :: (Eq a, Monad m) => [Maybe a] -> m [a]
fromMaybesM = return . fromMaybes

readNote :: String -> IO (Maybe Note)
readNote = (fmap decode) . readFile . prefixWithStorageDir

fileName :: Id -> FilePath
fileName id = prefixWithStorageDir id ++ ".txt"

prefixWithStorageDir :: String -> String
prefixWithStorageDir s = "target/.sharad/data/" ++ s

type Id = String
