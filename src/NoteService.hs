{-# LANGUAGE NamedFieldPuns #-}

module NoteService (Error(NotFound, FatalError), createNote, getAllNotes, deleteNote) where 

import Prelude hiding (id, writeFile, readFile)
import Data.Maybe (fromJust)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad (mzero)
import System.Directory (removePathForcibly, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import Model (NoteContent(..), StorageId(..), Note(..))
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Digest.Pure.SHA (sha256, bytestringDigest)
import Data.ByteString.Lazy.Char8 as BL hiding(map, filter)
import Data.ByteString.Lazy.Base64 (encodeBase64)
import Data.Text.Lazy as T hiding(map, filter)
import Data.Aeson (ToJSON, encode, decode)

createNote :: NoteContent -> MaybeT IO StorageId
createNote nc@(NoteContent { content  = contentStr }) = do
    uuid <- liftIO $ fmap toString nextRandom
    let storeId = StorageId { id = uuid, version = base64Sha256 contentStr }
    liftIO $ do
        createDirectoryIfMissing True "target/.sharad/data"
        writeFile (fileName uuid) (encode $ Note { noteContent = nc, storageId = storeId })
    return storeId

base64Sha256 :: String -> String
base64Sha256 contentToHash = T.unpack . encodeBase64 . bytestringDigest . sha256 $ (BL.pack contentToHash)

getAllNotes :: ExceptT Error IO [Note]
getAllNotes = do
    storageDirExist <- lift $ doesDirectoryExist "target/.sharad/data/"
    if not storageDirExist
        then return []
        else do
            simpleFileNames <- lift $ (listDirectory "target/.sharad/data/")
            contents <- lift $ mapM readNote simpleFileNames
            return $ fromMaybes contents

deleteNote :: String -> IO (Maybe Error)
deleteNote id = runMaybeT $ do
    storageDirExist <- lift $ doesFileExist $ fileName id
    if not storageDirExist
        then return $ FatalError ("File " ++ fileName id ++ " does not exist")
        else do
            lift $ removePathForcibly $ fileName id
            mzero

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
