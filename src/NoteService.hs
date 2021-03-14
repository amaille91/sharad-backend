{-# LANGUAGE NamedFieldPuns #-}

module NoteService (createNote) where 

import Prelude hiding (id, writeFile)
import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing)
import Model (NoteContent(..), StorageId(..), Note(..))
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Digest.Pure.SHA (sha256, bytestringDigest)
import Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.Base64 (encodeBase64)
import Data.Text.Lazy as T
import Data.Aeson (ToJSON, encode)

createNote :: NoteContent -> IO (Maybe StorageId)
createNote nc@(NoteContent { content  = contentStr }) = do
    uuid <- fmap toString $ nextRandom
    let storeId = StorageId { id = uuid, version = base64Sha256 contentStr }
    liftIO $ do
        createDirectoryIfMissing True "target/.sharad/data"
        writeFile ("target/.sharad/data/" ++ uuid ++ ".txt") (encode $ Note { noteContent = nc, storageId = storeId })
    return $ Just storeId

base64Sha256 :: String -> String
base64Sha256 contentToHash = T.unpack . encodeBase64 . bytestringDigest . sha256 $ (BL.pack contentToHash)
