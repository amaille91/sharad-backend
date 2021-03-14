{-# LANGUAGE DeriveGeneric #-}

module Model (NoteContent(NoteContent)
             , content
             , title
             , StorageId(StorageId)
             , id
             , version
             , Note(Note)
             , storageId
             , noteContent
             ) where 

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Prelude hiding (id)

data NoteContent = NoteContent { title :: Maybe String
                               , content :: String
                               } deriving(Show, Generic)

instance FromJSON NoteContent
instance ToJSON NoteContent

data StorageId = StorageId { id :: String 
                           , version :: String
                           } deriving (Show, Generic)

instance ToJSON StorageId

data Note = Note { storageId :: StorageId
                 , noteContent :: NoteContent
                 } deriving (Show, Generic)

instance ToJSON Note