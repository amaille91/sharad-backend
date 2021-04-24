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
             , NoteUpdate(NoteUpdate)
             , targetId
             , newContent
             ) where 

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Prelude hiding (id)

data NoteContent = NoteContent { title :: Maybe String
                               , content :: String
                               } deriving(Show, Generic, Eq)

instance FromJSON NoteContent
instance ToJSON NoteContent

data StorageId = StorageId { id :: String 
                           , version :: String
                           } deriving (Show, Generic, Eq)

instance ToJSON StorageId
instance FromJSON StorageId

data Note = Note { storageId :: StorageId
                 , noteContent :: NoteContent
                 } deriving (Show, Generic, Eq)

instance ToJSON Note
instance FromJSON Note

data NoteUpdate = NoteUpdate { targetId :: String 
                             , newContent :: NoteContent
                             } deriving (Show, Generic)

instance FromJSON NoteUpdate