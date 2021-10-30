{-# LANGUAGE MultiParamTypeClasses #-}

module NoteCrud (NoteServiceConfig(..), defaultNoteServiceConfig) where

import           Crud        (CRUDEngine (..), DiskFileStorageConfig (..))
import           Model       (NoteContent (..))
import qualified NoteService as NoteService

data NoteServiceConfig = NoteServiceConfig String

instance DiskFileStorageConfig NoteServiceConfig where
    rootPath (NoteServiceConfig path) = path

instance CRUDEngine NoteServiceConfig NoteContent where
  getItems = NoteService.getAllItems
  postItem = NoteService.createItem
  delItem = NoteService.deleteItem
  putItem = NoteService.modifyItem
  crudTypeDenomination _ = "note"

defaultNoteServiceConfig = NoteServiceConfig ".sharad/data/note"
