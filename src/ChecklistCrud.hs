{-# LANGUAGE MultiParamTypeClasses #-}

module ChecklistCrud (ChecklistServiceConfig(..), defaultChecklistServiceConfig) where

import Crud (DiskFileStorageConfig(..), CRUDEngine(..))
import Model (ChecklistContent(..))
import qualified NoteService as NoteService

data ChecklistServiceConfig = ChecklistServiceConfig String

instance DiskFileStorageConfig ChecklistServiceConfig where
    rootPath (ChecklistServiceConfig path) = path

instance CRUDEngine ChecklistServiceConfig ChecklistContent where
  getItems = NoteService.getAllItems
  postItem = NoteService.createItem
  delItem = NoteService.deleteItem
  putItem = NoteService.modifyItem
  crudTypeDenomination _ = "checklist"

defaultChecklistServiceConfig = ChecklistServiceConfig ".sharad/data/chekclist"
