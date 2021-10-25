{-# LANGUAGE MultiParamTypeClasses #-}

module NoteCrud (NoteServiceConfig(..)) where

import Crud (DiskFileStorageConfig(..), CRUDEngine(..))
import Model (NoteContent(..))
import qualified NoteService as NoteService

data NoteServiceConfig = NoteServiceConfig

instance DiskFileStorageConfig NoteServiceConfig where
    rootPath NoteServiceConfig = ".sharad/data/note"

instance CRUDEngine NoteServiceConfig NoteContent where
  getItems = NoteService.getAllItems
  postItem = NoteService.createItem
  delItem = NoteService.deleteItem
  putItem = NoteService.modifyItem
  crudTypeDenomination NoteServiceConfig = "note"

