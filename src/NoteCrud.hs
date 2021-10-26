{-# LANGUAGE MultiParamTypeClasses #-}

module NoteCrud (NoteServiceConfig(..)) where

import Crud (DiskFileStorageConfig(..), CRUDEngine(..))
import Model (NoteContent(..))
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

