{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Crud (DiskFileStorageConfig(..), CRUDEngine(..), Error(..)) where 

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Model (Content(..), StorageId(..), Identifiable(..))

class DiskFileStorageConfig a where
    rootPath :: a -> String

class (DiskFileStorageConfig crudType, Content a) => CRUDEngine crudType a | crudType -> a where
  getItems :: crudType -> ExceptT Error IO [Identifiable a]
  postItem :: crudType -> a -> MaybeT IO StorageId
  delItem :: crudType -> String -> IO (Maybe Error)
  putItem :: crudType -> Identifiable a -> ExceptT Error IO StorageId
  crudTypeDenomination :: crudType -> String

data Error = NotFound FilePath | NotCurrentVersion StorageId | FatalError String deriving(Eq, Show)

