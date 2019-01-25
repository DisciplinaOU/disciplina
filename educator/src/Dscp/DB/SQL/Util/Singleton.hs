{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.DB.SQL.Util.Singleton
     ( SingletonT (..)
     , getSingleton_
     , setSingleton_
     , loadSingleton_
     ) where

import Prelude hiding (_1, _2)

import Data.Typeable (typeRep)
import Database.Beam.Schema (PrimaryKey, TableEntity)
import qualified Database.Beam.Schema as Beam
import qualified Text.Show

import Dscp.DB.SQL.Functions
import Dscp.DB.SQL.Util.Common
import Dscp.Util

-- | Stores a single value.
data SingletonT a (f :: * -> *) = SingletonUnsafe
    { singletonId   :: Beam.C f Text
      -- ^ Primary key, Beam cannot live without it.
      -- Also it allows to easily extend the table to keep multiple elements.
    , singletonItem :: Beam.C f a
      -- ^ The stored item itself.
    } deriving (Generic)

instance Typeable a => Beam.Table (SingletonT a) where
    data PrimaryKey (SingletonT a) f = SingletonId (Beam.C f Text)
        deriving (Generic)
    primaryKey = SingletonId . singletonId

instance Beam.Beamable (SingletonT a)
instance Beam.Beamable (PrimaryKey (SingletonT a))

defSingletonId :: Text
defSingletonId = "main"

-- | Singleton remained uninitialized.
data SingletonIsNotInit a = SingletonIsNotInit

instance Typeable a => Show (SingletonIsNotInit a) where
    show _ = "Singleton value for " <> show (typeRep (Proxy @a)) <> " is not initialized"

instance Typeable a => Exception (SingletonIsNotInit a)

getSingleton_
    :: (MonadIO m, _)
    => Beam.DatabaseEntity be db (TableEntity (SingletonT a))
    -> DBT t m (Maybe a)
getSingleton_ tbl = selectByPk singletonItem tbl defSingletonId

setSingleton_
    :: (MonadIO m, _)
    => Beam.DatabaseEntity be db (TableEntity (SingletonT a))
    -> a
    -> DBT t m ()
setSingleton_ tbl val =
    runInsert . insert tbl $ insertValue
        SingletonUnsafe
        { singletonId = defSingletonId
        , singletonItem = val
        }

-- | Load the singleton value, initizing it with provided function when absent.
-- If singleton value is absent and no action is provided to initialize it
-- then exception is thrown.
loadSingleton_
    :: forall be db m a.
       (MonadIO m, MonadThrow m, _)
    => Beam.DatabaseEntity be db (TableEntity (SingletonT a))
    -> MaybeT (DBT 'WithinTx m) a
    -> DBT 'WithinTx m a
loadSingleton_ tbl onAbsent = do
    mvalue <- getSingleton_ tbl
    case mvalue of
        Just value -> return value
        Nothing -> do
            value <- runMaybeT onAbsent `assertJust` SingletonIsNotInit @a
            setSingleton_ tbl value
            return value
