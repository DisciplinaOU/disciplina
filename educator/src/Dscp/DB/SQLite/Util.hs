{-# LANGUAGE ExistentialQuantification #-}

-- | Utilities for writing SQLite queries.

module Dscp.DB.SQLite.Util
     (
       -- * Filtering
       FilterClause (..)
     , SomeParams (..)
     , oneParam
     , mkFilter
     , filterClauses

       -- * Conditional fields extraction
     , FetchIf
     , positiveFetch
     ) where

import Data.Singletons.Bool (SBoolI, fromSBool, sbool)
import Database.SQLite.Simple ((:.) (..), FromRow (..), Only (..), Query, ToRow (..))
import Database.SQLite.Simple.ToField (ToField)

---------------------------------------------------------------------
-- Filtering
---------------------------------------------------------------------

-- | One of conditions within conjenction sum after @where@.
newtype FilterClause = FilterClause { unFilterClause :: String }
    deriving (IsString)

-- | Contains a list of objects forming a row in SQL table.
data SomeParams = forall a. ToRow a => SomeParams a

instance Semigroup SomeParams where
    SomeParams a <> SomeParams b = SomeParams (a :. b)

instance Monoid SomeParams where
    mempty = SomeParams ()
    mappend = (<>)

instance ToRow SomeParams where
    toRow (SomeParams a) = toRow a

-- | Lift a single field to 'SomeParams'.
oneParam :: ToField a => a -> SomeParams
oneParam = SomeParams . Only

-- | For SQL table field and optional value, makes a clause to select
-- only rows with given condition (if corresponding filter is present).
-- This function helps to deal with sqlite feature of passing parameters
-- seperately from query itself.
mkFilter :: ToField a => String -> Maybe a -> (FilterClause, SomeParams)
mkFilter filterClause mparam = case mparam of
      Just param -> (FilterClause ("and " <> filterClause),
                     oneParam param)
      Nothing    -> ("", mempty)

-- | Attaches filtering @where@ clauses to query.
-- "where" statement with some condition should go last in the query.
filterClauses :: Query -> [FilterClause] -> Query
filterClauses queryText fs =
    mconcat $ queryText : map (fromString . unFilterClause) fs
infixl 1 `filterClauses`

---------------------------------------------------------------------
-- Fields selection
---------------------------------------------------------------------

-- | Pack of fields which are only parsed if 'required' is set to 'True'.
newtype FetchIf (required :: Bool) a = FetchIf (Maybe a)

-- | Get the value if it was required.
positiveFetch :: (required ~ 'True) => FetchIf required a -> a
positiveFetch (FetchIf a) = a ?: error "positiveFetch: Nothing"

instance (FromRow a, SBoolI required) => FromRow (FetchIf required a) where
    fromRow
        | fromSBool (sbool @required) = FetchIf . Just <$> fromRow
        | otherwise                   = pure (FetchIf Nothing)
