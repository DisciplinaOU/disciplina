module Dscp.DB.SQLite.Types
       ( -- * SQLite bindings
         SQLiteDBLocation (..)
       , SQLiteDB (..)
       , SQLiteParams (..)

         -- * Educator schema
       , TxBlockIdx (..)
       , intTxBlockIdx

         -- * SQL errors
       , asAlreadyExistsError
       ) where

import Control.Lens (Prism', prism)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Error (..), SQLError (..))

----------------------------------------------------------
-- SQLite bindings
----------------------------------------------------------

-- | Where database lies.
data SQLiteDBLocation
    = SQLiteReal !FilePath  -- ^ In given file
    | SQLiteInMemory        -- ^ In memory

data SQLiteParams = SQLiteParams
    { sdpLocation :: SQLiteDBLocation
    }

newtype SQLiteDB = SQLiteDB { sdConn :: Connection }

----------------------------------------------------------
-- Educator schema
----------------------------------------------------------

-- | Schema internal: idx of transaction within block.
data TxBlockIdx
    = TxBlockIdx Word32
    | TxInMempool
    deriving (Eq)

-- | Convert between 'TxBlockIdx' and true index which can be used in database.
intTxBlockIdx :: Prism' Int TxBlockIdx
intTxBlockIdx = prism toInt fromInt
  where
    toInt = \case
        TxBlockIdx idx -> fromIntegral idx
        TxInMempool    -> -1
    fromInt idx
        | idx >= 0 = Right $ TxBlockIdx (fromIntegral idx)
        | idx == -1 = Right $ TxInMempool
        | otherwise = Left idx

----------------------------------------------------------
-- Errors
----------------------------------------------------------

-- | Matches on errors declaring violation of UNIQUE constraint,
-- returns name of fields on which constraint was violated.
asAlreadyExistsError :: SQLError -> Maybe Text
asAlreadyExistsError err = do
    SQLError ErrorConstraint details _ <- pure err
    let pat = "UNIQUE constraint failed"
    guard $ pat `T.isPrefixOf` details
    return $ T.drop (length pat) details
