module Dscp.Faucet.Web.Types
    ( GenKeysRequest (..)
    , GenKeysResponse (..)
    , TransferMoneyRequest (..)
    , TransferMoneyResponse (..)
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import qualified Data.Text.Buildable
import Fmt ((+|), (|+))

import Dscp.Core
import Dscp.Crypto
import Dscp.Util.Aeson
import Dscp.Util.Servant

-- | Wrapper to generate proper JSON instance for request body.
newtype GenKeysRequest = GenKeysRequest
    { gkrPassword :: Maybe (AsByteString Base64Encoded PassPhrase)
    }

data GenKeysResponse = GenKeysResponse
    { gkrEncSecretKey :: EncodeSerialised Base64Encoded (Encrypted SecretKey)
    , gkrSecretKey    :: AsByteString Base64Encoded SecretKey
    , gkrPublicKey    :: PublicKey
    , gkrAddress      :: Address
    }

-- | Wrapper to generate proper JSON instance for request body.
newtype TransferMoneyRequest = TransferMoneyRequest
    { tmrDestination :: Address
    }

data TransferMoneyResponse = TransferMoneyResponse
    { tmrTxId   :: AsByteString HexEncoded TxId
    , tmrAmount :: Coin
    }

----------------------------------------------------------------------------
-- Buildable instances
----------------------------------------------------------------------------

instance Buildable GenKeysRequest where
    build GenKeysRequest{..} = case gkrPassword of
        -- lets keep statistics :)
        Nothing -> "no password"
        Just _  -> "some password O_o"

instance Buildable (ForResponseLog GenKeysResponse) where
    build (ForResponseLog GenKeysResponse{..}) =
        "Keys for '" +| gkrAddress |+ "'"

instance Buildable TransferMoneyRequest where
    build TransferMoneyRequest{..} = tmrDestination |+ ""

instance Buildable (ForResponseLog TransferMoneyResponse) where
    build (ForResponseLog TransferMoneyResponse{..}) =
        getAsByteString tmrTxId |+ " <- " +| tmrAmount |+ ""

----------------------------------------------------------------------------
-- JSON instances
----------------------------------------------------------------------------

deriveFromJSON defaultOptions ''GenKeysRequest
deriveFromJSON defaultOptions ''TransferMoneyRequest
deriveToJSON defaultOptions ''GenKeysResponse
deriveToJSON defaultOptions ''TransferMoneyResponse
