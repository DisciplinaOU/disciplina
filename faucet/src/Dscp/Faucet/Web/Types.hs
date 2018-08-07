module Dscp.Faucet.Web.Types
    ( GenKeysRequest (..)
    , GenKeysResponse (..)
    , TransferMoneyRequest (..)
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
    { gkrEncSecretKey :: CustomEncoding Base64Encoded (Encrypted SecretKey)
    , gkrPublicKey    :: PublicKey
    , gkrAddress      :: Address
    }

-- | Wrapper to generate proper JSON instance for request body.
newtype TransferMoneyRequest = TransferMoneyRequest
    { tmrDestination :: Address
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

----------------------------------------------------------------------------
-- JSON instances
----------------------------------------------------------------------------

deriveFromJSON defaultOptions ''GenKeysRequest
deriveFromJSON defaultOptions ''TransferMoneyRequest
deriveToJSON defaultOptions ''GenKeysResponse
