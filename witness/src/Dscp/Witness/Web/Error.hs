{-# LANGUAGE StrictData #-}

module Dscp.Witness.Web.Error
    ( WitnessAPIError (..)
    , ErrResponse (..)
    , DSON
    , witnessToServantErr
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), encode, withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import qualified Data.Text.Buildable as B
import Data.Typeable (cast)
import Servant (ServantErr (..), err400, err404, err500)

import Dscp.Util.Servant

data WitnessAPIError
    = BlockNotFound
    | TransactionNotFound
    | InternalError Text
    | InvalidFormat
    deriving (Show, Eq, Generic, Typeable)

instance Buildable WitnessAPIError where
    build = \case
        BlockNotFound -> "Specified block does not exist."
        TransactionNotFound -> "Specified transaction does not exist."
        InternalError msg -> B.build msg
        InvalidFormat -> "Failed to deserialise one of parameters."

instance Exception WitnessAPIError where
    fromException (SomeException e') =
        asum
        [ cast e'
        ]

-- | Contains info about error in client-convenient form.
data ErrResponse = ErrResponse
    { erError :: !WitnessAPIError
    } deriving (Show, Eq, Generic)

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''ErrResponse

instance ToJSON WitnessAPIError where
    toJSON = String . \case
        BlockNotFound -> "BlockNotFound"
        TransactionNotFound -> "TransactionNotFound"
        InternalError msg -> msg
        InvalidFormat -> "InvalidFormat"

instance FromJSON WitnessAPIError where
    parseJSON = withText "error" $ pure . \case
        "BlockNotFound" -> BlockNotFound
        "TransactionNotFound" -> TransactionNotFound
        "InvalidFormat" -> InvalidFormat
        msg -> InternalError msg

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

-- | Get HTTP error code of error.
toServantErrNoReason :: WitnessAPIError -> ServantErr
toServantErrNoReason = \case
    BlockNotFound       -> err404
    TransactionNotFound -> err404
    InternalError{}     -> err500
    InvalidFormat       -> err400

-- | Make up error which will be returned to client.
witnessToServantErr :: WitnessAPIError -> ServantErr
witnessToServantErr err = (toServantErrNoReason err){ errBody = encode $ ErrResponse err }

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data WitnessDecodeErrTag
instance Reifies WitnessDecodeErrTag String where
    reflect _ = decodeUtf8 $ encode InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON WitnessDecodeErrTag
