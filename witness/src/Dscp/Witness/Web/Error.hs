{-# LANGUAGE StrictData #-}

module Dscp.Witness.Web.Error
    ( WitnessAPIError (..)
    , ErrResponse (..)
    , DSON
    , witnessToServantErr
    ) where

import Data.Aeson (encode)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import qualified Data.Text.Buildable as B
import Data.Typeable (cast)
import Servant (ServantErr (..), err400, err404, err500, err503)

import Dscp.Snowdrop
import Dscp.Util.Servant
import Dscp.Web.Class
import Dscp.Witness.Relay (RelayException)

data WitnessAPIError
    = BlockNotFound
    | TransactionNotFound
    | TxError AccountValidationException
    | InternalError Text
    | ServiceUnavailable Text
    | InvalidFormat
    deriving (Show, Generic, Typeable)

instance Buildable WitnessAPIError where
    build = \case
        BlockNotFound -> "Specified block does not exist."
        TransactionNotFound -> "Specified transaction does not exist."
        TxError err -> B.build err
        InternalError msg -> "Internal error: " <> B.build msg
        ServiceUnavailable msg -> "Service unavailable: " <> B.build msg
        InvalidFormat -> "Failed to deserialise one of parameters."

fromSnowdropException :: Exceptions -> WitnessAPIError
fromSnowdropException = \case
    AccountValidationError e -> TxError e
    other -> InternalError $ pretty other

instance Exception WitnessAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , fromSnowdropException <$> fromException e
        , ServiceUnavailable . show @Text @RelayException <$> fromException e
        ]

instance HasErrorTag WitnessAPIError where
    errorTag = \case
        BlockNotFound -> "BlockNotFound"
        TransactionNotFound -> "TransactionNotFound"
        TxError err -> errorTag err
        InternalError{} -> "InternalError"
        ServiceUnavailable{} -> "ServiceUnavailable"
        InvalidFormat -> "InvalidFormat"

-- | Contains info about error in client-convenient form.
data ErrResponse = ErrResponse
    { erError   :: !Text
      -- ^ Enum which identifies type of error, for frontend
    , erContent :: !WitnessAPIError
      -- ^ Error itself, to allow client deserialise the error
    } deriving (Show, Generic)

toErrResponse :: WitnessAPIError -> ErrResponse
toErrResponse err =
    ErrResponse
    { erContent = err
    , erError = errorTag err
    }

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''WitnessAPIError
deriveJSON defaultOptions ''ErrResponse

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

instance ToServantErrNoReason WitnessAPIError where
    toServantErrNoReason = \case
        BlockNotFound        -> err404
        TransactionNotFound  -> err404
        TxError err          -> toServantErrNoReason err
        InternalError{}      -> err500
        ServiceUnavailable{} -> err503
        InvalidFormat        -> err400

-- | Make up error which will be returned to client.
witnessToServantErr :: WitnessAPIError -> ServantErr
witnessToServantErr err =
    (toServantErrNoReason err){ errBody = encode $ toErrResponse err }

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data WitnessDecodeErrTag
instance Reifies WitnessDecodeErrTag String where
    reflect _ = decodeUtf8 $ encode InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON WitnessDecodeErrTag
