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
import qualified Data.Text as T
import qualified Data.Text.Buildable as B
import Data.Typeable (cast)
import Servant (ServantErr (..), err400, err404, err500, err503)

import Dscp.Snowdrop
import Dscp.Util.Servant
import Dscp.Witness.Web.Util

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

instance Exception WitnessAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , fmap TxError . (^? _AccountValidationError) =<< fromException e
        ]

-- | Contains info about error in client-convenient form.
data ErrResponse = ErrResponse
    { erError :: !WitnessAPIError
    } deriving (Show, Generic)

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''ErrResponse

uaPrefix :: Text
uaPrefix = "<unavailable>"

prefixUnavailable :: Text -> Text
prefixUnavailable = (<>) uaPrefix

unprefixUnavailable :: Text -> Maybe Text
unprefixUnavailable txt =
    if T.take l txt == uaPrefix
    then Just $ T.drop l txt
    else Nothing
  where
    l = length uaPrefix

instance ToJSON WitnessAPIError where
    toJSON = String . \case
        BlockNotFound -> "BlockNotFound"
        TransactionNotFound -> "TransactionNotFound"
        TxError err -> snowdropErrorToShortJSON err
        InternalError msg -> msg
        ServiceUnavailable msg -> prefixUnavailable msg
        InvalidFormat -> "InvalidFormat"

instance FromJSON WitnessAPIError where
    parseJSON = withText "error" $ pure . \case
        "BlockNotFound" -> BlockNotFound
        "TransactionNotFound" -> TransactionNotFound
        "InvalidFormat" -> InvalidFormat
        msg | Just err <- parseShortJSONToSnowdropError msg -> TxError err
            | Just err <- unprefixUnavailable msg -> ServiceUnavailable err
            | otherwise -> InternalError msg

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

-- | Get HTTP error code of error.
toServantErrNoReason :: WitnessAPIError -> ServantErr
toServantErrNoReason = \case
    BlockNotFound        -> err404
    TransactionNotFound  -> err404
    TxError err          -> snowdropToServantErrNoReason err
    InternalError{}      -> err500
    ServiceUnavailable{} -> err503
    InvalidFormat        -> err400

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
