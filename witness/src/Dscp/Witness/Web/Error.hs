{-# LANGUAGE StrictData #-}

module Dscp.Witness.Web.Error
    ( WitnessAPIError (..)
    , ErrResponse (..)
    , DSON
    ) where

import Data.Aeson (encode)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import qualified Data.Text.Buildable as B
import Data.Typeable (cast)
import Servant (err400, err500, err503)
import Servant.Util (SimpleJSON)

import Dscp.Snowdrop
import Dscp.Web.Class
import Dscp.Witness.Logic.Exceptions
import Dscp.Witness.Relay (RelayException)

-- | All kinds of SD exceptions which we care about.
data SdExceptions
    = SdAccountError AccountException
    | SdLogicError LogicException
    deriving (Show)

instance Buildable SdExceptions where
    build = \case
        SdAccountError err -> B.build err
        SdLogicError err -> B.build err

fromSnowdropException :: Exceptions -> Maybe SdExceptions
fromSnowdropException = \case
    AccountError e -> pure $ SdAccountError e
    LogicError e -> pure $ SdLogicError e
    PublicationError{} -> mzero
    BlockError{} -> mzero
    SdInternalError{} -> mzero

-- | All witness API exceptions.
data WitnessAPIError
    = SdError SdExceptions
    | InternalError Text
    | ServiceUnavailable Text
    | InvalidFormat
    deriving (Show, Generic, Typeable)

instance Buildable WitnessAPIError where
    build = \case
        SdError err -> B.build err
        InternalError msg -> "Internal error: " <> B.build msg
        ServiceUnavailable msg -> "Service unavailable: " <> B.build msg
        InvalidFormat -> "Failed to deserialise one of parameters."

instance Exception WitnessAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , fmap SdError . fromSnowdropException =<< fromException e
        , ServiceUnavailable . show @Text @RelayException <$> fromException e
        ]

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''SdExceptions
deriveJSON defaultOptions ''WitnessAPIError

---------------------------------------------------------------------------
-- Error instances
---------------------------------------------------------------------------

instance HasErrorTag SdExceptions where
    errorTag = \case
        SdAccountError e -> errorTag e
        SdLogicError e -> errorTag e

instance HasErrorTag WitnessAPIError where
    errorTag = \case
        SdError err -> errorTag err
        InternalError{} -> "InternalError"
        ServiceUnavailable{} -> "ServiceUnavailable"
        InvalidFormat -> "InvalidFormat"

instance ToServantErr SdExceptions where
    toServantErrNoBody = \case
        SdAccountError e -> toServantErrNoBody e
        SdLogicError e -> toServantErrNoBody e

instance ToServantErr WitnessAPIError where
    toServantErrNoBody = \case
        SdError err          -> toServantErrNoBody err
        InternalError{}      -> err500
        ServiceUnavailable{} -> err503
        InvalidFormat        -> err400

instance FromServantErr WitnessAPIError

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data WitnessDecodeErrTag
instance Reifies WitnessDecodeErrTag String where
    reflect _ = decodeUtf8 $ encode InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON WitnessDecodeErrTag
