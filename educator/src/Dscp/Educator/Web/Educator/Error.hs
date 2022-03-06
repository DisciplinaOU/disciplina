-- | Educator API errors

module Dscp.Educator.Web.Educator.Error
       ( EducatorAPIError (..)
       , ThirdPartyRequestError (..)

       , ErrResponse (..)

       , DSON

       , toServantErr
       , unexpectedToServantErr
       ) where

import Universum
import Control.Lens (makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import Data.Swagger (ToSchema (..))
import Data.Typeable (cast)
import Fmt (Buildable (..), (+|), (|+), pretty)
import Servant.Server (ServerError (..), err502)
import Servant.Util (SimpleJSON)

import Dscp.DB.SQL (SQLRequestsNumberExceeded)
import Dscp.Educator.DB (DomainError)
import Dscp.Educator.Web.Util ()
import Dscp.Util
import Dscp.Util.Constructors
import Dscp.Web.Class
import Dscp.Web.Swagger
import Dscp.Web.Types

-- | Error resulting from requests made to a third-party service
data ThirdPartyRequestError
    = NonPositiveStatusCodeError Int
    | NonDecodableResponseError String
    deriving (Show, Eq, Generic)

instance Exception ThirdPartyRequestError

-- | Any error backend may return.
data EducatorAPIError
    = SomeDomainError DomainError
      -- ^ Something not found or already exists.
    | SomeGeneralBackendError GeneralBackendError
      -- ^ Common backend errors.
    | SomeThirdPartyRequestError ThirdPartyRequestError
      -- ^ Errors from requests to third-party services
    deriving (Show, Eq, Generic)

makePrisms ''EducatorAPIError

instance Buildable EducatorAPIError where
    build (SomeDomainError err) =
        "Database error: " <> build err
    build (SomeGeneralBackendError err) = build err
    build (SomeThirdPartyRequestError err) = build err

instance Exception EducatorAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , SomeDomainError <$> fromException e
        , SomeGeneralBackendError . ServiceUnavailable .
             pretty @SQLRequestsNumberExceeded <$> fromException e
        , SomeThirdPartyRequestError <$> fromException e
        ]

instance Buildable ThirdPartyRequestError where
    build = \case
      NonPositiveStatusCodeError statusCode ->
          "Expected a 2xx status code, received "+|statusCode|+" instead"
      NonDecodableResponseError err ->
          "Could not decode the response body: " <> build err


---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance HasErrorTag EducatorAPIError where
    errorTag = \case
        SomeDomainError err -> errorTag err
        SomeGeneralBackendError err -> errorTag err
        SomeThirdPartyRequestError err -> errorTag err

instance HasErrorTag ThirdPartyRequestError where
    errorTag = \case
        NonPositiveStatusCodeError _ -> "NonPositiveStatusCodeError"
        NonDecodableResponseError _ -> "NonDecodableResponseError"

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

deriveJSON defaultOptions ''ThirdPartyRequestError

deriveJSON defaultOptions ''EducatorAPIError

instance ToServantErr EducatorAPIError where
    toServantErrNoBody = \case
        SomeDomainError err -> toServantErrNoBody err
        SomeGeneralBackendError err -> toServantErrNoBody err
        SomeThirdPartyRequestError err -> toServantErrNoBody err

instance ToServantErr ThirdPartyRequestError where
    toServantErrNoBody = \case
        NonPositiveStatusCodeError _ -> err502
        NonDecodableResponseError _ -> err502

instance FromServantErr EducatorAPIError

---------------------------------------------------------------------------
-- Swagger instances
---------------------------------------------------------------------------

instance EnumHasDescription EducatorAPIError where
    enumDocDescription = gEnumDocDesc $ \case
        SomeDomainError err -> enumDocDescription (proxyOf err)
        SomeGeneralBackendError err -> enumDocDescription (proxyOf err)
        SomeThirdPartyRequestError err -> enumDocDescription (proxyOf err)

instance EnumHasDescription ThirdPartyRequestError where
    enumDocDescription t = errorCaseDocDesc @UnsafeFiller t $ \case
        NonPositiveStatusCodeError _ ->
            "The status code of the received response was not 2xx"
        NonDecodableResponseError _ ->
            "The body of the received response could not be decoded"

instance ToSchema EducatorAPIError where
    declareNamedSchema _ =
        declareSimpleSchema "ErrResponse" $
        errResponseSchema $ do
            setDocEnumDescription @EducatorAPIError

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data EducatorDecodeErrTag
instance Reifies EducatorDecodeErrTag String where
    reflect _ = decodeUtf8 . errBody $ toServantErr InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON EducatorDecodeErrTag
