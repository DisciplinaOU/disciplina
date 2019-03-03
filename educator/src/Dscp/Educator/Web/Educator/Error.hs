-- | Educator API errors

module Dscp.Educator.Web.Educator.Error
       ( EducatorAPIError (..)
       , ThirdPartyRequestError (..)

       , ErrResponse (..)

       , DSON

       , toServantErr
       , unexpectedToServantErr
       ) where

import Control.Lens (makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import Data.Swagger (ToSchema (..))
import Data.Typeable (cast)
import Fmt (build, (+|), (|+))
import Servant (ServantErr (..))
import Servant.Util (SimpleJSON)

import Dscp.DB.SQL (SQLRequestsNumberExceeded)
import Dscp.Educator.DB (DomainError)
import Dscp.Educator.Web.Util ()
import Dscp.Util
import Dscp.Web.Class
import Dscp.Web.Swagger
import Dscp.Web.Types

-- | Any error backend may return.
data EducatorAPIError
    = SomeDomainError DomainError
      -- ^ Something not found or already exists.
    | SomeGeneralBackendError GeneralBackendError
      -- ^ Common backend errors.
    deriving (Show, Eq, Generic)

makePrisms ''EducatorAPIError

instance Buildable EducatorAPIError where
    build (SomeDomainError err) =
        "Database error: " <> build err
    build (SomeGeneralBackendError err) = build err

instance Exception EducatorAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , SomeDomainError <$> fromException e
        , SomeGeneralBackendError . ServiceUnavailable .
             pretty @SQLRequestsNumberExceeded <$> fromException e
        ]

-- | Error resulting from requests made with a third-party service
data ThirdPartyRequestError
    = NonPositiveStatusCodeError Int
    | NonDecodableResponseError String
    deriving (Show, Eq, Generic)

instance Buildable ThirdPartyRequestError where
    build = \case
      NonPositiveStatusCodeError statusCode ->
          "Expected a 200 status code, received"+|statusCode|+"instead"
      NonDecodableResponseError err ->
          "Could not decode the response body" <> build err

instance Exception ThirdPartyRequestError

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance HasErrorTag EducatorAPIError where
    errorTag = \case
        SomeDomainError err -> errorTag err
        SomeGeneralBackendError err -> errorTag err

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

deriveJSON defaultOptions ''EducatorAPIError

instance ToServantErr EducatorAPIError where
    toServantErrNoBody = \case
        SomeDomainError err  -> toServantErrNoBody err
        SomeGeneralBackendError err  -> toServantErrNoBody err

instance FromServantErr EducatorAPIError

---------------------------------------------------------------------------
-- Swagger instances
---------------------------------------------------------------------------

instance EnumHasDescription EducatorAPIError where
    enumDocDescription = gEnumDocDesc $ \case
        SomeDomainError err -> enumDocDescription (proxyOf err)
        SomeGeneralBackendError err -> enumDocDescription (proxyOf err)

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
