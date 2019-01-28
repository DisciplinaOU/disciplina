-- | Educator API errors

module Dscp.Educator.Web.Educator.Error
       ( EducatorAPIError (..)

       , ErrResponse (..)

       , DSON

       , toServantErr
       , unexpectedToServantErr
       ) where

import Control.Lens (makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import qualified Data.Text.Buildable as B
import Data.Typeable (cast)
import Dscp.DB.SQL (SQLRequestsNumberExceeded)
import Dscp.Educator.DB (DomainError)
import Servant (ServantErr (..), err400, err503)
import Servant.Util (SimpleJSON)

import Dscp.Educator.Web.Util
import Dscp.Web.Class

-- | Any error backend may return.
data EducatorAPIError
    = SomeDomainError DomainError
      -- ^ Something not found or already exists.
    | InvalidFormat
      -- ^ Failed to decode something.
    | ServiceUnavailable !Text
      -- ^ Service is overloaded with requests.
    deriving (Show, Eq, Generic)

makePrisms ''EducatorAPIError

instance Buildable EducatorAPIError where
    build (SomeDomainError err) =
        "Database error: " <> B.build err
    build InvalidFormat =
        "Invalid format of the request"
    build (ServiceUnavailable msg) =
        "Service unavailable: " <> B.build msg

instance Exception EducatorAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , SomeDomainError <$> fromException e
        , ServiceUnavailable . pretty @SQLRequestsNumberExceeded <$> fromException e
        ]

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance HasErrorTag EducatorAPIError where
    errorTag = \case
        InvalidFormat        -> "InvalidFormat"
        SomeDomainError err  -> domainErrorToShortJSON err
        ServiceUnavailable{} -> "ServiceUnavailable"

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

deriveJSON defaultOptions ''EducatorAPIError

instance ToServantErr EducatorAPIError where
    toServantErrNoBody = \case
        InvalidFormat        -> err400
        ServiceUnavailable{} -> err503
        SomeDomainError err  -> domainToServantErrNoReason err
    toServantErr = toServantErrJustTag

instance FromServantErr EducatorAPIError

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data EducatorDecodeErrTag
instance Reifies EducatorDecodeErrTag String where
    reflect _ = decodeUtf8 . errBody $ toServantErr InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON EducatorDecodeErrTag
