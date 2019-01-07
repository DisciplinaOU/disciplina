-- | Student API errors

module Dscp.Educator.Web.Student.Error
       ( StudentAPIError (..)
       , _BadSubmissionSignature
       , _SomeDomainError
       , WrongSubmissionSignature (..)
       , _FakeSubmissionSignature
       , _SubmissionSignatureInvalid

       , DSON
       ) where

import Control.Lens (makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import qualified Data.Text.Buildable as B
import Data.Typeable (cast)
import Servant (ServantErr (..), err400, err403, err503)

import Dscp.Core.Validation
import Dscp.DB.SQLite (SQLRequestsNumberExceeded)
import Dscp.Educator.DB (DomainError (..))
import Dscp.Educator.Web.Util
import Dscp.Util.Servant
import Dscp.Web.Class

-- | Any error backend may return.
data StudentAPIError
    = BadSubmissionSignature WrongSubmissionSignature
      -- ^ Submission signature doesn't match the student nor has valid format.
    | SomeDomainError DomainError
      -- ^ Entity is missing or getting duplicated.
    | InvalidFormat
      -- ^ Decoding failed.
    | ServiceUnavailable !Text
      -- ^ Service is overloaded with requests.
    deriving (Show, Eq, Generic, Typeable)

makePrisms ''StudentAPIError

instance Buildable StudentAPIError where
    build (BadSubmissionSignature err) =
        "Bad submission signature: " <> B.build err
    build (SomeDomainError err) =
        "Database error: " <> B.build err
    build InvalidFormat =
        "Invalid format of the request"
    build (ServiceUnavailable msg) =
        "Service unavailable: " <> B.build msg

instance Exception StudentAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , BadSubmissionSignature <$> fromException e
        , SomeDomainError        <$> fromException e
        , ServiceUnavailable . pretty @SQLRequestsNumberExceeded <$> fromException e
        ]

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''WrongSubmissionSignature
deriveJSON defaultOptions ''StudentAPIError

instance HasErrorTag StudentAPIError where
    errorTag = \case
        BadSubmissionSignature err -> case err of
            FakeSubmissionSignature{}    -> "FakeSubmissionSignature"
            SubmissionSignatureInvalid{} -> "SubmissionSignatureInvalid"
        InvalidFormat ->        "InvalidFormat"
        ServiceUnavailable{} -> "ServiceUnavailable"
        SomeDomainError err -> domainErrorToShortJSON err

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

instance ToServantErr StudentAPIError where
    toServantErrNoBody = \case
        BadSubmissionSignature{} -> err403
        InvalidFormat{}          -> err400
        ServiceUnavailable{}     -> err503
        SomeDomainError err -> domainToServantErrNoReason err
    toServantErr = toServantErrJustTag

instance FromServantErr StudentAPIError

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data FaucetDecodeErrTag
instance Reifies FaucetDecodeErrTag String where
    reflect _ = decodeUtf8 $ errBody $ toServantErr InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON FaucetDecodeErrTag
