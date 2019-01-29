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
import Servant (ServantErr (..), err403)
import Servant.Util (SimpleJSON)

import Dscp.Core.Validation
import Dscp.DB.SQL (SQLRequestsNumberExceeded)
import Dscp.Educator.DB (DomainError (..))
import Dscp.Educator.Web.Util ()
import Dscp.Web.Class
import Dscp.Web.Types

-- | Any error backend may return.
data StudentAPIError
    = SomeDomainError DomainError
      -- ^ Entity is missing or getting duplicated.
    | BadSubmissionSignature WrongSubmissionSignature
      -- ^ Submission signature doesn't match the student nor has valid format.
    | SomeGeneralBackendError GeneralBackendError
      -- ^ Common backend errors.
    deriving (Show, Eq, Generic, Typeable)

makePrisms ''StudentAPIError

instance Buildable StudentAPIError where
    build (BadSubmissionSignature err) =
        "Bad submission signature: " <> B.build err
    build (SomeDomainError err) =
        "Database error: " <> B.build err
    build (SomeGeneralBackendError err) = B.build err

instance Exception StudentAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , BadSubmissionSignature <$> fromException e
        , SomeDomainError        <$> fromException e
        , SomeGeneralBackendError . ServiceUnavailable .
             pretty @SQLRequestsNumberExceeded <$> fromException e
        ]

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''WrongSubmissionSignature
deriveJSON defaultOptions ''StudentAPIError

instance HasErrorTag WrongSubmissionSignature where
    errorTag = \case
        FakeSubmissionSignature{}    -> "FakeSubmissionSignature"
        SubmissionSignatureInvalid{} -> "SubmissionSignatureInvalid"

instance HasErrorTag StudentAPIError where
    errorTag = \case
        BadSubmissionSignature err -> errorTag err
        SomeDomainError err -> errorTag err
        SomeGeneralBackendError err -> errorTag err

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

instance ToServantErr WrongSubmissionSignature where
    toServantErrNoBody _ = err403

instance ToServantErr StudentAPIError where
    toServantErrNoBody = \case
        BadSubmissionSignature err -> toServantErrNoBody err
        SomeDomainError err -> toServantErrNoBody err
        SomeGeneralBackendError err -> toServantErrNoBody err

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
