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

import Universum

import Control.Lens (makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import Data.Swagger (ToSchema (..))
import Data.Typeable (cast)
import Fmt (Buildable (..), pretty)
import Servant.Server (ServerError (..), err403)
import Servant.Util (SimpleJSON)

import Dscp.Core.Validation
import Dscp.DB.SQL (SQLRequestsNumberExceeded)
import Dscp.Educator.DB (DomainError (..))
import Dscp.Educator.Web.Util ()
import Dscp.Util
import Dscp.Util.Constructors
import Dscp.Web.Class
import Dscp.Web.Swagger
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
        "Bad submission signature: " <> build err
    build (SomeDomainError err) =
        "Database error: " <> build err
    build (SomeGeneralBackendError err) = build err

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
        BadSubmissionSignature err  -> errorTag err
        SomeDomainError err         -> errorTag err
        SomeGeneralBackendError err -> errorTag err

---------------------------------------------------------------------------
-- Error instances
---------------------------------------------------------------------------

instance ToServantErr WrongSubmissionSignature where
    toServantErrNoBody _ = err403

instance ToServantErr StudentAPIError where
    toServantErrNoBody = \case
        BadSubmissionSignature err  -> toServantErrNoBody err
        SomeDomainError err         -> toServantErrNoBody err
        SomeGeneralBackendError err -> toServantErrNoBody err

instance FromServantErr StudentAPIError

---------------------------------------------------------------------------
-- Swagger instances
---------------------------------------------------------------------------

instance EnumHasDescription WrongSubmissionSignature where
    enumDocDescription p = errorCaseDocDesc @UnsafeFiller p $ \case
        SubmissionSignatureInvalid{} ->
            "Signature does not match to claimed submission author."
        FakeSubmissionSignature{} ->
            "Claimed owner of submission does not match to authenticated user."

instance EnumHasDescription StudentAPIError where
    enumDocDescription = gEnumDocDesc $ \case
        BadSubmissionSignature err  -> enumDocDescription (proxyOf err)
        SomeDomainError err         -> enumDocDescription (proxyOf err)
        SomeGeneralBackendError err -> enumDocDescription (proxyOf err)

instance ToSchema StudentAPIError where
    declareNamedSchema _ =
        declareSimpleSchema "ErrResponse" $
        errResponseSchema $ do
            setDocEnumDescription @StudentAPIError

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data FaucetDecodeErrTag
instance Reifies FaucetDecodeErrTag String where
    reflect _ = decodeUtf8 $ errBody $ toServantErr InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON FaucetDecodeErrTag
