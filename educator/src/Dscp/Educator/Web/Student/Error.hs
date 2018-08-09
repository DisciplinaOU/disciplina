-- | Student API errors

module Dscp.Educator.Web.Student.Error
       ( APIError (..)
       , _BadSubmissionSignature
       , _DeletingGradedSubmission
       , _SomeDomainError
       , WrongSubmissionSignature (..)
       , _FakeSubmissionSignature
       , _SubmissionSignatureInvalid

       , ErrResponse (..)

       , DSON

       , toServantErr
       , unexpectedToServantErr
       ) where

import Control.Lens (makePrisms)
import Data.Aeson (ToJSON (..), Value (..), encode)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.Reflection (Reifies (..))
import Data.Typeable (cast)
import Servant (ServantErr (..), err400, err403, err500)

import Dscp.DB.SQLite.Queries (DomainError (..))
import Dscp.Educator.BlockValidation (SubmissionValidationFailure)
import Dscp.Educator.Web.Util
import Dscp.Util.Servant

-- | Any issues with submission signature content.
data WrongSubmissionSignature
    = FakeSubmissionSignature
      -- ^ Signature doesn't match the student who performs the request.
    | SubmissionSignatureInvalid [SubmissionValidationFailure]
      -- ^ Submission is invalid on itself.
    deriving (Eq, Show)

makePrisms ''WrongSubmissionSignature

instance Exception WrongSubmissionSignature

-- | Any error backend may return.
data APIError
    = BadSubmissionSignature WrongSubmissionSignature
      -- ^ Submission signature doesn't match the student nor has valid format.
    | DeletingGradedSubmission
      -- ^ Graded Submission is being deleted.
    | SomeDomainError DomainError
      -- ^ Entity is missing or getting duplicated.
    | InvalidFormat
      -- ^ Decoding failed.
    deriving (Show, Eq, Generic, Typeable)

makePrisms ''APIError

instance Exception APIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , BadSubmissionSignature <$> fromException e
        , SomeDomainError        <$> fromException e
        ]

-- | Contains info about error in client-convenient form.
data ErrResponse = ErrResponse
    { erError :: !APIError
    } deriving (Show, Eq, Generic)

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''WrongSubmissionSignature
deriveToJSON defaultOptions ''ErrResponse

instance ToJSON APIError where
    toJSON = String . \case
        BadSubmissionSignature err -> case err of
            FakeSubmissionSignature{}    -> "FakeSubmissionSignature"
            SubmissionSignatureInvalid{} -> "SubmissionSignatureInvalid"
        DeletingGradedSubmission{} ->       "DeletingGradedSubmission"
        InvalidFormat ->                    "InvalidFormat"
        SomeDomainError err -> domainErrorToShortJSON err

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

-- | Get HTTP error code of error.
toServantErrNoReason :: APIError -> ServantErr
toServantErrNoReason = \case
    BadSubmissionSignature{}   -> err403
    DeletingGradedSubmission{} -> err403
    InvalidFormat{}            -> err400
    SomeDomainError err -> domainToServantErrNoReason err

-- | Make up error which will be returned to client.
toServantErr :: APIError -> ServantErr
toServantErr err = (toServantErrNoReason err){ errBody = encode $ ErrResponse err }

unexpectedToServantErr :: SomeException -> ServantErr
unexpectedToServantErr err = err500{ errBody = show err }

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data FaucetDecodeErrTag
instance Reifies FaucetDecodeErrTag String where
    reflect _ = decodeUtf8 $ encode InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON FaucetDecodeErrTag
