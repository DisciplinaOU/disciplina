
-- | API errors

module Dscp.Educator.Web.Student.Error
       ( APIError (..)
       , WrongSubmissionSignature (..)

       , toServantErr
       , unexpectedToServantErr
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), encode, object, withObject, (.:), (.=))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import Servant (ServantErr (..), err403, err404)

import Dscp.Core.Aeson ()
import Dscp.DB.SQLite.Queries (DomainError (..))
import Dscp.Educator.BlockValidation (SubmissionValidationFailure)

data WrongSubmissionSignature
    = FakeSubmissionSignature
      -- ^ Signature doesn't match the student who performs the request.
    | SubmissionSignatureInvalid [SubmissionValidationFailure]
      -- ^ Submission is invalid on itself.
    deriving (Eq, Show)

data APIError
    = SubmissionMalformed
    | BadSubmissionSignature WrongSubmissionSignature
    | DeletingGradedSubmission
    | EntityAbsent DomainError
    deriving (Show, Eq, Generic, Typeable)

instance Exception APIError

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''WrongSubmissionSignature

instance FromJSON APIError where
    parseJSON = \case
        v@Object{}                          -> asum $ map ($ v)
            [ withObject "EntityAbsent" $ \o ->
                o .: "missing_entitry"
            , withObject "EntityAbsent" $ \o ->
                o .: "invalid_submission_signature"
            ]
        String "SubmissionMalformed"        -> pure SubmissionMalformed
        String "DeletingGradedSubmission"   -> pure DeletingGradedSubmission
        String other -> fail $ "invalid error constructor: " ++ toString other
        other -> typeMismatch "API error" other

instance ToJSON APIError where
    toJSON = \case
        SubmissionMalformed -> String "SubmissionMalformed"
        DeletingGradedSubmission -> String "DeletingGradedSubmission"
        BadSubmissionSignature err -> object
            [ "invalid_submission_signature" .= err
            ]
        EntityAbsent err -> object
            [ "missing_entity" .= err
            ]

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

-- | Get HTTP error code of error.
toServantErrNoReason :: APIError -> ServantErr
toServantErrNoReason = \case
    SubmissionMalformed{}      -> err403
    BadSubmissionSignature{}   -> err403
    DeletingGradedSubmission{} -> err403
    EntityAbsent{}             -> err404

-- | Make up error which will be returned to client.
toServantErr :: APIError -> ServantErr
toServantErr err = (toServantErrNoReason err){ errBody = encode err }

unexpectedToServantErr :: SomeException -> ServantErr
unexpectedToServantErr err = err500{ errBody = show err }
