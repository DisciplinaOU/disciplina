
-- | API errors

module Dscp.Educator.Web.Student.Error
       ( APIError (..)

       , toServantErr
       , unexpectedToServantErr
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), encode, object, (.=))
import Data.Aeson.Types (typeMismatch)
import Servant (ServantErr (..), err403, err404)

import Dscp.Core.Aeson ()
import Dscp.DB.SQLite.Queries (DomainError (..))

data APIError
    = SubmissionMalformed
    | SubmissionSignatureInvalid
    | DeletingGradedSubmission
    | EntityAbsent DomainError
    deriving (Show, Eq, Generic, Typeable)

instance Exception APIError

instance FromJSON APIError where
    parseJSON = \case
        o@Object{}                          -> EntityAbsent <$> parseJSON o
        String "SubmissionMalformed"        -> pure SubmissionMalformed
        String "SubmissionSignatureInvalid" -> pure SubmissionSignatureInvalid
        String "DeletingGradedSubmission"   -> pure DeletingGradedSubmission
        String other -> fail $ "invalid error constructor: " ++ toString other
        other -> typeMismatch "API error" other

instance ToJSON APIError where
    toJSON = \case
        SubmissionMalformed -> String "SubmissionMalformed"
        SubmissionSignatureInvalid -> String "SubmissionSignatureInvalid"
        DeletingGradedSubmission -> String "DeletingGradedSubmission"
        EntityAbsent err -> object
            [ "missing_entity" .= err
            ]

-- | Get HTTP error code of error.
toServantErrNoReason :: APIError -> ServantErr
toServantErrNoReason = \case
    SubmissionMalformed        -> err403
    SubmissionSignatureInvalid -> err403
    DeletingGradedSubmission   -> err403
    EntityAbsent _             -> err404

-- | Make up error which will be returned to client.
toServantErr :: APIError -> ServantErr
toServantErr err = (toServantErrNoReason err){ errBody = encode err }

unexpectedToServantErr :: SomeException -> ServantErr
unexpectedToServantErr err = err500{ errBody = show err }
