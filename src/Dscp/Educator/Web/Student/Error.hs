
-- | API errors

module Dscp.Educator.Web.Student.Error
       ( APIError (..)
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Data (Data (..), toConstr)

data APIError
    = SubmissionInvalid
    | SubmissionSignatureInvalid
    | DeletingGradedSubmission
    deriving (Show, Eq, Generic, Data, Typeable)

instance Exception APIError

-- | TODO: generic 'FromJSON' instance which matches strings with
-- constructors automatically.
instance FromJSON APIError where
    parseJSON = withText "APIError" $ \case
        "SubmissionInvalid"          -> pure SubmissionInvalid
        "SubmissionSignatureInvalid" -> pure SubmissionSignatureInvalid
        "DeletingGradedSubmission"   -> pure DeletingGradedSubmission
        other -> fail $ "invalid error constructor: " ++ toString other

instance ToJSON APIError where
    toJSON = String . show . toConstr
