-- | Student API errors

module Dscp.Educator.Web.Student.Error
       ( APIError (..)
       , _BadSubmissionSignature
       , _DeletingGradedSubmission
       , _EntityAbsent
       , _EntityAlreadyPresent
       , WrongSubmissionSignature (..)
       , _FakeSubmissionSignature
       , _SubmissionSignatureInvalid
       , ObjectAlreadyExistsError (..)
       , _SubmissionAlreadyExists

       , ErrResponse (..)

       , toServantErr
       , unexpectedToServantErr
       ) where

import Control.Lens (makePrisms)
import Data.Aeson (ToJSON (..), Value (..), encode)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.Typeable (cast)
import Servant (ServantErr (..), err400, err403, err404, err409, err500)

import qualified Dscp.Core as Core
import Dscp.Crypto (Hash)
import Dscp.DB.SQLite.Queries (DomainError (..))
import Dscp.Educator.BlockValidation (SubmissionValidationFailure)

data WrongSubmissionSignature
    = FakeSubmissionSignature
      -- ^ Signature doesn't match the student who performs the request.
    | SubmissionSignatureInvalid [SubmissionValidationFailure]
      -- ^ Submission is invalid on itself.
    deriving (Eq, Show)

makePrisms ''WrongSubmissionSignature

instance Exception WrongSubmissionSignature

data ObjectAlreadyExistsError
    = SubmissionAlreadyExists { saeSubmissionId :: !(Hash Core.Submission) }
    deriving (Show, Eq)

makePrisms ''ObjectAlreadyExistsError

instance Exception ObjectAlreadyExistsError

-- | Any error backend may return.
data APIError
    = BadSubmissionSignature WrongSubmissionSignature
      -- ^ Submission signature doesn't match the student nor has valid format.
    | DeletingGradedSubmission
      -- ^ Graded Submission is deleting
    | EntityAbsent DomainError
      -- ^ Entity is missing
    | EntityAlreadyPresent ObjectAlreadyExistsError
      -- ^ Entity is duplicated
    deriving (Show, Eq, Generic, Typeable)

makePrisms ''APIError

instance Exception APIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , BadSubmissionSignature <$> fromException e
        , EntityAbsent           <$> fromException e
        , EntityAlreadyPresent   <$> fromException e
        ]

-- | Contains info about error in client-convenient form.
data ErrResponse = ErrResponse
    { erError :: !APIError
    } deriving (Show, Eq, Generic)

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''ObjectAlreadyExistsError
deriveJSON defaultOptions ''WrongSubmissionSignature
deriveToJSON defaultOptions ''ErrResponse

instance ToJSON APIError where
    toJSON = String . \case
        BadSubmissionSignature err -> case err of
            FakeSubmissionSignature{}    ->          "FakeSubmissionSignature"
            SubmissionSignatureInvalid{} ->          "SubmissionSignatureInvalid"
        DeletingGradedSubmission{} ->                "DeletingGradedSubmission"
        EntityAbsent err -> case err of
            CourseDoesNotExist{}                  -> "CourseDoesNotExist"
            StudentDoesNotExist{}                 -> "StudentDoesNotExist"
            AssignmentDoesNotExist{}              -> "AssignmentDoesNotExist"
            StudentWasNotEnrolledOnTheCourse{}    -> "StudentWasNotEnrolledOnTheCourse"
            StudentWasNotSubscribedOnAssignment{} -> "StudentWasNotSubscribedOnAssignment"
            SubmissionDoesNotExist{}              -> "SubmissionDoesNotExist"
            TransactionDoesNotExist{}             -> "TransactionDoesNotExist"
            BlockWithIndexDoesNotExist{}          -> "BlockWithIndexDoesNotExist"
        EntityAlreadyPresent err -> case err of
            SubmissionAlreadyExists{} ->             "SubmissionAlreadyExists"

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

-- | Get HTTP error code of error.
toServantErrNoReason :: APIError -> ServantErr
toServantErrNoReason = \case
    BadSubmissionSignature{}   -> err403
    DeletingGradedSubmission{} -> err403
    EntityAbsent err           -> case err of
        CourseDoesNotExist{}                  -> err404
        StudentDoesNotExist{}                 -> err404
        AssignmentDoesNotExist{}              -> err404
        StudentWasNotEnrolledOnTheCourse{}    -> err400
        StudentWasNotSubscribedOnAssignment{} -> err400
        SubmissionDoesNotExist{}              -> err404
        TransactionDoesNotExist{}             -> err404
        BlockWithIndexDoesNotExist{}          -> err500
    EntityAlreadyPresent{}     -> err409

-- | Make up error which will be returned to client.
toServantErr :: APIError -> ServantErr
toServantErr err = (toServantErrNoReason err){ errBody = encode $ ErrResponse err }

unexpectedToServantErr :: SomeException -> ServantErr
unexpectedToServantErr err = err500{ errBody = show err }
