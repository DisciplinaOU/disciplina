-- | Arbitrary instances for educator types.

module Dscp.Educator.Arbitrary where

import Dscp.Core (Assignment, PrivateTx (..), Student, Submission, genStudentSignedSubmissions)
import Dscp.Resource.Keys (KeyJson (..))
import Dscp.Util.Test (Arbitrary (..), Gen)

genPrivateTx :: Gen Submission -> Gen (Student, Assignment, NonEmpty PrivateTx)
genPrivateTx genSubmission = do
    (student, assign, sigsubs) <- genStudentSignedSubmissions arbitrary genSubmission
    ptxs <- forM sigsubs $ \sigsub ->
        PrivateTx <$> pure sigsub <*> arbitrary <*> arbitrary
    return (student, assign, ptxs)

instance Arbitrary KeyJson where
    arbitrary = KeyJson <$> arbitrary
