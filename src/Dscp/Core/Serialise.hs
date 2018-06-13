module Dscp.Core.Serialise () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Types (ATG (..), ATGDelta (..), ATGEdge (..), ATGNode (..), Address (..),
                        Assignment (..), AssignmentType (..), CourseId (..), Grade (..),
                        SignedSubmission (..), SubjectId (..), Submission (..), SubmissionType (..),
                        SubmissionWitness (..), SubmissionWitnessAux (..))

-- TODO: move to well-specified serialisation instead of generic one.
deriving instance Serialise CourseId
deriving instance Serialise SubjectId

instance Serialise Address
instance Serialise Grade
instance Serialise ATGNode
instance Serialise ATGEdge

deriving instance Serialise ATGDelta
deriving instance Serialise ATG

instance Serialise Assignment
instance Serialise AssignmentType
instance Serialise Submission
instance Serialise SubmissionType
instance Serialise SubmissionWitness
instance Serialise SubmissionWitnessAux
instance Serialise SignedSubmission
