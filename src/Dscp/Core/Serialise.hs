module Dscp.Core.Serialise () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Types (ATG (..), ATGDelta (..), ATGEdge (..), ATGNode (..), Address (..),
                        Assignment (..), AssignmentType (..), Course (..), Grade (..),
                        SignedSubmission (..), Subject (..), Submission (..),
                        SubmissionWitness (..))
import Dscp.Util.Serialise (decodeCrcProtected, encodeCrcProtected)

-- TODO: move to well-specified serialisation instead of generic one.
deriving instance Serialise Course
deriving instance Serialise Subject

instance Serialise Address where
    encode = encodeCrcProtected $ encode . addrHash
    decode = decodeCrcProtected $ Address <$> decode

instance Serialise Grade
instance Serialise ATGNode
instance Serialise ATGEdge

deriving instance Serialise ATGDelta
deriving instance Serialise ATG

instance Serialise Assignment
instance Serialise AssignmentType
instance Serialise Submission
instance Serialise SubmissionWitness
instance Serialise SignedSubmission
