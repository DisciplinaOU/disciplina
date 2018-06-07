module Disciplina.Educator.Configuration
       ( -- * Block types
         PrivateBlock
       , PrivateUndo
       , PrivateBlund

         -- * Key types
       , CourseStudentsRef (..)
       , CourseSubjectsRef (..)
       , StudentCoursesRef (..)
       , StudentAssignmentsRef (..)

         -- * Value types
       , AssignmentState (..)

         -- * State types
       , Exceptions (..)
       , Ids (..)
       , Proofs (..)
       , Values (..)

         -- * Blockchain configuration
       , PrivateBlockVerifier
       , PrivateChainConfiguration
       , PrivateBlkStateConfiguration
       , privateChainConfiguration
       , privateBlkStateConfig
       ) where

import Universum

import Snowdrop.Model.Block.Apply (BlockApplicationException)
import Snowdrop.Model.Block.Core (BlkConfiguration (..), BlkStateConfiguration (..), Block (..),
                                  BlockIntegrityVerifier (..), Blund (..))
import Snowdrop.Model.Block.State (BlockRef, BlockStateException, TipKey, TipValue,
                                   inmemoryBlkStateConfiguration)
import Snowdrop.Model.State.Core (EStatePComputation, RestrictionInOutException, SValue,
                                  StateModificationException, StatePException, StateTx,
                                  StateTxType (..), StructuralValidationException, Validator (..),
                                  ValidatorExecException, mkValidator)

import Disciplina.Core.Types (AssignmentId, CourseId, StudentId, SubjectId, Submission)
import Disciplina.Crypto (hash)
import Disciplina.Educator.Block (PrivateBlockHeader (..), PrivateHeaderHash, getPrevBlockRefMaybe)
import Disciplina.Educator.Serialise ()
import Disciplina.Educator.Txs (PrivateTxWitness)
import Disciplina.Util (CSMappendException, ChangeSet, IdSumPrefixed (..), OldestFirst (..),
                        Prefix (..), deriveView, withInj, withInjProj)

------------------------------------------------------------
-- Exceptions
------------------------------------------------------------

data Exceptions = StatePError StatePException
    | BlockStateError (BlockStateException Ids)
    | StateModificationError (StateModificationException Ids)
    | ValidatorExecError ValidatorExecException
    | StructuralValidationError (StructuralValidationException Ids)
    | CSMappendError (CSMappendException Ids)
    | BlockApplicationError (BlockApplicationException PrivateHeaderHash)
    | ExpanderRestrictionError RestrictionInOutException
    | ConversionError Text
    deriving (Show)

instance Exception Exceptions

------------------------------------------------------------
-- Key-value storage keys
------------------------------------------------------------

-- | Newtypes for distinguishing between different keys of same type
newtype StudentCoursesRef ref = StudentCoursesRef
    { getStudentCoursesRef :: ref
    } deriving (Eq, Ord, Show)

newtype CourseStudentsRef ref = CourseStudentsRef
    { getCourseStudentsRef :: ref
    } deriving (Eq, Ord, Show)

newtype CourseSubjectsRef ref = CourseSubjectsRef
    { getCourseSubjectsRef :: ref
    } deriving (Eq, Ord, Show)

newtype StudentAssignmentsRef ref = StudentAssignmentsRef
    { getStudentAssignmentsRef :: ref
    } deriving (Eq, Ord, Show)

-- | Sum of possible keys
data Ids
    = TipKeyIds TipKey
    | BlockRefIds (BlockRef PrivateHeaderHash)
    | StudentCoursesIds (StudentCoursesRef StudentId)
    | CourseStudentsIds (CourseStudentsRef CourseId)
    | CourseSubjectsIds (CourseSubjectsRef CourseId)
    | StudentAssignmentsIds (StudentAssignmentsRef StudentId)
    deriving (Eq, Ord, Show)

instance IdSumPrefixed Ids where
    idSumPrefix (TipKeyIds _)             = Prefix 1
    idSumPrefix (BlockRefIds _)           = Prefix 2
    idSumPrefix (StudentCoursesIds _)     = Prefix 3
    idSumPrefix (CourseStudentsIds _)     = Prefix 4
    idSumPrefix (CourseSubjectsIds _)     = Prefix 5
    idSumPrefix (StudentAssignmentsIds _) = Prefix 6

type instance SValue TipKey                            = TipValue PrivateHeaderHash
type instance SValue (BlockRef PrivateHeaderHash)      = PrivateBlund
type instance SValue (StudentCoursesRef StudentId)     = [CourseId]
type instance SValue (CourseStudentsRef CourseId)      = [StudentId]
type instance SValue (CourseSubjectsRef CourseId)      = [SubjectId]
type instance SValue (StudentAssignmentsRef StudentId) = [AssignmentState]

------------------------------------------------------------
-- Key-value storage values
------------------------------------------------------------

data AssignmentState = AssignmentState
    { asCourse     :: !CourseId
    , asId         :: !AssignmentId
    , asSubmission :: !(Maybe Submission)
    } deriving (Eq, Show, Generic)

-- | Sum of possible values
data Values
    = TipValueVal (TipValue PrivateHeaderHash)
    | BlundVal PrivateBlund
    | CoursesVal [CourseId]
    | StudentsVal [StudentId]
    | SubjectsVal [SubjectId]
    | AssignmentStatesVal [AssignmentState]
    deriving (Eq, Show)

------------------------------------------------------------
-- Transaction proofs
------------------------------------------------------------

data Proofs = PrivateWitnesses PrivateTxWitness
    deriving (Eq, Show)

------------------------------------------------------------
-- Block definition
------------------------------------------------------------

-- | Define block using Snowdrop datatype.
type PrivatePayload = [StateTx Ids Proofs Values]
type PrivateBlock = Block PrivateBlockHeader PrivatePayload

-- | There's arguably no use case for rolling blocks back in
-- private chain (_especially_ if headers of these blocks has
-- already been published), but let's make this Undo a default one
-- (simply a 'ChangeSet').
type PrivateUndo = ChangeSet Ids Values

type PrivateBlund = Blund PrivateBlockHeader PrivatePayload PrivateUndo

------------------------------------------------------------
-- Block configuration
------------------------------------------------------------

-- | Type for verifier of private block.
type PrivateBlockVerifier =
    BlockIntegrityVerifier PrivateBlockHeader PrivatePayload

-- TODO: where should we check that Merkle tree over _raw_ transactions
-- matches the block header?
verifyPrivatePayload :: PrivateBlockVerifier
verifyPrivatePayload = BIV $ const True
-- verifyPrivatePayload = BIV $ \Block {..} ->
--     blkHeader^.pbhBodyProof ==
--     getSizedMerkleRoot (mkSizedMerkleTree $ blkPayload^.pbbTxs)

-- | Deciding if one chain is better than another.
-- TODO: not sure what should be there, because how there can be
-- forks in public chain? Let it be chain lenght for now.
privateIsBetterThan ::
       OldestFirst [] PrivateBlockHeader
    -> OldestFirst [] PrivateBlockHeader
    -> Bool
privateIsBetterThan = (>=) `on` length

-- | Private chains shouldn't fork at all.
privateMaxForkDepth :: Int
privateMaxForkDepth = 1

-- | Block configuration for private chain.
type PrivateChainConfiguration =
    BlkConfiguration PrivateBlockHeader PrivatePayload PrivateHeaderHash

privateChainConfiguration :: PrivateChainConfiguration
privateChainConfiguration = BlkConfiguration
    { bcBlockRef     = hash
    , bcPrevBlockRef = getPrevBlockRefMaybe
    , bcBlkVerify    = verifyPrivatePayload
    , bcIsBetterThan = privateIsBetterThan
    , bcMaxForkDepth = privateMaxForkDepth
    }

-- TODO: provide an actual validator
privateValidator :: Validator Exceptions Ids Proofs Values
privateValidator = mkValidator (StateTxType 0) mempty

type PrivateBlkStateConfiguration = BlkStateConfiguration
    PrivateBlockHeader PrivatePayload PrivateUndo PrivateHeaderHash
    (EStatePComputation Exceptions Ids Values)

privateBlkStateConfig :: PrivateBlkStateConfiguration
privateBlkStateConfig =
    inmemoryBlkStateConfiguration privateChainConfiguration privateValidator

-------------------------------------------------------------
-- TH-derived instances
-------------------------------------------------------------

deriveView withInj ''Exceptions
deriveView withInjProj ''Ids
deriveView withInjProj ''Proofs
deriveView withInjProj ''Values
