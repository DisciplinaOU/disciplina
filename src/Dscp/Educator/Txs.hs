
-- | Definitions of private transactions

module Dscp.Educator.Txs
       ( PrivateTx (..)
       , PrivateTxId
       , PrivateTxWitness (..)
       , PrivateTxAux (..)

       -- * Lenses
       , ptaTx
       , ptaWitness
       , ptGrade
       , ptSignedSubmission
       , ptTime
       , ptwKey
       , ptwSig
       ) where

import Universum

import Control.Lens (makeLenses)
import Data.Time.Clock (UTCTime)

import Dscp.Core.Types (Grade, SignedSubmission (..))
import Dscp.Crypto (Hash, PublicKey, Signature)

-- | Private transaction.
data PrivateTx = PrivateTx
    { _ptSignedSubmission :: !SignedSubmission
    -- ^ Every transaction contains one signed student submission
    , _ptGrade            :: !Grade
    -- ^ Grade for this submission
    , _ptTime             :: !UTCTime
    -- ^ Timestamp for this transaction
    } deriving (Show, Eq, Generic)

type PrivateTxId = Hash PrivateTx

-- | Which data to sign in transaction.
-- 'PrivateTxId' is basically a hash of all transaction contents,
-- so it's sufficient to sign only that.
type PrivateTxSigData = PrivateTxId

-- | Type alias for private tx signature.
type PrivateTxSig = Signature PrivateTxSigData

-- | Witness contains data required to verify transaction.
-- Included 'PublicKey' belongs either to Student or Educator.
-- TODO: maybe we can say that Educator's key is already known
-- to everybody, and not include it into Educator's witness?
data PrivateTxWitness = PkWitness
    { _ptwKey :: !PublicKey
    , _ptwSig :: !PrivateTxSig
    } deriving (Show, Eq, Generic)

-- | Datatype for verifiable transaction (transaction with a witness)
data PrivateTxAux = PrivateTxAux
    { _ptaTx      :: !PrivateTx
    , _ptaWitness :: !PrivateTxWitness
    } deriving (Show, Eq, Generic)

makeLenses ''PrivateTx
makeLenses ''PrivateTxWitness
makeLenses ''PrivateTxAux
