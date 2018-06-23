
-- | Validation of private block

module Dscp.Educator.Validate
       ( ValidationFailure (..)
       , validatePrivateBlk
       ) where

import Universum

import Data.Text.Buildable (build)

import Dscp.Core (Address (..), SignedSubmission (..), Submission (..),
                  SubmissionSig, SubmissionWitness (..))
import Dscp.Crypto (Hash, PublicKey, MerkleSignature, fromFoldable, getMerkleRoot, hash, verify)
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockHeader (..), pbBody, pbbTxs)
import Dscp.Educator.Serialise ()
import Dscp.Educator.Txs (PrivateTx (..))

import qualified Text.Show
import qualified Data.Text.Buildable ()


-- | Validation failures
data ValidationFailure
    = MerkleSignatureMismatch (MerkleSignature PrivateTx) (MerkleSignature PrivateTx)
      -- ^ Header merkle tree root does not match
      -- body merkle tree root
    | TxPublicKeyMismatch (Hash PublicKey) (Hash PublicKey)
      -- ^ Witness public key do not match transaction address
    | TxSignatureMismatch PublicKey SubmissionSig (Hash Submission)
      -- ^ Witness signature do not match transaction submission signature
    deriving (Eq)

instance Show ValidationFailure where
    show e = toString . pretty $ e

instance Buildable ValidationFailure where
    build (MerkleSignatureMismatch expected got) =
      "Merkle tree root signature mismatch. Expected " `mappend` show expected
      `mappend` " got " `mappend` show got
    build (TxPublicKeyMismatch expected got) =
      "Tx public key address mismatch. Expected " `mappend` show expected `mappend`
      " got " `mappend` show got
    build (TxSignatureMismatch key sig h) =
      "Tx signature mismatch. Submission data " `mappend` show h `mappend` " with signature "
      `mappend` show sig `mappend` " does not correspond to public key " `mappend` show key

-- | Validate private block.
-- Private block is valid iff
-- 1. Header matches body
-- 2. All transactions have valid signatures
-- 3. Transaction signature public key correspond to student public key
validatePrivateBlk :: PrivateBlock -> Either [ValidationFailure] ()
validatePrivateBlk pb =
    let txs = pb ^. pbBody . pbbTxs
        merkleRoot = getMerkleRoot (fromFoldable txs)
        headerVer = validateTxHeader (_pbHeader pb) merkleRoot
        blockValid :: [(Bool, ValidationFailure)]
        blockValid = filter ((==False) . fst) (headerVer <> concatMap validateTx txs)
    in case blockValid of
         [] -> Right ()
         x  -> Left (map snd x)
  where validateTxHeader (PrivateBlockHeader {..}) merkleRoot =
          [(_pbhBodyProof == merkleRoot, MerkleSignatureMismatch _pbhBodyProof merkleRoot)]

        validateTx (PrivateTx {..}) =
          let submission = ssSubmission _ptxSignedSubmission
              witnessKey = _swKey (ssWitness _ptxSignedSubmission)
              witnessSign = _swSig (ssWitness _ptxSignedSubmission)
              txAddrHash = addrHash (sStudentId submission)
              witnessSigValid = verify witnessKey (hash submission) witnessSign
              witnessKeyValid = hash witnessKey == txAddrHash
          in [(witnessKeyValid, TxPublicKeyMismatch txAddrHash (hash witnessKey))
             ,(witnessSigValid, TxSignatureMismatch witnessKey witnessSign (hash submission))]
