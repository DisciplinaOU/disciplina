
-- | Blocks- and blockchain-related datatypes definition.

module Disciplina.Educator.Block
       ( -- * Basic types
         PrivateHeaderHash
       , PrivateBlockHeader (..)
       , pbhPrevBlock
       , pbhBodyProof
       , pbhAtgDelta
       , PrivateBlockBody (..)
       , pbbTxs
       , PrivateBlock
       , PrivateUndo
       , PrivateBlund

         -- * Blockchain configuration
       , PrivateBlockVerifier
       , PrivateChainConfiguration
       , privateChainConfiguration
       ) where

import Universum

import Control.Lens (makeLenses)
import Data.ByteArray (ByteArrayAccess)
import Snowdrop.Model.Block.Core (BlkConfiguration (..), Block (..), BlockIntegrityVerifier (..),
                                  Blund (..))

import Disciplina.Core.Types (ATGDelta (..))
import Disciplina.Crypto (Hash, hash, unsafeHash)
import Disciplina.Educator.Txs (PrivateTxAux)
import Disciplina.Util (OldestFirst (..))

-- | A type for root of sized Merkle tree.
-- TODO: define it in the separate module containing actual
-- sized Merkle tree implementation (and this module should
-- probably be in `auth-data-structures` repo).
data SizedMerkleRoot a = SizedMerkleRoot
    { smrHash :: !(Hash (SizedMerkleTree a))
    , smrSize :: !Word32
    } deriving (Show, Eq, Ord, Generic)

-- | Stub type for a sized Merkle tree.
type SizedMerkleTree a = Void

-- | Stub function for constructing a sized Merkle tree from
-- a bunch of elements.
mkSizedMerkleTree :: [a] -> SizedMerkleTree a
mkSizedMerkleTree = error "Sized Merkle trees are not implemented yet"

getSizedMerkleRoot :: SizedMerkleTree a -> SizedMerkleRoot a
getSizedMerkleRoot = absurd

----------------------------------------------------------
-- Block elements
----------------------------------------------------------

-- | Hash of the private block.
type PrivateHeaderHash = Hash PrivateBlockHeader

-- | Header of a private block.
data PrivateBlockHeader = PrivateBlockHeader
    { _pbhPrevBlock :: !PrivateHeaderHash
    -- ^ Previous header in the chain
    , _pbhBodyProof :: !(SizedMerkleRoot PrivateTxAux)
    -- ^ Body payload proof (for now - only root of sized Merkle tree
    -- over private transactions)
    , _pbhAtgDelta  :: !ATGDelta
    -- ^ Changes in courses taught by Educator
    } deriving (Show, Eq, Generic)

makeLenses ''PrivateBlockHeader

-- | Genesis hash, serves as previous block reference for the first block.
-- TODO: move to 'Genesis' module when it is formed somehow. Also, should
-- private genesis hash actually make some sense?
genesisHeaderHash :: PrivateHeaderHash
genesisHeaderHash = unsafeHash ("pvaforever" :: ByteString)

-- | Private block body. Contains only private transactions (for now).
-- TODO: should we also store inner Merkle nodes in some sort of cache,
-- to provide quick positions?
data PrivateBlockBody = PrivateBlockBody
    { _pbbTxs :: ![PrivateTxAux]
    -- ^ We don't store tx witnesses separately, because we don't really care
    -- much about private block size, but we do care about simplicity of
    -- sharing private transactions together with their proofs.
    } deriving (Show, Eq, Generic)

makeLenses ''PrivateBlockBody

------------------------------------------------------------
-- Block definition
------------------------------------------------------------

-- | Define block using Snowdrop datatype.
type PrivateBlock = Block PrivateBlockHeader PrivateBlockBody

-- | There's arguably no use case for rolling blocks back in
-- private chain (_especially_ if headers of these blocks has
-- already been published). So we leave 'PrivateUndo' as unit
-- for now.
-- TODO: probably it still makes sense to provide a working
-- way to undo a block?
data PrivateUndo = UnsafePrivateUndo

type PrivateBlund = Blund PrivateBlockHeader PrivateBlockBody PrivateUndo

------------------------------------------------------------
-- Block configuration
------------------------------------------------------------

-- | Get previous block header, if previous block exists,
-- 'Nothing' otherwise.
getPrevBlockRefMaybe :: PrivateBlockHeader -> Maybe PrivateHeaderHash
getPrevBlockRefMaybe PrivateBlockHeader {..} =
    if _pbhPrevBlock == genesisHeaderHash
    then Nothing
    else Just _pbhPrevBlock

-- | Type for verifier of private block.
type PrivateBlockVerifier =
    BlockIntegrityVerifier PrivateBlockHeader PrivateBlockBody

verifyPrivatePayload :: PrivateBlockVerifier
verifyPrivatePayload = BIV $ \Block {..} ->
    blkHeader^.pbhBodyProof ==
    getSizedMerkleRoot (mkSizedMerkleTree $ blkPayload^.pbbTxs)

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
    BlkConfiguration PrivateBlockHeader PrivateBlockBody PrivateHeaderHash

privateChainConfiguration :: PrivateChainConfiguration
privateChainConfiguration = BlkConfiguration
    { bcBlockRef     = error "nice hashing isn't implemented yet"
      -- ^ Here should be 'hash', but binary serialization isn't ready yet.
    , bcPrevBlockRef = getPrevBlockRefMaybe
    , bcBlkVerify    = verifyPrivatePayload
    , bcIsBetterThan = privateIsBetterThan
    , bcMaxForkDepth = privateMaxForkDepth
    }
