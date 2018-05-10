
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

         -- * Constants
       , genesisHeaderHash

         -- * Helpers
       , getPrevBlockRefMaybe
       ) where

import Universum

import Control.Lens (makeLenses)
import Disciplina.Core.Types (ATGDelta (..))
import Disciplina.Crypto (Hash, unsafeHash)
import Disciplina.Crypto.MerkleTree (MerkleSignature, fromFoldable, getMerkleRoot)
import Disciplina.Educator.Txs (PrivateTxAux)


----------------------------------------------------------
-- Block elements
----------------------------------------------------------

-- | Hash of the private block.
type PrivateHeaderHash = Hash PrivateBlockHeader

-- | Header of a private block. There's no signatures here, as it's private anyway.
-- During publishing, Educator will provide a signature as a part of transaction.
data PrivateBlockHeader = PrivateBlockHeader
    { _pbhPrevBlock :: !PrivateHeaderHash
    -- ^ Previous header in the chain
    , _pbhBodyProof :: !(MerkleSignature PrivateTxAux)
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

-- | Get previous block header, if previous block exists,
-- 'Nothing' otherwise.
getPrevBlockRefMaybe :: PrivateBlockHeader -> Maybe PrivateHeaderHash
getPrevBlockRefMaybe PrivateBlockHeader {..} =
    if _pbhPrevBlock == genesisHeaderHash
    then Nothing
    else Just _pbhPrevBlock

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
