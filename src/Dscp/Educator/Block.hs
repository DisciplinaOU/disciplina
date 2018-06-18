
-- | Blocks- and blockchain-related datatypes definition.

module Dscp.Educator.Block
       ( -- * Basic types
         PrivateHeaderHash
       , PrivateBlockHeader (..)
       , pbhPrevBlock
       , pbhBodyProof
       , pbhAtgDelta
       , PrivateBlockBody (..)
       , pbbTxs
       , PrivateBlock (..)
       , pbHeader
       , pbBody

         -- * Constants
       , genesisHeaderHash

         -- * Helpers
       , getPrevBlockRefMaybe
       ) where

import Universum

import Control.Lens (makeLenses)
import Dscp.Core.Types (ATGDelta (..))
import Dscp.Crypto (Hash, unsafeHash)
import Dscp.Crypto.MerkleTree (MerkleSignature)
import Dscp.Educator.Txs (PrivateTx)

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
    , _pbhBodyProof :: !(MerkleSignature PrivateTx)
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
    { _pbbTxs :: ![PrivateTx]
    } deriving (Show, Eq, Generic)

makeLenses ''PrivateBlockBody

-- | Private block consists (surprisingly) of header and body.
-- We won't define a private undo yet, because we're yet to define
-- the usecase for rollbacks in private chain.
data PrivateBlock = PrivateBlock
    { _pbHeader :: !PrivateBlockHeader
    , _pbBody   :: !PrivateBlockBody
    } deriving (Show, Eq, Generic)

makeLenses ''PrivateBlock
