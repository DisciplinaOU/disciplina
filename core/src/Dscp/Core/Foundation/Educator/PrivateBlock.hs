
module Dscp.Core.Foundation.Educator.PrivateBlock where

import Control.Lens (makeLenses)
import qualified Data.ByteArray as BA

import Dscp.Core.Foundation.Address
import Dscp.Core.Foundation.Educator.ATGDelta
import Dscp.Core.Foundation.Educator.PrivateTx
import Dscp.Crypto
import Dscp.Util

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
    } deriving (Show, Eq, Ord, Generic)

makeLenses ''PrivateBlockHeader

instance Buildable PrivateBlockHeader where
    build PrivateBlockHeader {..} =
        "PrivateBlockHeader { prev: " +| _pbhPrevBlock |+
        "; body proof: " +| _pbhBodyProof |+
        "; atg:" +| _pbhAtgDelta |+ " }"

-- | Genesis hash, serves as previous block reference for the first block.
-- Different for each educator.
-- TODO: move to 'Genesis' module when it is formed somehow. Also, should
-- private genesis hash actually make some sense?
genesisHeaderHash :: Address -> PrivateHeaderHash
genesisHeaderHash (Address addr) =
    unsafeHash ("pvaforever" <> BA.convert addr :: ByteString)

-- | Get previous block header, if previous block exists,
-- 'Nothing' otherwise.
getPrevBlockRefMaybe :: PrivateBlockHeader -> Address -> Maybe PrivateHeaderHash
getPrevBlockRefMaybe PrivateBlockHeader {..} address =
    if _pbhPrevBlock == genesisHeaderHash address
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

