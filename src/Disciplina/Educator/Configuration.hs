module Disciplina.Educator.Configuration
       ( -- * Block types
         PrivateBlock
       , PrivateUndo (..)
       , PrivateBlund

         -- * Blockchain configuration
       , PrivateBlockVerifier
       , PrivateChainConfiguration
       , privateChainConfiguration
       ) where

import Universum

import Snowdrop.Model.Block.Core (BlkConfiguration (..), Block (..), BlockIntegrityVerifier (..),
                                  Blund (..))

import Disciplina.Crypto (hash)
import Disciplina.Educator.Block (PrivateBlockBody, PrivateBlockHeader (..), PrivateHeaderHash,
                                  getPrevBlockRefMaybe, getSizedMerkleRoot, mkSizedMerkleTree,
                                  pbbTxs, pbhBodyProof)
import Disciplina.Educator.Serialise ()
import Disciplina.Util (OldestFirst (..))

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
    { bcBlockRef     = hash
    , bcPrevBlockRef = getPrevBlockRefMaybe
    , bcBlkVerify    = verifyPrivatePayload
    , bcIsBetterThan = privateIsBetterThan
    , bcMaxForkDepth = privateMaxForkDepth
    }
