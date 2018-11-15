module Dscp.Snowdrop.Contracts.Stages.DupedChunk where

import Control.Lens (makeLenses)
import qualified Data.Map as Map
import Fmt ((+||), (||+))

import Snowdrop.Core (PreValidator (..), StateTx (..), StateTxType (..), Validator, ValueOp (..),
                      changeSet, mkValidator, queryOne, SValue (..), ERoComp)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto
import qualified Dscp.Crypto as DC (PublicKey)
import Dscp.Snowdrop.AccountValidation
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types

data ContractDupedChunk = ContractDupedChunk
    { _cdcContractID :: ContractID
    , _cdcMerklePath :: MerkleProof ByteString
    , _cdcIndexI     :: Int
    , _cdcIndexJ     :: Int
    }

makeLenses ''ContractBrokenChunk
makeLenses ''ContractDupedChunk

checkBuyerFoundDupedChunk =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractDupedChunk
            { _cdcContractID = cid
            , _cdcMerklePath = ipath
            , _cdcIndexI     = i
            , _cdcIndexJ     = j
            } txFees
            <- authenticate @_ txProof

        contract <- getContract cid

        () <- check (buyer == contract^.caBuyer) WrongBuyer

        -- All the work here goes inside the expander, too.

        return ()
