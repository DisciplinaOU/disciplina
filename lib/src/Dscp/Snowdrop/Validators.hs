-- | Validators and block configuration.

module Dscp.Snowdrop.Validators
    ( blkStateConfig
    ) where


import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.Execution as SD
import qualified Snowdrop.Model.State.Core as SD

import Dscp.Core
import Dscp.Crypto (PublicKey, hash)
import Dscp.Snowdrop.Configuration (Exceptions, Ids, Proofs, SHeader, SPayload, SUndo, Values)

type IOCtx chgAccum = SD.IOCtx chgAccum Ids Values

----------------------------------------------------------------------------
-- Validator
----------------------------------------------------------------------------

validator :: SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
validator = error "validator"

----------------------------------------------------------------------------
-- Block configuration
----------------------------------------------------------------------------

blkStateConfig ::
       PublicKey
    -> SD.BlkStateConfiguration SHeader SPayload SUndo HeaderHash
                                (SD.ERwComp Exceptions Ids Values (IOCtx chgAccum))
blkStateConfig pk =
    SD.inmemoryBlkStateConfiguration (simpleBlkConfiguration pk) validator

simpleBlkConfiguration
    :: PublicKey
    -> SD.BlkConfiguration SHeader [SD.StateTx Ids Proofs Values] HeaderHash
simpleBlkConfiguration pk = SD.BlkConfiguration
    { bcBlockRef = hash
    , bcPrevBlockRef = getPrevHash . hPrevHash
    , bcBlkVerify = mconcat verifiers
    , bcIsBetterThan = \_ _ -> True
    , bcMaxForkDepth = 0
    }
  where
    getPrevHash h | h == genesisHash = Nothing
                  | otherwise = Just h

    verifiers :: [SD.BlockIntegrityVerifier SHeader [SD.StateTx id proof value]]
    verifiers =
      [ -- I should get a hash of block body, but i only have SPayload!
        -- verify pk (BlockToSign hDifficulty hPrevHash (hSignature sheader)
        SD.BIV $ \(SD.Block _sheader _sbody) -> True
      , SD.BIV $ \(SD.Block sheader _) -> pk == (hIssuer sheader)
      ]
