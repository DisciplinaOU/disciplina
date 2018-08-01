-- | Genesis block. Simplest way possible unless DSCP-156 is done.

module Dscp.Core.Foundation.Genesis
    ( genesisBlock
    , genesisHash
    ) where

import Dscp.Core.Serialise ()
import Dscp.Core.Foundation.Transactions
import Dscp.Crypto (hash, keyGen, sign, unsafeHash, withIntSeed)

genesisBlock :: Block
genesisBlock = Block header payload
  where
    (sk,pk)  = withIntSeed 12345 keyGen
    payload  = BlockBody []
    prevHash = unsafeHash ("gromak `on` rechka" :: Text)
    toSign   = BlockToSign 0 prevHash payload
    header   = Header
        { hSignature  = sign sk toSign
        , hIssuer     = pk
        , hDifficulty = 0
        , hPrevHash   = prevHash
        }

genesisHash :: HeaderHash
genesisHash = hash (rbHeader genesisBlock)
