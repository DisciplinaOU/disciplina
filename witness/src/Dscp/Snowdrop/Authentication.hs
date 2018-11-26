
module Dscp.Snowdrop.Authentication
       ( Authenticated (..)
       , authenticate
       ) where

import Data.Default (def)

import Snowdrop.Core(queryOne, ERoComp)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto as DC (PublicKey)
import Dscp.Snowdrop.Assertions
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Types

data Authenticated payload = Authenticated
    { aAccountId :: AccountId
    , aAccount   :: Account
    , aPayload   :: payload
    , aMinFees   :: Fees -- ^ The minimum fees tx sender must pay.
    } deriving (Show)

authenticate ::
       forall txid ctx payload.
       ( Eq txid
       , HasPrism Proofs (PersonalisedProof txid payload)
       , HasPrism Proofs txid
       , HasGetter DC.PublicKey Address
       , CanVerifyPayload txid payload
       )
    => Proofs
    -> ERoComp Exceptions Ids Values ctx (Authenticated payload)
authenticate proof = do
    PersonalisedProof signedHash fees
        :: PersonalisedProof txid payload
        <- requirePart proof SignatureIsMissing

    realHashfromExpander <- requirePart proof TransactionIsCorrupted

    (hash, pk, payload) <- signedHash `assertSigned` SignatureIsCorrupted

    let authorId = gett pk

    before <- fromMaybe def <$> queryOne (AccountId authorId)
    ()     <- realHashfromExpander == hash `check` TransactionIsCorrupted

    return $ Authenticated
        (AccountId authorId)
        before
        payload
        fees

{-
bypassAuthentication
    :: forall txid ctx payload.
       ( Eq txid
       , HasPrism Proofs (PersonalisedProof txid payload)
       , HasPrism Proofs txid
       , HasGetter DC.PublicKey Address
       , CanVerifyPayload txid payload
       )
    => Proofs
    -> ERoComp Exceptions Ids Values ctx (Authenticated payload)
bypassAuthentication proof = do
    PersonalisedProof signedHash fees
        :: PersonalisedProof txid payload
        <- requirePart proof SignatureIsMissing

    (hash, pk, payload) <- signedHash `assertSigned` SignatureIsCorrupted

    let authorId = gett pk

    before <- fromMaybe def <$> queryOne (AccountId authorId)

    return $ Authenticated
        (AccountId authorId)
        before
        payload
        fees
-}
