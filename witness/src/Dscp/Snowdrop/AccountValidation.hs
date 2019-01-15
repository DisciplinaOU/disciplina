{-# LANGUAGE DataKinds #-}

-- | Account-style validation. Copied from
-- 'Snowdrop.Model.State.Accounting.Account'. It'll be brought back to
-- snowdrop at some point :yao:

module Dscp.Snowdrop.AccountValidation
       ( validateSimpleMoneyTransfer

       , Authenticated (..)
       , authenticate
       , requirePart
       , assertSigned
       , assertExists
       , nothingToLocalError
       , check
       ) where

import Control.Monad.Error.Class (MonadError)
import Data.Default (def)
import Snowdrop.Core (ERoComp, HasKeyValue, PreValidator (..), StatePException, StateTx (..),
                      StateTxType (..), Validator, mkValidator, queryOne, validateIff)
import Snowdrop.Util

import Dscp.Core
import qualified Dscp.Crypto as DC (PublicKey)
import Dscp.Snowdrop.Configuration (CanVerifyPayload, Exceptions, Ids, PersonalisedProof (..),
                                    Proofs, TxIds, Values)
import Dscp.Snowdrop.Types

assertSigned
    :: ( VerifySign sigScheme a
       , HasReview e e1
       , MonadError e m
       )
    => WithSignature sigScheme a
    -> e1
    -> m a
assertSigned WithSignature {..} message = do
    () <- validateIff message $
        verifySignature wsPublicKey wsBody wsSignature

    return wsBody

infix 1 `check`
check :: (Monoid a, HasException e e1) => Bool -> e1 -> ERoComp e id value ctx a
check = flip validateIff

validateSimpleMoneyTransfer
    :: forall ctx.
       Validator Exceptions Ids Proofs Values ctx
validateSimpleMoneyTransfer = mkValidator ty
    [preValidateSimpleMoneyTransfer @ctx]
  where
    ty = StateTxType $ getId (Proxy @TxIds) AccountTxTypeId

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

preValidateSimpleMoneyTransfer :: PreValidator Exceptions Ids Proofs Values ctx
preValidateSimpleMoneyTransfer =
    PreValidator $ \_trans@StateTx {..} -> do
        pass  -- all the validation happens in expander

-- | Require that whole projects into part or throw error.
requirePart :: (HasReview e e1, MonadError e m, HasPrism s hash) => s -> e1 -> m hash
requirePart whole message = maybe (throwLocalError message) pure $ proj whole

-- | Require that id exists in a database or throw error.
assertExists ::
       ( HasExceptions e '[ e1, StatePException]
       , Ord id
       , Ord id'
       , HasKeyValue id value id' a
       )
    => id'
    -> e1
    -> ERoComp e id value ctx a
assertExists thing message =
    maybe (throwLocalError message) pure =<< queryOne thing

nothingToLocalError
    :: HasReview e e1
    => e1 -> Maybe a -> ERoComp e id value ctx a
nothingToLocalError err mThing = maybe (throwLocalError err) pure mThing
