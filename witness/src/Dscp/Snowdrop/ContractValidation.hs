{-# language NoMonomorphismRestriction #-}

module Dscp.Snowdrop.ContractValidation
       (
       ) where

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

data Stage
    = Created
    | Cancelled
    | Transmitted
    | KeySent
    | Arbitration
    | Agreed

-- TODO (kirill.andreev): Find a place for these.
data ContractAccount = ContractAccount
    { _caSelf          :: Account
    , _caSeller        :: AccountId
    , _caBuyer         :: AccountId
    , _caCost          :: Coin
    , _caFees          :: Coin
    , _caPublication   :: PublicationHead
    , _caEndSlot       :: SlotId
    , _caInheritor     :: AccountId
    , _caStage         :: Stage
    }

newtype ContractID = ContractID { getContractID :: Address }

type instance SValue ContractID = ContractAccount

data ContractCreationRequest = ContractCreationRequest
    { _acContractID       :: ContractID
    , _acContractBody     :: ContractAccount
    , _acMoneyTransmitted :: Coin
    }

data ContractStartingRequest = ContractStartingRequest
    { _csrContractID :: ContractID
    , _csrFeesPaid   :: Coin
    }

data ContractChecksum = ContractChecksum
    { _ccContractID :: ContractID
    , _ccChecksum   :: MerkleSignature ByteString
    , _ccStage      :: Stage
    }

data ContractIsAccepted = ContractIsAccepted
    { _ciaContractID :: ContractID
    }

data ContractBrokenChunk = ContractBrokenChunk
    { _cbcContractID :: ContractID
    , _cbcEncrypted  :: ByteString
    , _cbcSecretKey  :: SecretKey
    , _cbcMerklePath :: MerkleProof ByteString
    , _cbcIndex      :: Int
    }

data ContractDupedChunk = ContractDupedChunk
    { _cdcContractID :: ContractID
    , _cdcMerklePath :: MerkleProof ByteString
    , _cdcIndexI     :: Int
    , _cdcIndexJ     :: Int
    }

makeLenses ''ContractAccount
makeLenses ''ContractCreationRequest
makeLenses ''ContractStartingRequest
makeLenses ''ContractChecksum
makeLenses ''ContractBrokenChunk
makeLenses ''ContractDupedChunk

data ContractException
    = PublicationDoesNotExists
    | WrongSeller
    | WrongBuyer
    | ThisCostsMore
    | WrongIntialStage
    | WrongInheritor
    | WrongPersonToStartTheTrade
    | SellerUnderpaidTheFees
    | CancellingOnCorrectSig
    | NotCancellingOnInCorrectSig
    | AddressIsAlreadyTaken
    | ContractDoesNotExist

-- validateCreated = mkValidator ty [checkCreation]
--   where
--     ty = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId

type PreValidate ctx = PreValidator Exceptions Ids Proofs Values ctx
type SDActionM   ctx = ERoComp      Exceptions Ids        Values ctx

checkCreation :: () => PreValidate ctx
checkCreation =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractCreationRequest
            { _acContractID       = cid
            , _acContractBody     = body
            , _acMoneyTransmitted = money
            } fees
            <- authenticate @_ txProof

        seller <- getPublicationAuthor (body^.caPublication)

        ()     <- assertAbsence cid AddressIsAlreadyTaken

        ()     <- check (body^.caSeller              == seller)  WrongSeller
        ()     <- check (body^.caBuyer               == buyer)   WrongBuyer
        ()     <- check (body^.caCost + body^.caFees <= money)   ThisCostsMore
        ()     <- check (body^.caStage               == Created) WrongIntialStage
        ()     <- check (body^.caInheritor           == buyer)   WrongInheritor

        ()     <- assertCorrectCostAndFees body
        ()     <- assertSlotIsInFuture (body^.caEndSlot)

        return ()

checkStartingTheTrade =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated seller _ ContractStartingRequest
            { _csrContractID = cid
            , _csrFeesPaid   = fees
            } txFees
            <- authenticate @_ txProof

        contract <- getContract cid

        () <- check (seller == contract^.caSeller) WrongPersonToStartTheTrade
        () <- check (fees   <= contract^.caFees)   SellerUnderpaidTheFees

        -- I assume here that expander sets correct stage here

        return ()

checkBuyerCalculatedChecksum =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractChecksum
            { _ccContractID = cid
            , _ccChecksum   = signature
            , _ccStage      = stage
            } txFees
            <- authenticate @_ txProof

        contract     <- getContract cid
        theSignature <- getSignatureOfPublication (contract^.caPublication)

        () <- check (signature /= theSignature && stage == Cancelled)   CancellingOnCorrectSig
        () <- check (signature == theSignature && stage == Transmitted) NotCancellingOnInCorrectSig

        return ()

checkBuyerAccepts =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractIsAccepted
            { _ciaContractID = cid
            } txFees
            <- authenticate @_ txProof

        contract <- getContract cid

        () <- check (buyer == contract^.caBuyer) WrongBuyer

        return ()

checkBuyerFoundBrokenChunk =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractBrokenChunk
            { _cbcContractID = cid
            , _cbcSecretKey  = sk
            , _cbcEncrypted  = chunk
            , _cbcMerklePath = mpath
            , _cbcIndex      = index
            } txFees
            <- authenticate @_ txProof

        contract <- getContract cid

        () <- check (buyer == contract^.caBuyer) WrongBuyer

        -- All the work here goes inside the expander, where the choice
        -- of money destination is made.

        return ()

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

getContract :: ContractID -> SDActionM ctx ContractAccount
getContract cid =
    queryOne @Ids @ContractID @Values @ContractAccount cid
        >>= maybe (throwLocalError ContractDoesNotExist) pure

-- | Require that id exists in a database or throw error.
-- assertAbsence ::
--        ( HasExceptions e '[e1, StatePException]
--        , Ord id
--        , Ord id'
--        , HasKeyValue id value id' a
--        )
--     => id'
--     -> e1
--     -> ERoComp e id value ctx a
assertAbsence thing message =
    queryOne thing
        >>= maybe
            (pure mempty)
            (const $ throwLocalError message)
