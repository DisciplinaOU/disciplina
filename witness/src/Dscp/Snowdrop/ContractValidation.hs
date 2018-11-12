module Dscp.Snowdrop.ContractValidation
       (
       ) where

{-
import qualified Data.Map as Map
import Fmt ((+||), (||+))
import Snowdrop.Core (PreValidator (..), StateTx (..), StateTxType (..), Validator, ValueOp (..),
                      changeSet, mkValidator, queryOne)
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
    | Transmitted
    | KeySent
    | Arbitration
    | Agreed

-- TODO (kirill.andreev): Find a place for these.
data ContractAccount = ContractAccount
    { _caSelf          :: Account
    , _caSeller        :: Address
    , _caBuyer         :: Address
    , _caCost          :: Coins
    , _caPublication   :: PublicationHead
    , _caEndSlot       :: Slot
    , _caInheritor     :: Address
    , _caStage         :: Stage
    }

newtype ContractID = ContractID { getContractID :: Address }

data ContractCreationRequest = ContractCreationRequest
    { _acContractID   :: Address
    , _acContractBody :: ContractAccount
    }

makeLenses ''ContractAccount

data ContractException
    = PublicationDoesNotExists
    | WrongSeller

validateCreated
    ::  ( HasPrism Proofs (PersonalisedProof ContractCreatedTxId ContractCreationRequest)
        , ...
        )
    =>  Validator Exceptions Ids Proofs Values ctx
validateCreated = mkValidator ty [checkCreation]
  where
    ty = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId

checkCreation =
    PreValidator $ \StateTx { txProof, txBody } -> do
        Authenticated buyer before () ContractCreationRequest
            { _acContractID   = cid
            , _acContractBody = contract
            }
            <- authenticate @TxId txProof

        ()  <- assertAbsence cid

        PublicationInfo { _piAuthor = seller }
            <- requirePart (body^.caPublication) PublicationDoesNotExists

        check (seller == contract^.caSeller) WrongSeller

        check (cost )

-- | Require that id exists in a database or throw error.
assertAbsence ::
       ( HasExceptions e '[e1, StatePException]
       , Ord id
       , Ord id'
       , HasKeyValue id value id' a
       )
    => id'
    -> e1
    -> ERoComp e id value ctx a
assertAbsence thing message =
    queryOne thing
        >>= maybe
            (pure mempty)
            (const $ throwLocalError message)
-}
