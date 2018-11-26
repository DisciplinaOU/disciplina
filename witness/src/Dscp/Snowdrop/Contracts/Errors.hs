
module Dscp.Snowdrop.Contracts.Errors where

import Fmt (build, (+|), (|+), (+||), (||+))

import Dscp.Core
import Dscp.Snowdrop.Types
import Dscp.Snowdrop.Contracts.Contract

data ContractException
    = PublicationDoesNotExist       PublicationTxId
    | PublicationHeaderDoesNotExist PrivateHeaderHash
    | WrongSeller                   ContractID AccountId
    | WrongBuyer                    ContractID AccountId
    | ThisCostsMore                 ContractID Coin Coin
    | WrongStage                    ContractID Stage
    | WrongInheritor                ContractID AccountId
    | SellerUnderpaidTheFees        ContractID Coin
    | NotCancellingOnIncorrectSig   ContractID
    | AddressIsAlreadyTaken         ContractID
    | ContractDoesNotExist          ContractID
    | SelfContractsUnallowed        ContractID AccountId
    | CannotSumCostAndFees          ContractID Text
    | NoLinkedAccount               ContractID AccountId
    deriving (Generic)

instance Buildable ContractException where
    build = \case
        PublicationDoesNotExist ptid ->
            "Publication" +| ptid |+ "does not exists"

        PublicationHeaderDoesNotExist phh ->
            "Publication header hash" +| phh |+ "does not exists"

        WrongSeller cid seller ->
            onContract cid $ "Wrong seller" +|| seller ||+ ""

        WrongBuyer cid buyer ->
            onContract cid $ "" +|| buyer ||+ " is not the buyer"

        ThisCostsMore cid costs paid ->
            onContract cid $ "This costs " +|| costs ||+ ", attempted to pay " +|| paid ||+ ""

        WrongStage cid stage ->
            onContract cid $ "This contract is not in stage `" +|| stage ||+ ""

        WrongInheritor cid inheritor ->
            onContract cid $ "" +|| inheritor ||+ " cannot inherit funds"

        SellerUnderpaidTheFees cid paid ->
            onContract cid $ "" +|| paid ||+ " is not enough fees for seller to start contract"

        NotCancellingOnIncorrectSig cid ->
            onContract cid $ "Data was corrupted, but buyer did not cancel the trade"

        AddressIsAlreadyTaken cid ->
            onContract cid $ "This ContractID already exists"

        ContractDoesNotExist cid ->
            onContract cid $ "This ContractID does not exists"

        SelfContractsUnallowed cid buyer ->
            onContract cid $ "" +|| buyer ||+ " attempted to make contract with themselves"

        CannotSumCostAndFees cid msg ->
            onContract cid $ "Error while adding cost and fees" +| msg |+ ""

        NoLinkedAccount cid aid ->
            onContract cid $ "Account " +|| aid ||+ ", liked to contract does not exist"
      where
        onContract cid msg = "Contract " +|| cid ||+ ": " <> msg
