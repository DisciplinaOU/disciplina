
module Dscp.Snowdrop.Contracts.Errors where

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
    | SelfContractsUnallowed
