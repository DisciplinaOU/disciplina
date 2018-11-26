
module Dscp.Snowdrop.Contracts.Entities where

import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Util
import Dscp.Snowdrop.Contracts.PublicationSignature
import Dscp.Snowdrop.Storage.Types

retrieve :: forall k v e ctx
    .  ( HasReview Exceptions e
       , Ord k
       , HasKeyValue Ids Values k v
       )
    => k
    -> e
    -> SDActionM ctx v
retrieve = assertExists @Ids @k @Values @v

getContract :: ContractID -> SDActionM ctx Contract
getContract cid = retrieve cid (ContractDoesNotExist cid)

getPublicationAuthor :: PublicationHead -> SDActionM ctx AccountId
getPublicationAuthor (PublicationHead headerId) = do
    ptxId
        <- retrieve headerId (PublicationHeaderDoesNotExist headerId)

    PublicationItself witnessed
        <- retrieve ptxId (PublicationDoesNotExist ptxId)

    let address = mkAddr $ pwPk $ ptwWitness witnessed

    return $ AccountId address

getSignatureOfPublication :: PublicationHead -> SDActionM ctx PublicationSignature
getSignatureOfPublication = error "TODO"

assertCorrectCostAndFees :: Contract -> SDActionM ctx ()
assertCorrectCostAndFees = error "TODO"

accessContractAsBuyerAtStage :: AccountId -> ContractID -> Stage -> SDActionM ctx Contract
accessContractAsBuyerAtStage buyer cid stage = do
    contract <- getContract cid
    ()       <- check (buyer == contract^.caBuyer) (WrongBuyer cid buyer)
    ()       <- check (stage == contract^.caStage) (WrongStage cid stage)
    return contract

accessContractAsSellerAtStage :: AccountId -> ContractID -> Stage -> SDActionM ctx Contract
accessContractAsSellerAtStage seller cid stage = do
    contract <- getContract cid
    ()       <- check (seller == contract^.caSeller) (WrongSeller cid seller)
    ()       <- check (stage  == contract^.caStage)  (WrongStage  cid stage)
    return contract

assertSlotIsInFuture :: SlotId -> SDActionM ctx ()
assertSlotIsInFuture = error "TODO"

pay :: AccountId -> Integer -> AccountId -> SDActionM ctx Delta
pay who howMuch whom = merge
    [ changeBalance who  (- howMuch)
    , changeBalance whom (  howMuch)
    ]

changeBalance :: AccountId -> Integer -> SDActionM ctx Delta
changeBalance aid change = do
    old <- retrieve aid (AccountDoesNotExist aid)
    let new = old { aBalance = aBalance old + change }

    () <- check (aBalance new >= 0) $
        BalanceCannotBecomeNegative change (aBalance old)

    return $ delta [ aid ==> Upd new ]

createContract :: ContractID -> Contract -> SDActionM ctx Delta
createContract cid body = return $ delta
    [  cid           ==> New body
    , (body^.caSelf) ==> New (def :: Account)
    ]

createAccount :: AccountId -> Account -> SDActionM ctx Delta
createAccount aid body = return $ delta
    [ aid ==> New body
    ]

setStage :: ContractID -> Stage -> SDActionM ctx Delta
setStage cid stage = do
    contract <- getContract cid
    return $ delta
        [ cid ==> Upd (contract & caStage .~ stage)
        ]
