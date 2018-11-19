
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

accessContractAsBuyer :: AccountId -> ContractID -> SDActionM ctx Contract
accessContractAsBuyer buyer cid = do
    contract <- getContract cid
    ()       <- check (buyer == contract^.caBuyer) (WrongBuyer cid buyer)
    return contract

assertSlotIsInFuture :: SlotId -> SDActionM ctx ()
assertSlotIsInFuture = error "TODO"
