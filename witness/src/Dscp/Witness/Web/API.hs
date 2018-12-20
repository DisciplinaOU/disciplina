{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Dscp.Witness.Web.API
     ( WitnessEndpoints (..)
     , WitnessAPI
     , witnessAPI
     ) where

import Servant
import Servant.Generic

import Dscp.Core
import Dscp.Witness.Web.Error
import Dscp.Witness.Web.Types

-- | API/implementation of witness endpoints.
data WitnessEndpoints route = WitnessEndpoints
    { -- shortened prefix for purpose, one will most probably use type aliases
      -- refering this type

      wPing :: route
        :- "ping"
        :> Verb 'GET 200 '[DSON] ()

    , wSubmitTx :: route
        :- "transactions"
        :> ReqBody '[JSON] TxWitnessed
        :> Verb 'POST 201 '[DSON] TxId

    , wSubmitPublication :: route
        :- "publications"
        :> ReqBody '[JSON] PublicationTxWitnessed
        :> Verb 'POST 201 '[DSON] PublicationTxId

      -- Like 'wSubmitTx', but does not any checks on transaction application.
      -- Useful, since submitting transaction over network may take long.
    , wSubmitTxAsync :: route
        :- "transactions"
        :> "async"
        :> ReqBody '[JSON] TxWitnessed
        :> Verb 'POST 202 '[DSON] TxId

    , wGetBlocks :: route
        :- "blocks"
        :> QueryParam "skip" Word64
        :> QueryParam "count" Int
        :> Verb 'GET 200 '[DSON] BlockList

    , wGetBlock :: route
        :- "blocks" :> Capture "headerHash" HeaderHash
        :> Verb 'GET 200 '[DSON] BlockInfo

    , wGetAccount :: route
        :- "accounts" :> Capture "address" Address
        :> QueryFlag "includeTxs"
        :> Verb 'GET 200 '[DSON] AccountInfo

    , wGetTransactions :: route
        :- "transactions"
        :> QueryParam "count" Int
        :> QueryParam "from" TxId
        :> QueryParam "account" Address
        :> Verb 'GET 200 '[DSON] TxList

    , wGetTransaction :: route
        :- "transactions" :> Capture "transactionHash" GTxId
        :> Verb 'GET 200 '[DSON] GTxInfo

    , wGetPublications :: route
        :- "publications"
        :> QueryParam "count" Int
        :> QueryParam "from" PublicationTxId
        :> QueryParam "educator" Address
        :> Verb 'GET 200 '[DSON] PublicationList

    , wGetHashType :: route
        :- "hash" :> Capture "hash" Text
        :> Verb 'GET 200 '[DSON] HashIs

    , wCheckFairCV :: route
        :- "checkcv"
        :> ReqBody '[JSON] FairCV
        :> Verb 'PUT 200 '[DSON] FairCVCheckResult
    } deriving (Generic)

type WitnessAPI =
    "api" :> "witness" :> "v1" :> ToServant (WitnessEndpoints AsApi)

witnessAPI :: Proxy WitnessAPI
witnessAPI = Proxy
