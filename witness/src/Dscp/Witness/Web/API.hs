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
        :- "tx"
        :> ReqBody '[JSON] TxWitnessed
        :> Verb 'POST 201 '[DSON] ()

      -- Like 'wSubmitTx', but does not any checks on transaction application.
      -- Useful, since submitting transaction over network may take long.
    , wSubmitTxAsync :: route
        :- "tx"
        :> "async"
        :> ReqBody '[JSON] TxWitnessed
        :> Verb 'POST 202 '[DSON] ()

    , wGetBlocks :: route
        :- "blocks"
        :> QueryParam "count" Int
        :> QueryParam "from" HeaderHash
        :> Verb 'GET 200 '[DSON] [BlockInfo]

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
        :> Verb 'GET 200 '[DSON] [TxInfo]

    , wGetTransaction :: route
        :- "transactions" :> Capture "transactionHash" GTxId
        :> Verb 'GET 200 '[DSON] TxInfo
    } deriving (Generic)

type WitnessAPI =
    "api" :> "witness" :> "v1" :> ToServant (WitnessEndpoints AsApi)

witnessAPI :: Proxy WitnessAPI
witnessAPI = Proxy
