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
import Dscp.Witness.Web.Types

-- | API/implementation of witness endpoints.
data WitnessEndpoints route = WitnessEndpoints
    { -- shortened prefix for purpose, one will most probably use type aliases
      -- refering this type

      wPing :: route
        :- "ping"
        :> Verb 'GET 200 '[JSON] ()

    , wGetAccountState :: route
        :- "account" :> Capture "accountId" Address
        :> "state"
        :> Verb 'GET 200 '[JSON] AccountState

    , wSubmitTx :: route
        :- "tx"
        :> ReqBody '[JSON] TxWitnessed
        :> Verb 'POST 201 '[JSON] ()

      -- Like 'wSubmitTx', but does not any checks on transaction application.
      -- Useful, since submitting transaction over network may take long.
    , wSubmitTxAsync :: route
        :- "tx"
        :> "async"
        :> ReqBody '[JSON] TxWitnessed
        :> Verb 'POST 202 '[JSON] ()
    } deriving (Generic)

type WitnessAPI =
    "api" :> "witness" :> "v1" :> ToServant (WitnessEndpoints AsApi)

witnessAPI :: Proxy WitnessAPI
witnessAPI = Proxy
