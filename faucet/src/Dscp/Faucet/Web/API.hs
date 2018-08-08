{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Faucet HTTP API definition.

module Dscp.Faucet.Web.API
       ( FaucetApiEndpoints (..)
       , FaucetAPI
       , faucetAPI
       , FaucetApiHandlers
       ) where

import Servant
import Servant.Generic

import Dscp.Faucet.Web.Error
import Dscp.Faucet.Web.Types

data FaucetApiEndpoints route = FaucetApiEndpoints
    { -- uses POST because accepts private data
      fGenKeyPair :: route
        :- "keygen"
        :> ReqBody '[DSON] GenKeysRequest
        :> Post '[DSON] GenKeysResponse

    , fTransferMoneyTo :: route
        :- "transfer"
        :> ReqBody '[DSON] TransferMoneyRequest
        :> Post '[DSON] TransferMoneyResponse
    } deriving (Generic)

type FaucetAPI = "v1" :> ToServant (FaucetApiEndpoints AsApi)

type FaucetApiHandlers m = FaucetApiEndpoints (AsServerT m)

faucetAPI :: Proxy FaucetAPI
faucetAPI = Proxy
