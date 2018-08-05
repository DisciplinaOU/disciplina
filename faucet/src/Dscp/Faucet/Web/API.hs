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

import Dscp.Faucet.Web.Types

data FaucetApiEndpoints route = FaucetApiEndpoints
    { -- uses POST because accepts private data
      fGenKeyPair :: route
        :- "keygen"
        :> ReqBody '[JSON] GenKeysRequest
        :> Post '[JSON] GenKeysResponse

    , fTransferMoneyTo :: route
        :- "transfer"
        :> ReqBody '[JSON] TransferMoneyRequest
        :> Post '[JSON] ()
    } deriving (Generic)

type FaucetAPI = "v1" :> ToServant (FaucetApiEndpoints AsApi)

type FaucetApiHandlers m = FaucetApiEndpoints (AsServerT m)

faucetAPI :: Proxy FaucetAPI
faucetAPI = Proxy
