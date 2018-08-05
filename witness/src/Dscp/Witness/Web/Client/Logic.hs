module Dscp.Witness.Web.Client.Logic
       ( WitnessClient
       , WitnessEndpoints (..)
       , createWitnessClient
       ) where

import Servant.Client (Client, ClientM, Scheme (..), client, runClientM)
import Servant.Generic ((:-), fromServant)

import Dscp.Util
import Dscp.Web
import Dscp.Witness.Web.API
import Dscp.Witness.Web.Client.Error

-- todo: not needed with servant-client-0.14 (lts-12)
data AsClientT (m :: * -> *)
type instance AsClientT m :- api = Client m api

type WitnessClient = WitnessEndpoints (AsClientT IO)

-- todo: use hoistClient or smth similar in lts-12 age
hoistWitnessClient
    :: forall n m.
       (forall a. m a -> n a)
    -> WitnessEndpoints (AsClientT m)
    -> WitnessEndpoints (AsClientT n)
hoistWitnessClient nat es =
    WitnessEndpoints
    { wGetAccountState = nat ... wGetAccountState es
    , wSubmitTx = nat ... wSubmitTx es
    , wSubmitTxAsync = nat ... wSubmitTxAsync es
    }

createWitnessClient :: NetworkAddress -> IO WitnessClient
createWitnessClient netAddr = do
    cliEnv <- buildClientEnv Http netAddr
    let nat :: ClientM a -> IO a
        nat act = runClientM act cliEnv >>= leftToThrow servantToWitnessError

    let es :: WitnessEndpoints (AsClientT ClientM)
        es = fromServant $ client witnessAPI
    return $ hoistWitnessClient nat es
