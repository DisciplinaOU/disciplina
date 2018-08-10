module Dscp.Witness.Web.Client.Logic
       ( WitnessClient
       , WitnessEndpoints (..)
       , createWitnessClient
       , hoistWitnessClient
       ) where

import Servant.Client (Client, ClientM, Scheme (..), client, runClientM)
import Servant.Generic ((:-), fromServant)

import Dscp.Resource.Class
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
    { wPing = nat ... wPing es
    , wSubmitTx = nat ... wSubmitTx es
    , wSubmitTxAsync = nat ... wSubmitTxAsync es
    , wGetBlocks = nat ... wGetBlocks es
    , wGetBlock = nat ... wGetBlock es
    , wGetAccount = nat ... wGetAccount es
    , wGetTransaction = nat ... wGetTransaction es
    }

createWitnessClient :: MonadIO m => NetworkAddress -> m WitnessClient
createWitnessClient netAddr = do
    cliEnv <- buildClientEnv Http netAddr
    let nat :: ClientM a -> IO a
        nat act = runClientM act cliEnv >>= leftToThrow servantToWitnessError

    let es :: WitnessEndpoints (AsClientT ClientM)
        es = fromServant $ client witnessAPI
    return $ hoistWitnessClient nat es

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance AllocResource NetworkAddress WitnessClient where
    allocResource params =
        buildComponentR "witness client"
            (createWitnessClient params)
            (\_ -> pass)  -- items clean on itself on GC, kaef
