module Dscp.Witness.Web.Client.Logic
       ( WitnessClient
       , WitnessEndpoints (..)
       , createWitnessClient
       , hoistWitnessClient
       ) where

import Servant.Client (ClientM, client, runClientM)
import Servant.Generic (fromServant)

import Dscp.Resource.Class
import Dscp.Util
import Dscp.Web
import Dscp.Witness.Web.API
import Dscp.Witness.Web.Client.Error

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
    , wSubmitPublication = nat ... wSubmitPublication es
    , wSubmitTxAsync = nat ... wSubmitTxAsync es
    , wGetBlocks = nat ... wGetBlocks es
    , wGetBlock = nat ... wGetBlock es
    , wGetAccount = nat ... wGetAccount es
    , wGetTransactions = nat ... wGetTransactions es
    , wGetTransaction = nat ... wGetTransaction es
    , wGetPublications = nat ... wGetPublications es
    , wGetHashType = nat ... wGetHashType es
    , wCheckFairCV = nat ... wCheckFairCV es
    }

createWitnessClient :: MonadIO m => BaseUrl -> m WitnessClient
createWitnessClient netAddr = do
    cliEnv <- buildClientEnv netAddr
    let nat :: ClientM a -> IO a
        nat act = runClientM act cliEnv >>= leftToThrow servantToWitnessError

    let es :: WitnessEndpoints (AsClientT ClientM)
        es = fromServant $ client witnessAPI
    return $ hoistWitnessClient nat es

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance AllocResource WitnessClient where
    type Deps WitnessClient = BaseUrl
    allocResource params =
        buildComponentR "witness client"
            (createWitnessClient params)
            (\_ -> pass)  -- things clean up by themselves on GC, kaef
