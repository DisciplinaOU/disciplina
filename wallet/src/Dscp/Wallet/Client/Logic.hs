module Dscp.Wallet.Client.Logic
       ( WalletClient
       , WitnessEndpoints (..)
       , createWalletClient
       ) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (..), Client, ClientEnv, ClientM, Scheme (..), client, mkClientEnv,
                       runClientM)
import Servant.Generic ((:-), fromServant)

import Dscp.Util
import Dscp.Wallet.Client.Error
import Dscp.Web
import Dscp.Witness.Web

-- todo: not needed with servant-client-0.14 (lts-12)
data AsClientT (m :: * -> *)
type instance AsClientT m :- api = Client m api

type WalletClient = WitnessEndpoints (AsClientT IO)

buildClientEnv :: NetworkAddress -> IO ClientEnv
buildClientEnv netAddr = do
    let baseUrl = BaseUrl
            { baseUrlScheme = Http
            , baseUrlHost = toString $ naHost netAddr
            , baseUrlPort = fromIntegral $ naPort netAddr
            , baseUrlPath = ""
            }
    manager <- newManager defaultManagerSettings
    return $ mkClientEnv manager baseUrl

createWalletClient :: NetworkAddress -> IO WalletClient
createWalletClient netAddr = do
    cliEnv <- buildClientEnv netAddr
    let nat :: ClientM a -> IO a
        nat act = runClientM act cliEnv >>= leftToThrow servantToWitnessError

    let es :: WitnessEndpoints (AsClientT ClientM)
        es = fromServant $ client witnessAPI
    -- todo: use hoistClient or smth similar in lts-12 age
    return WitnessEndpoints
        { wGetAccountState = nat ... wGetAccountState es
        , wSubmitTx = nat ... wSubmitTx es
        }
