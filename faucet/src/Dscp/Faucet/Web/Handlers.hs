-- | Faucet API handlers

module Dscp.Faucet.Web.Handlers
       ( faucetApiHandlers
       , convertFaucetApiHandler
       ) where

import Servant (Handler, throwError)

import Dscp.Core
import Dscp.Crypto
import Dscp.Faucet.Launcher
import Dscp.Faucet.Web.API
import Dscp.Faucet.Web.Error
import Dscp.Faucet.Web.Logic
import Dscp.Faucet.Web.Types
import Dscp.Launcher.Rio
import Dscp.Util.Aeson

faucetApiHandlers
    :: forall m ctx. FaucetWorkMode ctx m
    => FaucetApiHandlers m
faucetApiHandlers =
    FaucetApiEndpoints
    { fGenKeyPair = \(GenKeysRequest mpp) -> do
          let pp = maybe emptyPassPhrase getAsByteString mpp
          (esk, pk) <- runSecureRandom (encKeyGen pp)
          return GenKeysResponse
              { gkrEncSecretKey = CustomEncoding esk
              , gkrPublicKey = pk
              , gkrAddress = mkAddr pk
              }

    , fTransferMoneyTo = \(TransferMoneyRequest dest) ->
          faucetTransferMoneyTo dest
    }

convertFaucetApiHandler
    :: FaucetContext
    -> FaucetRealMode a
    -> Handler a
convertFaucetApiHandler ctx handler =
    liftIO (runRIO ctx handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)
