-- | Faucet API handlers

module Dscp.Faucet.Web.Handlers
       ( faucetApiHandlers
       , convertFaucetApiHandler
       ) where

import Servant (Handler)

import Dscp.Core
import Dscp.Crypto
import Dscp.Faucet.Launcher
import Dscp.Faucet.Web.API
import Dscp.Faucet.Web.Error
import Dscp.Faucet.Web.Logic
import Dscp.Faucet.Web.Types
import Dscp.Rio
import Dscp.Util.Aeson
import Dscp.Web.Class

faucetApiHandlers
    :: forall m ctx. FaucetWorkMode ctx m
    => FaucetApiHandlers m
faucetApiHandlers =
    FaucetApiEndpoints
    { fGenKeyPair = \(GenKeysRequest mpp) -> do
          let pp = maybe emptyPassPhrase getAsByteString mpp
          (sk, pk) <- runSecureRandom keyGen
          let esk = encrypt pp sk
          return GenKeysResponse
              { gkrEncSecretKey = CustomEncoding esk
              , gkrSecretKey = AsByteString sk
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
    processServerErrors @FaucetAPIError (runRIO ctx handler)
