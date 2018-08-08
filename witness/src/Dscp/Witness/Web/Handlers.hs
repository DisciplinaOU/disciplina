{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Student API handlers

module Dscp.Witness.Web.Handlers
       ( witnessServantHandlers
       ) where

import Servant
import Servant.Generic (AsServerT, toServant)

import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Web.API
import Dscp.Witness.Web.Logic

witnessServantHandlers
    :: forall m ctx. WitnessWorkMode ctx m
    => ServerT WitnessAPI m
witnessServantHandlers =
    toServant @(WitnessEndpoints (AsServerT m)) WitnessEndpoints
    { wPing = pass
    , wGetAccountState = getAccountState
    , wSubmitTx = submitUserTx
    , wSubmitTxAsync = submitUserTxAsync
    }
