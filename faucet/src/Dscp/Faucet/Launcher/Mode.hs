{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Educator's WorkMode and its implementations.

module Dscp.Faucet.Launcher.Mode
    (
      -- * Constraints
      FaucetWorkMode

      -- * Implementations
    , FaucetContext (..)
    , FaucetRealMode
    ) where

import Control.Lens (makeLenses)
import qualified Dscp.Launcher.Mode as Basic
import Loot.Base.HasLens (HasCtx, HasLens (..))
import Loot.Log (Logging)

import Dscp.Faucet.Config
import Dscp.Faucet.Launcher.Marker
import Dscp.Faucet.Launcher.Resource
import Dscp.Faucet.Variables
import Dscp.Resource.Keys
import Dscp.Rio (RIO)
import Dscp.Witness.Web.Client

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Faucet node.
type FaucetWorkMode ctx m =
    ( Basic.BasicWorkMode m
    , HasFaucetConfig
    , HasCtx ctx m
        [ GiftedAddresses
        , TxSendLock
        , WitnessClient
        , KeyResources FaucetApp
        ]
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

data FaucetContext = FaucetContext
    { _fcResources :: !FaucetResources
      -- ^ Resources used by faucet.
    , _fcVariables :: !FaucetVariables
      -- ^ Variables used by faucet.
    }

makeLenses ''FaucetContext

type FaucetRealMode = RIO FaucetContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

#define GenHasLens(SUBRES, IMPL) \
    instance HasLens (SUBRES) FaucetContext (SUBRES) where \
        lensOf = IMPL

GenHasLens(Logging IO            , fcResources . frLogging)
GenHasLens(KeyResources FaucetApp, fcResources . frKeys)
GenHasLens(WitnessClient         , fcResources . frWitnessClient)
GenHasLens(GiftedAddresses       , fcVariables . fvGiftedAddresses)
GenHasLens(TxSendLock            , fcVariables . fvTxSendLock)

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: FaucetRealMode ()
_sanity = withFaucetConfig (error "") _sanityCallee
  where
    _sanityCallee :: FaucetWorkMode ctx m => m ()
    _sanityCallee = pass
