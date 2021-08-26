module Dscp.Faucet.Variables
    ( GiftedAddresses
    , TxSendLock

    , FaucetVariables
    , fvGiftedAddresses
    , fvTxSendLock
    , mkFaucetVariables
    ) where

import Control.Lens (makeLenses)

import Dscp.Core

-- | Keeps set of addresses which has already got money from faucet.
type GiftedAddresses = TVar (Set Address)

-- | Provides sequential transaction submission.
type TxSendLock = MVar ()

-- | All faucet variables.
data FaucetVariables = FaucetVariables
    { _fvGiftedAddresses :: GiftedAddresses
    , _fvTxSendLock      :: TxSendLock
    }

makeLenses ''FaucetVariables

-- | Construct initial faucet variables.
mkFaucetVariables :: MonadIO m => m FaucetVariables
mkFaucetVariables = do
    _fvGiftedAddresses <- newTVarIO mempty
    _fvTxSendLock <- newMVar ()
    return FaucetVariables{..}
