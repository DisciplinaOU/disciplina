
module Dscp.Witness.Util (dieGracefully) where

import Loot.Log (MonadLogging, logError)

dieGracefully :: (MonadLogging m, MonadCatch m) => m () -> m ()
dieGracefully action =
    action `catchAny` \e -> do
        logError $ fromString $ "Exception in transactionRelayInput: " <> show e
