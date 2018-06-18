
-- | Starting point for running an Educator node

module Main where

import Universum

import Loot.Log (logInfo, logWarning, modifyLogName)

import Dscp.DB (DBParams (..))
import Dscp.Educator (EducatorParams (..), launchEducatorRealMode)
import Dscp.Witness (WitnessParams (..))

import qualified EducatorParams as Params

main :: IO ()
main = do
    Params.EducatorParams {..} <- Params.getEducatorParams
    let educatorParams = EducatorParams
            { epWitnessParams = WitnessParams
                { wpLoggingParams = epLogParams
                , wpDBParams = DBParams{ dbpPath = epDbPath }
                }
            }
    launchEducatorRealMode educatorParams $
      modifyLogName (<> "node") $ do
        logInfo "This is the stub for Educator node executable"
        logWarning "Please don't forget to implement everything else!"
