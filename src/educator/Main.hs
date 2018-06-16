
-- | Starting point for running an Educator node

module Main where

import Universum

import System.Wlog (logInfo, logWarning)

import Disciplina.DB (DBParams (..))
import Disciplina.Educator (EducatorParams (..), launchEducatorRealMode)
import Disciplina.Witness (WitnessParams (..))

import EducatorParams (EducatorOptParams (..), getEducatorParams)
main :: IO ()
main = do
    EducatorOptParams {..} <- getEducatorParams
    let educatorParams = EducatorParams
            { epWitnessParams = WitnessParams
                { wpLoggingParams = epLogParams
                , wpDBParams = DBParams{ dbpPath = epDbPath }
                }
            }
    launchEducatorRealMode educatorParams $ do
        logInfo "This is the stub for Educator node executable"
        logWarning "Please don't forget to implement everything else!"
