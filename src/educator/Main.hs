
-- | Starting point for running an Educator node

module Main where

import Universum

import System.Wlog (logInfo, logWarning)

import Disciplina.Educator (EducatorParams (..), launchEducatorRealMode)
import Disciplina.Launcher (BasicNodeParams (..))

import qualified Params as Params

main :: IO ()
main = do
    Params.EducatorParams {..} <- Params.getEducatorParams
    let educatorParams = EducatorParams
            { epBasicParams = BasicNodeParams
                { bnpLoggingParams = epLogParams
                }
            }
    launchEducatorRealMode educatorParams $ do
        logInfo "This is the stub for Educator node executable"
        logWarning "Please don't forget to implement everything else!"
