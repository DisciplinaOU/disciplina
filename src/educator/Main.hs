
-- | Starting point for running an Educator node

module Main where

import Universum

import System.Wlog (logInfo, logWarning)

import Disciplina.DB (DBParams)
import Disciplina.Educator (EducatorParams (..))
import Disciplina.Launcher (BasicNodeParams (..), prepareAndRunRealMode)

import qualified Params as Params

main :: IO ()
main = do
    Params.EducatorParams {..} <- Params.getEducatorParams
    let educatorParams = EducatorParams
            { epBasicParams = BasicNodeParams
                { bnpLoggingParams = epLogParams
                }
            }
    prepareAndRunRealMode educatorParams $ do
        logInfo "This is the stub for Educator node executable"
        logWarning "Please don't forget to implement everything else!"
