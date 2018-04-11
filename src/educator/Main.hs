
-- | Starting point for running an Educator node

module Main where

import Universum

import Mockable (Production (..), runProduction)
import System.Wlog (logInfo, logWarning)

import Disciplina.Launcher (BasicNodeParams (..), bracketBasicNodeResources, runBasicRealMode)

import Params (EducatorParams (..), getEducatorParams)

main :: IO ()
main = do
    EducatorParams {..} <- getEducatorParams
    let basicParams = BasicNodeParams epLogParams
    runProduction . bracketBasicNodeResources basicParams $
        \nr -> runBasicRealMode nr $ do
            logInfo "This is the stub for Educator node executable"
            logWarning "Please don't forget to implement everything else!"
