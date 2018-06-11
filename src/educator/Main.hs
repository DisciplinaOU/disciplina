
-- | Starting point for running an Educator node

module Main where

import Universum

import System.Wlog (logInfo, logWarning)

import Disciplina.DB (DBType (EducatorDB))
import Disciplina.Launcher (BasicNodeParams (..), bracketBasicNodeResources, runEducatorRealMode)

import Params (EducatorParams (..), getEducatorParams)

main :: IO ()
main = do
    EducatorParams {..} <- getEducatorParams
    let basicParams = BasicNodeParams
            { bnpLoggingParams = epLogParams
            , bnpDBType        = EducatorDB
            , bnpDBPath        = epDbPath
            }
    bracketBasicNodeResources basicParams $
        \nr -> runEducatorRealMode nr $ do
            logInfo "This is the stub for Educator node executable"
            logWarning "Please don't forget to implement everything else!"
