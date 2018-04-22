module Main where

import Universum

import Test.Framework (defaultMain)

import qualified Test.Disciplina.Core.ATG as ATG
import qualified Test.Disciplina.WorldState as WS

main :: IO ()
main = defaultMain
    (  []
    ++ WS.tests
    ++ ATG.tests
    )
