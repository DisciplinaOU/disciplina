module Main where

import Universum

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Disciplina.Core.ATG as ATG
import qualified Test.Disciplina.WorldState as WS

main :: IO ()
main = defaultMain $
    testGroup "Disciplina"
    [ WS.tests
    , ATG.tests
    ]
