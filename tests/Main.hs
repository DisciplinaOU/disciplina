
import Universum

import Test.Framework (defaultMain)

import qualified WorldState

main :: IO ()
main = defaultMain
    (   []
    ++  WorldState.tests
    )
