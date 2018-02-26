
import Test.Framework (Test, defaultMain, testGroup)

import qualified WorldState

main = defaultMain
    (   []
    ++  WorldState.tests
    )
