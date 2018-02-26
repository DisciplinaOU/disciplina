
module WorldState (tests) where

import Common

import qualified Disciplina.WorldState as World
import qualified Debug.Trace           as Debug

tests :: [Test]
tests =
    [ testGroup "World/Transactions"
        [ testProperty "yes" True
        , testProperty "no" False
        ]
    ]
