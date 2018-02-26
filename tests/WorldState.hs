
module WorldState (tests) where

import Common

import qualified Disciplina.WorldState as World
import qualified Debug.Trace           as Debug

tests :: [Test]
tests =
    [ testGroup "World/Transactions"
        [ testProperty "another server node can apply transactions" $
            \box @ (Sandbox world transactions) ->
                World.Server world `worldMProperty` do
                    World.assumeTransaction (head transactions)
                    return True

        -- , testProperty "another client node can apply transactions" $
        --     \box @ (Sandbox world transactions) ->
        --         let worldProof = World.diffWorldState def world in
        --         World.Client worldProof `worldMProperty` do
        --             for_ transactions World.assumeTransaction
        --             return True

        ]
    ]
