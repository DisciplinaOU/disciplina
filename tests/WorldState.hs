
module WorldState (tests) where

import Common

import qualified Disciplina.WorldState as World
import qualified Debug.Trace           as Debug

tests :: [Test]
tests =
    [ testGroup "World/Transactions"
        [ testProperty "another server node can apply transactions" $
            \box @ (Sandbox world transactions a e b _) ->
                World.Server world `worldMProperty` do
                    World.assumeTransaction (head transactions)
                    return True

        , testProperty "another client node can apply transactions" $
            \box @ (Sandbox world transactions a e b _) ->
                let worldProof = World.diffWorldState def world in
                World.Client worldProof `worldMProperty` do
                    for_ transactions World.assumeTransaction
                    return True

        , testProperty "impossible to send more than you have" $
            \box @ (Sandbox world transactions a e b limit) ->
                expectFailure $ do
                    World.Server world `worldMProperty` do
                        World.impersonate a $ do
                            transaction <- World.plan [World.TransferTokens b (limit + 1)]
                            World.connectTransaction transaction
                            return False
        ]
    ]
