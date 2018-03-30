
module WorldState (tests) where

import Common

import qualified Disciplina.WorldState as World
import qualified Debug.Trace           as Debug

tests :: [Test]
tests =
    [ testGroup "World/Transactions"
        [ testProperty "another server node can apply transactions" $
            \box @ (Sandbox world transactions a e b _) ->
                World.Server world `worldTProperty` do
                    World.assumeTransaction (head transactions)
                    return True

        , testProperty "another client node can apply transactions" $
            \box @ (Sandbox world transactions a e b _) -> do
                World.Server world `worldTProperty` do
                    worldProof <- World.diffWorldState def world
                    World.Client worldProof `World.isolate` do
                        for_ transactions World.assumeTransaction
                        return True

        , testProperty "impossible to send more than you have" $
            \box @ (Sandbox world transactions a e b limit) ->
                expectFailure $ do
                    World.Server world `worldTProperty` do
                        World.impersonate a $ do
                            transaction <- World.plan [World.TransferTokens b (limit + 1)]
                            World.connectTransaction transaction
                            return False

        , testProperty "impossible to do things being an absent entity" $
            \box @ (Sandbox world transactions a e b limit) ->
                expectFailure $ do
                    World.Server world `worldTProperty` do
                        World.impersonate def $ do
                            transaction <- World.plan [World.TransferTokens b (limit - 1)]
                            World.connectTransaction transaction
                            return False
        ]
    ]
