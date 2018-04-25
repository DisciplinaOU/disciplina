
module Test.Disciplina.WorldState (tests) where

import Test.Common

import qualified Disciplina.WorldState as World
--import qualified Debug.Trace           as Debug

tests :: [Test]
tests =
    [ testGroup "World/Transactions"
        [ testProperty "another server node can apply transactions" $
            \(Sandbox world (transaction : _) _ _ _ _) ->
                World.Server world `worldTProperty` do
                    World.replayTransaction transaction
                    return True

        , testProperty "another client node can apply transactions" $
            \(Sandbox world transactions _ _ _ _) -> do
                World.Server world `worldTProperty` do
                    worldProof <- World.diffWorldState def world

                    World.Client worldProof `World.isolate` do
                        for_ transactions World.replayTransaction
                        return True

        , testProperty "impossible to send more than you have" $
            \(Sandbox world _ a _ b limit) ->
                expectFailure $ do
                    World.Server world `worldTProperty` do
                        World.impersonate a $ do
                            transaction <- World.plan [World.TransferTokens b (limit + 1)]
                            _ <- World.playTransaction transaction
                            return False

        , testProperty "impossible to do things being an absent entity" $
            \(Sandbox world _ _ _ b limit) ->
                expectFailure $ do
                    World.Server world `worldTProperty` do
                        World.impersonate def $ do
                            transaction <- World.plan [World.TransferTokens b (limit - 1)]
                            _ <- World.playTransaction transaction
                            return False
        ]
    , testGroup "World/Blocks"
        [ testProperty "Client can add block to the blockchain" $
            \(Sandbox world transactions _ _ _ _) -> do
                World.Server world `worldTProperty` do
                    worldProof <- World.diffWorldState def world
                    block      <- World.generateBlock $ (^.World.wpBody) <$> transactions

                    World.Client worldProof `World.isolate` do
                        World.replayBlock block

                    return True

        , testProperty "Client can add block to the blockchain" $
            \(Sandbox world transactions _ _ _ _) -> do
                World.Server world `worldTProperty` do
                    block <- World.dryRun $ do
                        World.generateBlock $ (^.World.wpBody) <$> transactions

                    World.replayBlock block

                    return True

        ]
    ]
