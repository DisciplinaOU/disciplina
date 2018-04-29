module Test.Disciplina.WorldState where

import Test.Common

import qualified Disciplina.WorldState as World

spec_Transactions :: Spec
spec_Transactions = describe "Transactions" $ do
    it "can be applied by another server node" $ property $
        \(Sandbox world (transaction : _) _ _ _ _) ->
            World.Server world `worldTProperty` do
                World.replayTransaction transaction
                return True

    it "can be applied by another client node" $ property $
        \(Sandbox world transactions _ _ _ _) -> do
            World.Server world `worldTProperty` do
                worldProof <- World.diffWorldState def world

                World.Client worldProof `World.isolate` do
                    for_ transactions World.replayTransaction
                    return True

    it "is impossible to send more than you have" $ property $
        \(Sandbox world _ a _ b limit) ->
            expectFailure $ do
                World.Server world `worldTProperty` do
                    World.impersonate a $ do
                        transaction <- World.plan [World.TransferTokens b (limit + 1)]
                        _ <- World.playTransaction transaction
                        return False

    it "is impossible to do things being an absent entity" $ property $
        \(Sandbox world _ _ _ b limit) ->
            expectFailure $ do
                World.Server world `worldTProperty` do
                    World.impersonate def $ do
                        transaction <- World.plan [World.TransferTokens b (limit - 1)]
                        _ <- World.playTransaction transaction
                        return False

spec_Blocks :: Spec
spec_Blocks = describe "Blocks" $ do
    it "can be added by a client to the blockchain" $ property $
        \(Sandbox world transactions _ _ _ _) -> do
            World.Server world `worldTProperty` do
                worldProof <- World.diffWorldState def world
                block      <- World.generateBlock $ (^.World.wpBody) <$> transactions

                World.Client worldProof `World.isolate` do
                    World.replayBlock block

                return True

    it "can be added by another server node to the blockchain" $ property $
        \(Sandbox world transactions _ _ _ _) -> do
            World.Server world `worldTProperty` do
                block <- World.dryRun $ do
                    World.generateBlock $ (^.World.wpBody) <$> transactions

                World.replayBlock block

                return True
