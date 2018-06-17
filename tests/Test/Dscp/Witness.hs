module Test.Dscp.Witness where

import Test.Common

import qualified Dscp.Witness as Witness

spec_Transactions :: Spec
spec_Transactions = describe "Transactions" $ do
    it "can be applied by another server node" $ property $
        \(Sandbox world (transaction : _) _ _ _ _) ->
            Witness.Server world `worldTProperty` do
                Witness.replayTransaction transaction
                return True

    it "can be applied by another client node" $ property $
        \(Sandbox world transactions _ _ _ _) -> do
            Witness.Server world `worldTProperty` do
                worldProof <- Witness.diffWorldState def world

                Witness.Client worldProof `Witness.isolate` do
                    for_ transactions Witness.replayTransaction
                    return True

    it "is impossible to send more than you have" $ property $
        \(Sandbox world _ a _ b limit) ->
            expectFailure $ do
                Witness.Server world `worldTProperty` do
                    Witness.impersonate a $ do
                        transaction <- Witness.plan [Witness.TransferTokens b (limit + 1)]
                        _ <- Witness.playTransaction transaction
                        return False

    it "is impossible to do things being an absent entity" $ property $
        \(Sandbox world _ _ _ b limit) ->
            expectFailure $ do
                Witness.Server world `worldTProperty` do
                    Witness.impersonate def $ do
                        transaction <- Witness.plan [Witness.TransferTokens b (limit - 1)]
                        _ <- Witness.playTransaction transaction
                        return False

spec_Blocks :: Spec
spec_Blocks = describe "Blocks" $ do
    it "can be added by a client to the blockchain" $ property $
        \(Sandbox world transactions _ _ _ _) -> do
            Witness.Server world `worldTProperty` do
                worldProof <- Witness.diffWorldState def world
                block      <- Witness.generateBlock $ (^.Witness.wpBody) <$> transactions

                Witness.Client worldProof `Witness.isolate` do
                    Witness.replayBlock block

                return True

    it "can be added by another server node to the blockchain" $ property $
        \(Sandbox world transactions _ _ _ _) -> do
            Witness.Server world `worldTProperty` do
                block <- Witness.dryRun $ do
                    Witness.generateBlock $ (^.Witness.wpBody) <$> transactions

                Witness.replayBlock block

                return True
