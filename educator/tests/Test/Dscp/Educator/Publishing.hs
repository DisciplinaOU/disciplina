module Test.Dscp.Educator.Publishing where

import Universum

import Dscp.Core
import Dscp.DB.SQL
import Dscp.Educator
import Dscp.Util
import Dscp.Util.Test

import Test.Dscp.DB.SQL.Mode
import Test.Dscp.Educator.Mode
import Test.Dscp.Educator.Web.Scenarios

spec_Private_blocks_publishing :: Spec
spec_Private_blocks_publishing = specWithTempPostgresServer $ do
    it "Single block is successfully created" $ educatorPropertyM $ do
        env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
        let txs = ordNub . tiList $ ctePrivateTxs env

        _ <- lift $ do
            transact $ prepareAndCreateSubmissions env
            transact $ forM_ txs createTransaction
            nothingToPanic "No block created" <$> dumpPrivateBlock

        return True

    it "Several blocks are successfully created" $ educatorPropertyM $ do
        env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
        txsPacks <- pick . groupArbitrarily . ordNub . tiList $ ctePrivateTxs env

        lift $ do
            transact $ prepareAndCreateSubmissions env
            void $ fmap concat . forM txsPacks $ \txs ->
                forM txs $ \tx -> do
                    void $ transact $ createTransaction tx
                    nothingToPanic "No block created" <$> dumpPrivateBlock

        return True
