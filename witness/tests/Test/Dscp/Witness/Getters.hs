module Test.Dscp.Witness.Getters where

import Control.Lens (traversed)
import qualified Data.List as L
import Test.QuickCheck.Monadic (pre)

import Dscp.Core
import Dscp.Crypto
import Dscp.Util.Test
import Dscp.Witness

import Test.Dscp.Witness.Common
import Test.Dscp.Witness.Mode


spec_Witness_getters :: Spec
spec_Witness_getters = do
    describe "getPublicationByHeaderHash" $ do
        it "Finds tx in chain/mempool when present" $ witnessProperty $ do
            txsNum <- pick $ choose (1, 5)
            txs <- replicateM txsNum $ do
                tx <- createAndSubmitGTxGen selectGenesisSecret
                withProbability_ (1/3) $ lift dumpBlock
                return tx

            let pubs = txs ^.. traversed . _GPublicationTx
            pre (not $ null pubs)
            let pub = L.head pubs

            pubTw <-
                lift . runSdMempool $ getPublicationByHeaderHash (hash $ ptHeader pub)
            return (fmap ptwTx pubTw == Just pub)

        it "Finds no tx in absert" $ witnessProperty $ do
            txsNum <- pick $ choose (1, 5)
            replicateM_ txsNum $ do
                void $ createAndSubmitGTxGen selectGenesisSecret
                withProbability_ (1/3) $ lift dumpBlock

            badPubHash <- pick arbitrary

            pubTw <- lift . runSdMempool $ getPublicationByHeaderHash badPubHash
            return (pubTw == Nothing)
