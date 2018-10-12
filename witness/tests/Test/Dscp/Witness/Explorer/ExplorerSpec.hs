module Test.Dscp.Witness.Explorer.ExplorerSpec where

import Data.Default (def)

import Dscp.Core
import Dscp.Crypto
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Mode
import Dscp.Snowdrop.Types
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness

import Test.Dscp.Witness.Common
import Test.Dscp.Witness.Mode

-- | Generate valid transaction and put it into mempool.
createAndSubmitTx
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => Gen SecretKey -> PropertyM m Tx
createAndSubmitTx genSecret = do
    sk <- pick $ mkSecretKeyData <$> genSecret
    outs <- pick $ genSafeTxOuts 100 (choose (1, 5))
    account <- lift . runSdReadM $ fromMaybe def <$> getMempoolAccountMaybe (skAddress sk)

    let txw = createTxw (fcMoney feeConfig) sk (aNonce account) outs
    lift $ addTxToMempool (GMoneyTxWitnessed txw)
    return $ twTx txw

-- | Generate valid publication and put it into mempool.
createAndSubmitPub
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => Gen SecretKey -> PropertyM m PublicationTx
createAndSubmitPub genSecret = do
    sk <- pick $ mkSecretKeyData <$> genSecret
    sig <- pick arbitrary
    lastHeaderHash <- lift . runSdMempol $ getPrivateTipHash (skAddress sk)
    let ptHeader = PrivateBlockHeader
            { _pbhPrevBlock = lastHeaderHash
            , _pbhBodyProof = sig
            , _pbhAtgDelta = mempty
            }
        tx = PublicationTx
            { ptAuthor = skAddress sk
            , ptFeesAmount = unFees $ calcFeePub (fcPublication feeConfig) ptHeader
            , ptHeader
            }
    let txw = signPubTx sk tx
    lift $ addTxToMempool (GPublicationTxWitnessed txw)
    return tx

-- | Dump all mempool transactions into a new block.
dumpBlock
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => SlotId -> m HeaderHash
dumpBlock slotId = do
    block <- createBlock runSdM slotId
    void $ applyBlock block
    return (headerHash block)

-- | Run 'getTransactions' with pagination page by page until all transactions
-- are fetched.
getTransactionsPaged
    :: WitnessWorkMode ctx m
    => Int
    -> Maybe Address
    -> m [[WithBlockInfo Tx]]
getTransactionsPaged chunkSize mAddress = getFrom Nothing
  where
    getFrom mFrom = do
        txList <- getTransactions (Just chunkSize) mFrom mAddress
        next <- maybe (pure []) (getFrom . Just) (plNextId txList)
        return (plItems txList : next)

spec :: Spec
spec = describe "Explorer" $ do
  describe "getTransaction" $ do
    it "Returns existing tx fine" . once $ witnessProperty $ do
        tx <- createAndSubmitTx selectGenesisSecret
        _ <- lift $ dumpBlock 0
        res <- lift $ getTransactionInfo (toGTxId $ GMoneyTx tx)
        return $ wbiItem res === GMoneyTx tx

    it "Mempool txs are taken into account" . once $ witnessProperty $ do
        tx <- createAndSubmitTx selectGenesisSecret
        res <- lift $ getTransactionInfo (toGTxId $ GMoneyTx tx)
        return $ res === WithBlockInfo Nothing (GMoneyTx tx)

    it "Errors on absent tx correctly" . once $ witnessProperty $ do
        gTxId <- pick arbitrary
        _ <- lift $ dumpBlock 0
        lift $ throwsPrism (_LogicError . _LETxAbsent) $
            getTransactionInfo gTxId

  describe "getTransactions" $ do
    it "Returns all transactions at once just fine" . once $ witnessProperty $ do
        n <- pick $ choose (1, 3)
        txs <- replicateM n $ createAndSubmitTx selectGenesisSecret
        _ <- lift $ dumpBlock 0

        res <- lift $ getTransactions Nothing Nothing Nothing
        -- return from recent-first order, discarding genesis transactions
        let resTop = reverse . take n $ plItems res
        -- comparing transactions on their id for prettier errors
        return $ map (toTxId . wbiItem) resTop
                 ===
                 map toTxId txs

    it "Pagination works fine" $ witnessProperty $ do
        txsNum <- pick $ choose (1, 5)
        chunkSize <- pick $ choose (1, 3)
        txs <- replicateM txsNum $ createAndSubmitTx selectGenesisSecret
        _ <- lift $ dumpBlock 0

        res <- lift $ getTransactionsPaged chunkSize Nothing
        return $ conjoin
            [ property $
                  all ((== chunkSize) . length) $
                  maybe [] init (nonEmpty res)

            , map (toTxId . wbiItem) (reverse . take txsNum $ concat res)
              ===
              map toTxId txs
            ]

    it "Skipping transactions works fine with mempool" $ witnessProperty $ do
        chainTxsNum <- pick $ choose (1, 3)
        chainTxs <- replicateM chainTxsNum $ createAndSubmitTx selectGenesisSecret
        _ <- lift $ dumpBlock 0
        memTxsNum <- pick $ choose (0, 3)
        memTxs <- replicateM memTxsNum $ createAndSubmitTx selectGenesisSecret

        let txs = chainTxs <> memTxs
        mSince <- pick $ elements (Nothing : map (Just . toTxId) txs)

        res <- lift $ getTransactions Nothing mSince Nothing
        let expected = maybe id (\since -> dropWhile ((/= since) . toTxId)) mSince (reverse txs)
        let resTop = zipWith const (plItems res) expected

        return $ map (toTxId . wbiItem) resTop
                 ===
                 map toTxId expected

    it "Errors nicely when 'since' transaction is absent" $ witnessProperty $ do
        chainTxsNum <- pick $ choose (1, 5)
        replicateM_ chainTxsNum $ createAndSubmitTx selectGenesisSecret
        _ <- lift $ dumpBlock 0
        memTxsNum <- pick $ choose (0, 3)
        replicateM_ memTxsNum $ createAndSubmitTx selectGenesisSecret

        since <- pick arbitrary

        lift $ throwsPrism (_LogicError . _LETxAbsent) $
            getTransactions Nothing (Just since) Nothing

    it "Blocks info is present" . once $ witnessProperty $ do
        _ <- createAndSubmitTx selectGenesisSecret
        blockHash <- lift $ dumpBlock 0
        _ <- createAndSubmitTx selectGenesisSecret

        res <- lift $ getTransactions Nothing Nothing Nothing
        let tx2 : tx1 : _ = plItems res
        return $ conjoin
            [ fmap biHeaderHash (wbiBlockInfo tx1) === Just blockHash
            , property $ isNothing (wbiBlockInfo tx2)
            ]

    it "Filtering on address works fine (when the address is tx input)" $ witnessProperty $ do
        let selectSecret = oneof [selectGenesisSecret, pure testSomeGenesisSecret]
            interestingAddress = mkAddr $ toPublic testSomeGenesisSecret
        n <- pick $ choose (1, 5)
        txs <- replicateM n $ createAndSubmitTx selectSecret
        _ <- lift $ dumpBlock 0

        let expected = filter (\tx -> interestingAddress `elem` txRelatedAddrs tx) txs
        res <- lift $ getTransactions Nothing Nothing (Just interestingAddress)
        let resTop = reverse . take (length expected) $ plItems res
        return $ map (toTxId . wbiItem) resTop
                 ===
                 map toTxId expected

  describe "getPublications" $ do
    it "Returns all transactions at once just fine" $ once $ witnessProperty $ do
        n <- pick $ choose (1, 3)
        txs <- replicateM n $ createAndSubmitPub (pure testSomeGenesisSecret)
        _ <- lift $ dumpBlock 0

        res <- lift $ getPublications Nothing Nothing Nothing
        let resTop = reverse $ plItems res
        return $ map (toPtxId . wbiItem) resTop
                 ===
                 map toPtxId txs

    it "Filtering on author works fine" $ witnessProperty $ do
        let selectSecret = oneof [selectGenesisSecret, pure testSomeGenesisSecret]
            interestingAddress = mkAddr $ toPublic testSomeGenesisSecret
        n <- pick $ choose (1, 5)
        txs <- replicateM n $ createAndSubmitPub selectSecret
        _ <- lift $ dumpBlock 0

        let expected = filter (\tx -> interestingAddress == ptAuthor tx) txs
        res <- lift $ getPublications Nothing Nothing (Just interestingAddress)
        let resTop = reverse $ plItems res
        return $ map (toPtxId . wbiItem) resTop
                 ===
                 map toPtxId expected

    it "Blocks info is present" . once $ witnessProperty $ do
        _ <- createAndSubmitPub selectGenesisSecret
        blockHash <- lift $ dumpBlock 0
        _ <- createAndSubmitPub selectGenesisSecret

        res <- lift $ getPublications Nothing Nothing Nothing
        let tx2 : tx1 : _ = plItems res
        return $ conjoin
            [ fmap biHeaderHash (wbiBlockInfo tx1) === Just blockHash
            , property $ isNothing (wbiBlockInfo tx2)
            ]

  describe "getHashType" $ do
    it "Handles (mempool) money transactions correctly" . once $ witnessProperty $ do
        tx <- createAndSubmitTx selectGenesisSecret
        res <- lift $ getHashType (toHex $ toTxId tx)
        return $ res === HashIsMoneyTx

    it "Handles (mempool) pub transactions correctly" . once $ witnessProperty $ do
        tx <- createAndSubmitPub selectGenesisSecret
        res <- lift $ getHashType (toHex $ toPtxId tx)
        return $ res === HashIsPublicationTx
