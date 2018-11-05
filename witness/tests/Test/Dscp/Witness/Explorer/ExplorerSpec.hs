module Test.Dscp.Witness.Explorer.ExplorerSpec where

import Data.Default (def)

import Dscp.Core
import Dscp.Crypto
import Dscp.Snowdrop.Types
import Dscp.Snowdrop.Mode
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
    account <- lift $ fromMaybe def <$> getMempoolAccountMaybe (skAddress sk)

    let txw = createTxw (fcMoney feeConfig) sk (aNonce account) outs
    isNew <- lift $ addTxToMempool (GMoneyTxWitnessed txw)
    unless isNew $ error "Duplicated transaction???"
    return $ twTx txw

-- | Generate valid publication and put it into mempool.
createAndSubmitPub
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => Gen SecretKey -> PropertyM m PublicationTx
createAndSubmitPub genSecret = do
    sk <- pick $ mkSecretKeyData <$> genSecret
    sig <- pick arbitrary
    lastHeaderHash <- lift . runSdMempoolRead $ getPrivateTipHash (skAddress sk)
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
    isNew <- lift $ addTxToMempool (GPublicationTxWitnessed txw)
    unless isNew $ error "Duplicated transaction???"
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
  describe "getTransactions" $ do
    it "Returns all transactions at once just fine" $ witnessProperty $ do
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

    it "Blocks info is present" $ witnessProperty $ do
        -- TODO [DSCP-335] Uncomment when mempool is taken into consideration
        -- And also adjust one similar test below.
        -- _ <- createAndSubmitTx selectGenesisSecret
        -- blockHash <- lift $ dumpBlock 0
        -- _ <- createAndSubmitTx selectGenesisSecret

        -- res <- lift $ getTransactions Nothing Nothing Nothing
        -- let tx2 : tx1 : _ = traceShowId $ plItems res
        -- return $ conjoin
        --     [ fmap biHeaderHash (wbiBlockInfo tx1) === Just blockHash
        --     , property $ isNothing (wbiBlockInfo tx2)
        --     ]

        _ <- createAndSubmitTx selectGenesisSecret
        blockHash <- lift $ dumpBlock 0

        res <- lift $ getTransactions Nothing Nothing Nothing
        let tx : _ = plItems res
        return $ fmap biHeaderHash (wbiBlockInfo tx) === Just blockHash

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
    it "Returns all transactions at once just fine" $ witnessProperty $ do
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

    it "Blocks info is present" $ witnessProperty $ do
        _ <- createAndSubmitPub selectGenesisSecret
        blockHash <- lift $ dumpBlock 0

        res <- lift $ getPublications Nothing Nothing Nothing
        let tx : _ = plItems res
        return $ fmap biHeaderHash (wbiBlockInfo tx) === Just blockHash
