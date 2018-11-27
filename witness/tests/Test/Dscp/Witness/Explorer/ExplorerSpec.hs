module Test.Dscp.Witness.Explorer.ExplorerSpec where

import Control.Lens (at, _Just)
import Data.Default (def)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Loot.Base.HasLens (lensOf)
import Test.QuickCheck (arbitraryBoundedEnum, shuffle)

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Mode
import Dscp.Snowdrop.Types
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness

import Test.Dscp.Witness.Common
import Test.Dscp.Witness.Mode

-- | Given a secret key data and a list of outputs, create a money transaction,
-- sign it and submit it into the mempool.
createAndSubmitTx
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => SecretKeyData -> [TxOut] -> m Tx
createAndSubmitTx sk outs = do
    account <- runSdReadM $
        fromMaybe def <$> getMempoolAccountMaybe (skAddress sk)
    let txw = createTxw (fcMoney feeConfig) sk (aNonce account) outs
    addTxToMempool (GMoneyTxWitnessed txw)
    return $ twTx txw

-- | Generate valid transaction and put it into mempool.
createAndSubmitTxGen
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => Gen SecretKey -> PropertyM m Tx
createAndSubmitTxGen genSecret = do
    sk <- pick $ mkSecretKeyData <$> genSecret
    outs <- pick $ genSafeTxOuts 100 (choose (1, 5))
    lift $ createAndSubmitTx sk outs

-- | Given a secret key data and a Merkle root, create a valid publication
-- which follows a previous one and submit it to the mempool.
createAndSubmitPub
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => SecretKeyData -> MerkleSignature PrivateTx -> m PublicationTx
createAndSubmitPub sk sig = do
    lastHeaderHash <- runSdMempool $ getPrivateTipHash (skAddress sk)
    let ptHeader = PrivateBlockHeader
            { _pbhPrevBlock = lastHeaderHash
            , _pbhBodyProof = sig
            , _pbhAtgDelta = mempty
            }
        tx = PublicationTx
            { ptAuthor = skAddress sk
            , ptFeesAmount = unFees $
                calcFeePub (fcPublication feeConfig) ptHeader
            , ptHeader
            }
        txw = signPubTx sk tx

    addTxToMempool (GPublicationTxWitnessed txw)
    return tx

-- | Generate valid publication and put it into mempool.
createAndSubmitPubGen
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => Gen SecretKey -> PropertyM m PublicationTx
createAndSubmitPubGen genSecret = do
    sk <- pick $ mkSecretKeyData <$> genSecret
    sig <- pick arbitrary
    lift $ createAndSubmitPub sk sig

-- | Dump all mempool transactions into a new block.
dumpBlock
    :: (TestWitnessWorkMode ctx m, WithinWriteSDLock)
    => m HeaderHash
dumpBlock = do
    slotId <- rewindToNextSlot
    let issuerKey = KeyResources . mkSecretKeyData $ testFindSlotOwner slotId

    local (lensOf @(KeyResources WitnessNode) .~ issuerKey) $ do
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

data SpoilVariant
    = WrongAddr
    | WrongHeaderHash
    | WrongProof
    deriving (Show, Eq, Generic, Enum, Bounded)

-- | Spoils a @'MerkleProof'@ by changing some of its internal nodes
-- TODO: spoil better.
spoilMerkleProof
    :: Monad m
    => MerkleProof a
    -> PropertyM m (MerkleProof a)
spoilMerkleProof mproof = do
    newSig <- pick arbitrary
    pure $ mproof { pnSig = newSig }

-- | Spoils a @'FairCV'@ by changing some of its parts to random parts.
-- TODO: remove copy-paste
spoilFairCV
    :: Monad m
    => FairCV Unchecked
    -> PropertyM m (FairCV Unchecked)
spoilFairCV (FairCV cv) = pick arbitraryBoundedEnum >>= \case
    WrongAddr -> do
        oldAddr <- pick . elements $ M.keys cv
        newAddr <- pick arbitrary
        let subCv = fromMaybe (error "impossible") $
                    M.lookup oldAddr cv
        return $ FairCV $ M.insert newAddr subCv $ M.delete oldAddr cv
    WrongHeaderHash -> do
        addr <- pick . elements $ M.keys cv
        let subCv = fromMaybe (error "impossible") $
                    M.lookup addr cv
        oldHhash <- pick . elements $ M.keys subCv
        newHhash <- pick arbitrary
        let proof = fromMaybe (error "impossible") $
                    M.lookup oldHhash subCv
        return $ FairCV $
            M.adjust (M.insert newHhash proof . M.delete oldHhash) addr cv
    WrongProof -> do
        addr <- pick . elements $ M.keys cv
        let subCv = fromMaybe (error "impossible") $
                    M.lookup addr cv
        hhash <- pick . elements $ M.keys subCv
        let proof = fromMaybe (error "impossible") $
                    M.lookup hhash subCv
        newProof <- mkTaggedProof <$> spoilMerkleProof (unTaggedProof proof)
        return $ FairCV $
            cv & at addr . _Just . at hhash . _Just .~ newProof

spec :: Spec
spec = describe "Explorer" $ do
    describe "getTransaction" $ do
        it "Returns existing tx fine" . once $ witnessProperty $ do
            tx <- createAndSubmitTxGen selectGenesisSecret
            _ <- lift dumpBlock
            res <- lift $ getTransactionInfo (toGTxId $ GMoneyTx tx)
            return $ wbiItem res === GMoneyTx tx

        it "Mempool txs are taken into account" . once $ witnessProperty $ do
            tx <- createAndSubmitTxGen selectGenesisSecret
            res <- lift $ getTransactionInfo (toGTxId $ GMoneyTx tx)
            return $ res === WithBlockInfo Nothing (GMoneyTx tx)

        it "Errors on absent tx correctly" . once $ witnessProperty $ do
            gTxId <- pick arbitrary
            _ <- lift dumpBlock
            lift $ throwsPrism (_LogicError . _LETxAbsent) $
                getTransactionInfo gTxId

    describe "getTransactions" $ do
        it "Returns all transactions at once just fine" . once $ witnessProperty $ do
            n <- pick $ choose (1, 3)
            txs <- replicateM n $ createAndSubmitTxGen selectGenesisSecret
            _ <- lift dumpBlock

            res <- lift $ getTransactions Nothing Nothing Nothing
            -- return from recent-first order, discarding genesis transactions
            let resTop = reverse . take n $ plItems res
            -- comparing transactions on their id for prettier errors
            return $ map (toTxId . wbiItem) resTop ===
                map toTxId txs

        it "Pagination works fine" $ witnessProperty $ do
            txsNum <- pick $ choose (1, 5)
            chunkSize <- pick $ choose (1, 3)
            txs <- replicateM txsNum $ createAndSubmitTxGen selectGenesisSecret
            _ <- lift dumpBlock

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
            chainTxs <- replicateM chainTxsNum $ createAndSubmitTxGen selectGenesisSecret
            _ <- lift dumpBlock
            memTxsNum <- pick $ choose (0, 3)
            memTxs <- replicateM memTxsNum $ createAndSubmitTxGen selectGenesisSecret

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
            replicateM_ chainTxsNum $ createAndSubmitTxGen selectGenesisSecret
            _ <- lift dumpBlock
            memTxsNum <- pick $ choose (0, 3)
            replicateM_ memTxsNum $ createAndSubmitTxGen selectGenesisSecret

            since <- pick arbitrary

            lift $ throwsPrism (_LogicError . _LETxAbsent) $
                getTransactions Nothing (Just since) Nothing

        it "Blocks info is present" . once $ witnessProperty $ do
            _ <- createAndSubmitTxGen selectGenesisSecret
            blockHash <- lift dumpBlock
            _ <- createAndSubmitTxGen selectGenesisSecret

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
            txs <- replicateM n $ createAndSubmitTxGen selectSecret
            _ <- lift dumpBlock

            let expected = filter (\tx -> interestingAddress `elem` txRelatedAddrs tx) txs
            res <- lift $ getTransactions Nothing Nothing (Just interestingAddress)
            let resTop = reverse . take (length expected) $ plItems res
            return $ map (toTxId . wbiItem) resTop
                ===
                map toTxId expected

    describe "getPublications" $ do
        it "Returns all transactions at once just fine" $ once $ witnessProperty $ do
            n <- pick $ choose (1, 3)
            txs <- replicateM n $ createAndSubmitPubGen (pure testSomeGenesisSecret)
            _ <- lift dumpBlock

            res <- lift $ getPublications Nothing Nothing Nothing
            let resTop = reverse $ plItems res
            return $ map (toPtxId . wbiItem) resTop
                ===
                map toPtxId txs

        it "Filtering on author works fine" $ witnessProperty $ do
            let selectSecret = oneof [selectGenesisSecret, pure testSomeGenesisSecret]
                interestingAddress = mkAddr $ toPublic testSomeGenesisSecret
            n <- pick $ choose (1, 5)
            txs <- replicateM n $ createAndSubmitPubGen selectSecret
            _ <- lift dumpBlock

            let expected = filter (\tx -> interestingAddress == ptAuthor tx) txs
            res <- lift $ getPublications Nothing Nothing (Just interestingAddress)
            let resTop = reverse $ plItems res
            return $ map (toPtxId . wbiItem) resTop
                ===
                map toPtxId expected

        it "Blocks info is present" . once $ witnessProperty $ do
            _ <- createAndSubmitPubGen selectGenesisSecret
            blockHash <- lift dumpBlock
            _ <- createAndSubmitPubGen selectGenesisSecret

            res <- lift $ getPublications Nothing Nothing Nothing
            let tx2 : tx1 : _ = plItems res
            return $ conjoin
                [ fmap biHeaderHash (wbiBlockInfo tx1) === Just blockHash
                , property $ isNothing (wbiBlockInfo tx2)
                ]

    describe "getHashType" $ do
        it "Handles (mempool) money transactions correctly" . once $ witnessProperty $ do
            tx <- createAndSubmitTxGen selectGenesisSecret
            res <- lift $ getHashType (toHex $ toTxId tx)
            return $ res === HashIsMoneyTx

        it "Handles (mempool) pub transactions correctly" . once $ witnessProperty $ do
            tx <- createAndSubmitPubGen selectGenesisSecret
            res <- lift $ getHashType (toHex $ toPtxId tx)
            return $ res === HashIsPublicationTx

    describe "checkFairCV" $
        it "FairCV validation works correctly" $ witnessProperty $ do
            -- Select some secret keys for educators,
            numEducators <- pick $ choose (1, length testGenesisSecrets)
            sks <- map mkSecretKeyData . take numEducators <$>
                   pick (shuffle testGenesisSecrets)

            -- create a bunch of private blocks and according publications
            -- for every educator, picking FairCV randomly from those,
            cvPairs <- forM sks $ \sk -> do
                pChainLen <- pick $ choose (1, 5)
                edCvs <- replicateM pChainLen $ do
                    ptxs <- pick $ listOf1 arbitrary
                    proofIdxs <- pick $
                        S.fromList <$> sublistOf [0 :: Word32 .. fromIntegral (length ptxs - 1)]

                    let mtree = fromList ptxs
                        msig = getMerkleRoot mtree
                        mProof = mkTaggedProof <$> mkMerkleProof mtree proofIdxs
                    pub <- lift $ createAndSubmitPub sk msig

                    let hhash = hash $ pub ^. ptHeaderL
                    return $ (hhash, ) <$> mProof

                let edCvMap = M.fromList $ catMaybes edCvs
                return $ if M.null edCvMap
                         then Nothing
                         else Just (skAddress sk, edCvMap)

            let fairCv = FairCV . M.fromList $ catMaybes cvPairs
            fairCvInvalid <- spoilFairCV fairCv

            -- Do positive and negative case in one `it` to reduce test
            -- running time
            checkResGood <- lift $ checkFairCV fairCv
            checkResBad <- lift $ checkFairCV fairCvInvalid
            return $
                fullyValid checkResGood &&
                not (fullyValid checkResBad)
