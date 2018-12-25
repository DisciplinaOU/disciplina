module Test.Dscp.Witness.Explorer.Explorer where

import Data.Default (def)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Exts (fromList)
import Test.QuickCheck (arbitraryBoundedEnum, shuffle)
import Test.QuickCheck.Monadic (pre)

import Dscp.Core
import Dscp.Crypto
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.ReadMode
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
    account <- runSdReadM @'ChainAndMempool $
        fromMaybe def <$> getAccountMaybe (skAddress sk)
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

-- | Generate a @'PrivateTx'@ featuring submission from a student with
-- given secret key.
genPrivateTx :: SecretKeyData -> Gen PrivateTx
genPrivateTx skData = do
    timestamp <- arbitrary
    contentsHash <- arbitrary
    assignmentHash <- arbitrary
    grade <- arbitrary

    let sub = Submission
              { _sStudentId = skAddress skData
              , _sContentsHash = contentsHash
              , _sAssignmentHash = assignmentHash
              }

    return PrivateTx
        { _ptGrade = grade
        , _ptTime = timestamp
        , _ptSignedSubmission = SignedSubmission
            { _ssSubmission = sub
            , _ssWitness = SubmissionWitness
                { _swKey = skPublic skData
                , _swSig = sign (skSecret skData) (hash sub)
                }
            }
        }

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
    | WrongStudent
    | WrongHeaderHash
    | WrongProof
    deriving (Show, Eq, Generic, Enum, Bounded)

-- | Spoils a @'MerkleProof'@ by changing some of its internal nodes
spoilMerkleProof
    :: (Eq a, Arbitrary a)
    => MerkleProof a
    -> Gen (MerkleProof a)
spoilMerkleProof (ProofBranch l r) = do
    spoilLeft <- arbitrary
    if spoilLeft
        then ProofBranch <$> spoilMerkleProof l <*> pure r
        else ProofBranch l <$> spoilMerkleProof r
spoilMerkleProof (ProofLeaf v) =
    ProofLeaf <$> (arbitrary `suchThat` (/= v))
spoilMerkleProof (ProofPruned sig) =
    ProofPruned <$> (arbitrary `suchThat` (/= sig))

-- | Helper function to change arbitrary key-value pair in the map.
-- Assumes that the map is not empty.
editSomeKV :: Ord k => Map k v -> (k -> v -> Gen (k, v)) -> Gen (Map k v)
editSomeKV mp action = do
    (k, v) <- elements $ M.toList mp
    (k', v') <- action k v
    pure $ M.insert k' v' $ M.delete k mp

-- | Spoils a @'FairCV'@ by changing some of its parts to random parts.
spoilFairCV
    :: FairCV
    -> Gen FairCV
spoilFairCV (FairCV sAddr sName cv) = do
    var <- arbitraryBoundedEnum
    case var of
        WrongStudent ->
            (\addr -> FairCV addr sName cv) <$>
            arbitrary `suchThat` (/= sAddr)
        _ -> fmap (FairCV sAddr sName) $
             editSomeKV cv $ case var of
            WrongAddr -> \oldAddr subCv ->
                (,subCv) <$> arbitrary `suchThat` (/= oldAddr)
            WrongHeaderHash -> \addr subCv -> do
                subCv' <- editSomeKV subCv $ \oldHash proof ->
                    (,proof) <$> arbitrary `suchThat` (/= oldHash)
                return (addr, subCv')
            WrongProof -> \addr subCv -> do
                subCv' <- editSomeKV subCv $ \hhash proof -> do
                    proof' <- spoilMerkleProof proof
                    return (hhash, proof')
                return (addr, subCv')
            _ -> error "already ruled out"

spec_Explorer :: Spec
spec_Explorer = describe "Explorer" $ do
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
            -- Select a student address and name,
            sKeyData <- mkSecretKeyData . mkPrivKey <$> pick arbitrary
            let sAddr = skAddress sKeyData
                sName = "John Doe" -- why not

            -- select some secret keys for educators,
            numEducators <- pick $ choose (1, length testGenesisSecrets)
            sks <- map mkSecretKeyData . take numEducators <$>
                   pick (shuffle testGenesisSecrets)

            -- create a bunch of private blocks and according publications
            -- for every educator, picking FairCV randomly from those,
            cvPairsMs <- forM sks $ \sk -> do
                pChainLen <- pick $ choose (1, 5)
                edCvs <- replicateM pChainLen $ do
                    ptxs <- pick $ listOf1 (genPrivateTx sKeyData)
                    proofIdxs <- pick $
                        S.fromList <$> sublistOf [0 :: Word32 .. fromIntegral (length ptxs - 1)]

                    let mtree = fromList ptxs
                        msig = getMerkleRoot mtree
                        mProof = mkMerkleProof mtree proofIdxs
                    pub <- lift $ createAndSubmitPub sk msig

                    let hhash = hash $ pub ^. ptHeaderL
                    return $ (hhash, ) <$> mProof

                let edCvMap = M.fromList $ catMaybes edCvs
                return $ if M.null edCvMap
                         then Nothing
                         else Just (skAddress sk, edCvMap)

            let cvPairs = catMaybes cvPairsMs
            pre $ not (null cvPairs)

            let fairCv = FairCV sAddr sName $ M.fromList cvPairs
            fairCvInvalid <- pick $ spoilFairCV fairCv

            -- Do positive and negative case in one `it` to reduce test
            -- running time
            checkResGood <- lift $ checkFairCV fairCv
            checkResBad <- lift $ checkFairCV fairCvInvalid
            let goodCheck =
                    counterexample "Valid FairCV is not checked correctly" $
                    fairCVFullyValid checkResGood
                badCheck =
                    counterexample "Invalid FairCV is not rejected" $
                    not $ fairCVFullyValid checkResBad

            return $ goodCheck .&&. badCheck
