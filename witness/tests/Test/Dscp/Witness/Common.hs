module Test.Dscp.Witness.Common
    ( selectGenesisSecret
    , genSafeOutAddr
    , genSafeTxOuts
    , createAndSubmitTx
    , createAndSubmitTxGen
    , createAndSubmitPub
    , createAndSubmitPubGen
    , createAndSubmitGTxGen
    , genPrivateTx
    , dumpBlock
    ) where

import Data.Default (def)
import Loot.Base.HasLens (lensOf)
import Loot.Config (option)

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Snowdrop
import Dscp.Util.Test
import Dscp.Witness

-- | Arbitrarly choose genesis secret.
selectGenesisSecret :: Gen SecretKey
selectGenesisSecret = elements testGenesisSecrets

-- | Generate output `Address` which does not make transaction invalid.
genSafeOutAddr :: Gen Address
genSafeOutAddr = arbitrary `suchThat` (`notElem` testGenesisAddresses)

-- | Generate output `TxOut` which does not make transaction invalid.
genSafeTxOuts :: Word64 -> Gen Word32 -> Gen [TxOut]
genSafeTxOuts maxVal genN = do
    n <- fromIntegral <$> genN
    txOutAddrs <- vectorUniqueOf n genSafeOutAddr
    txOutValues <- vectorOf n $ Coin <$> choose (1, maxVal)
    return $ zipWith TxOut txOutAddrs txOutValues

-- | Given a secret key data and a list of outputs, create a money transaction,
-- sign it and submit it into the mempool.
createAndSubmitTx
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => SecretKeyData -> [TxOut] -> m Tx
createAndSubmitTx sk outs = do
    account <- runSdReadM @'ChainAndMempool $
        fromMaybe def <$> getAccountMaybe (skAddress sk)
    let txw = createTxw (feeConfig ^. option #money) sk (aNonce account) outs
    addTxToMempool (GMoneyTxWitnessed txw)
    return $ twTx txw

-- | Generate valid money transaction and put it into mempool.
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
                calcFeePub (feeConfig ^. option #publication) ptHeader
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

-- | Generate valid transaction and put it into mempool.
createAndSubmitGTxGen
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => Gen SecretKey -> PropertyM m GTx
createAndSubmitGTxGen genSecret = do
    -- this looks weird, but allows to prevent bugs when new transaction types appear
    pick arbitrary >>= \case
        GMoneyTx{} -> GMoneyTx <$> createAndSubmitTxGen genSecret
        GPublicationTx{} -> GPublicationTx <$> createAndSubmitPubGen genSecret

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

-- | Dump all mempool transactions into a new block.
dumpBlock
    :: (TestWitnessWorkMode ctx m, WithinWriteSDLock)
    => m HeaderHash
dumpBlock = do
    slotId <- rewindToNextSlot
    let issuerKey = KeyResources . mkSecretKeyData $ testFindSlotOwner slotId

    local (lensOf @(KeyResources WitnessNode) .~ issuerKey) $ do
        block <- createBlock slotId
        void $ applyBlock block
        return (headerHash block)
