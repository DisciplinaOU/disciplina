{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Test.Dscp.Witness.Tx.PublicationTxSpec where

import Control.Lens (_last)
import qualified GHC.Exts as Exts
import Test.QuickCheck.Modifiers (Positive (..))
import Test.QuickCheck.Monadic (pre)

import Dscp.Core
import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness
import Test.Dscp.Witness.Mode

genSmallMerkleSignature :: Gen (MerkleSignature a)
genSmallMerkleSignature = MerkleSignature <$> arbitrary <*> choose (0, 100)

genPublicationChain
    :: HasWitnessConfig
    => Positive Int -> SecretKey -> Gen (NonEmpty PublicationTx)
genPublicationChain (Positive n) secret
    | n <= 0 = error "genPublicationChain: n <= 0"
    | otherwise = do
        let addr = mkAddr (toPublic secret)
        sigs <- vectorUniqueOf n genSmallMerkleSignature
        return . Exts.fromList . fix $ \pubTxs ->
            zip sigs (genesisHeaderHash : map (hash . ptHeader) pubTxs) <&>
              \(sig, prevHeaderHash) ->
                let ptHeader = PrivateBlockHeader
                        { _pbhPrevBlock = prevHeaderHash
                        , _pbhBodyProof = sig
                        , _pbhAtgDelta = mempty
                        }
                in PublicationTx
                { ptAuthor = addr
                , ptFeesAmount = unFees $ calcFeePub feeCoefficients (mrSize sig)
                , ptHeader
                }

author :: SecretKey
author = detGen 21 $ elements testGenesisSecrets

deriving instance Num a => Num (Positive a)

submitPub
    :: (MempoolCtx ctx m, WithinWriteSDLock)
    => PublicationTxWitnessed -> m ()
submitPub tx = do
    new <- addTxToMempool (GPublicationTxWitnessed tx)
    unless new $ error "Duplicated transaction in test scenario"

spec :: Spec
spec = describe "Publication tx expansion + validation" $ do
    it "First correct tx is fine" $ witnessProperty $ do
        pub :| [] <- pick $ genPublicationChain 1 author
        let tw = signPubTx author pub
        lift . noThrow $ submitPub tw

    it "Consequent txs are fine" $ witnessProperty $ do
        chainLen <- pick arbitrary
        pubs     <- pick $ genPublicationChain chainLen author
        let tws = map (signPubTx author) pubs
        lift . noThrow $ mapM_ submitPub tws

    it "Tx with wrong previous hash isn't fine" $ witnessProperty $ do
        chainLen <- pick $ Positive <$> choose (1, 4)
        pubs     <- pick $ genPublicationChain chainLen author
        badPubs  <- pick $ shuffleNE pubs
        pre (pubs /= badPubs)
        let badTws = map (signPubTx author) badPubs
        lift $ throwsSome $ mapM_ submitPub badTws

    it "Foreign author in the chain is not fine" $ witnessProperty $ do
        otherSecret <- pick (arbitrary `suchThat` (/= author))
        let otherAddr = mkAddr (toPublic otherSecret)
        chainLen    <- pick $ Positive <$> choose (2, 5)
        pubs        <- pick $ genPublicationChain chainLen author
        let badPubs = pubs & _tailNE . _last . ptAuthorL .~ otherAddr
        let badTws = map (signPubTx author) badPubs
        lift $ do
            mapM_ submitPub (init badTws)
            throwsSome $ submitPub (last badTws)

    it "Forking publications chain isn't fine" $ witnessProperty $ do
        chainLen <- pick $ Positive <$> choose (2, 4)
        pubs     <- pick $ genPublicationChain chainLen author

        forkPub' <- pick $ elements (toList pubs)
        forkPub <- pick arbitrary <&> \ptHeader -> forkPub'{ ptHeader }
        pre (forkPub /= forkPub')
        let badPubs = pubs <> one forkPub
        let badTws = map (signPubTx author) badPubs
        lift $ do
            mapM_ submitPub (init badTws)
            throwsSome $ submitPub (last badTws)

    it "Loops are not fine" $ witnessProperty $ do
        chainLen <- pick $ Positive <$> choose (2, 5)
        pubs     <- pick $ genPublicationChain chainLen author
        loopPoint <- pick $ elements (init pubs)
        let badPubs = pubs & _tailNE . _last . ptHeaderL .~ ptHeader loopPoint
        let badTws = map (signPubTx author) badPubs
        lift $ do
            mapM_ submitPub (init badTws)
            -- 'addTxToMempool' kicks duplicated transactions, so we have to
            -- dump them into block
            void . applyBlock =<< createBlock 0
            throwsSome $ submitPub (last badTws)

    it "Wrong signature is not fine" $ witnessProperty $ do
        pub :| [] <- pick $ genPublicationChain 1 author
        let saneTw = signPubTx author pub
        otherTw   <- pick arbitrary
        mixTw     <- pick $ arbitraryUniqueMixture saneTw otherTw
        lift $ throwsSome $ submitPub mixTw
