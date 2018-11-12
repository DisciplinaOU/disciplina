{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Test.Dscp.Witness.Tx.PublicationTxSpec where

import Control.Lens (forOf, _last)
import qualified GHC.Exts as Exts
import Test.QuickCheck.Monadic (pre)

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Snowdrop
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness
import Test.Dscp.Witness.Mode

genPublicationChain
    :: HasWitnessConfig
    => Word -> SecretKeyData -> Gen (NonEmpty PublicationTx)
genPublicationChain n secret
    | n <= 0 = error "genPublicationChain: n == 0"
    | otherwise = do
        let addr = mkAddr (skPublic secret)
        sigs <- vectorUniqueOf (fromIntegral n) arbitrary
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
                , ptFeesAmount = unFees $ calcFeePub (fcPublication feeConfig) ptHeader
                , ptHeader
                }

selectAuthor :: Gen SecretKeyData
selectAuthor = fmap mkSecretKeyData $ elements (testGenesisSecrets <> testCommitteeSecrets)

submitPub
    :: (MempoolCtx ctx m, WithinWriteSDLock)
    => PublicationTxWitnessed -> m ()
submitPub = addTxToMempool . GPublicationTxWitnessed

spec :: Spec
spec = describe "Publication tx expansion + validation" $ do
    it "First correct tx in chain is fine" $ witnessProperty $ do
        author <- pick selectAuthor
        pub :| [] <- pick $ genPublicationChain 1 author
        let tw = signPubTx author pub
        lift . noThrow $ submitPub tw

    it "Consequent txs are fine" $ witnessProperty $ do
        author <- pick selectAuthor
        chainLen <- pick $ choose (1, 5)
        pubs     <- pick $ genPublicationChain chainLen author
        let tws = map (signPubTx author) pubs
        lift . noThrow $ mapM_ submitPub tws

    it "Tx with wrong previous hash isn't fine" $ witnessProperty $ do
        author <- pick selectAuthor
        chainLen <- pick $ choose (1, 4)
        pubs     <- pick $ genPublicationChain chainLen author
        badPubs  <- pick $ shuffleNE pubs
        pre (pubs /= badPubs)
        let badTws = map (signPubTx author) badPubs
        lift $ throwsSome $ mapM_ submitPub badTws

    it "Foreign author in the chain is not fine" $ witnessProperty $ do
        author <- pick selectAuthor
        otherSecret <- pick (arbitrary `suchThat` (/= author))
        let otherAddr = mkAddr (skPublic otherSecret)
        chainLen    <- pick $ choose (2, 5)
        pubs        <- pick $ genPublicationChain chainLen author
        let badPubs = pubs & _tailNE . _last . ptAuthorL .~ otherAddr
        let badTws = map (signPubTx author) badPubs
        lift $ do
            mapM_ submitPub (init badTws)
            throwsSome $ submitPub (last badTws)

    it "Not enough fees is not fine" $ witnessProperty $ do
        author <- pick selectAuthor
        issuingWitness <- lift $ ourSecretKeyData @WitnessNode
        pre (author /= issuingWitness)  -- no fees are fine otherwise
        pub :| [] <- pick $ genPublicationChain 1 author
        badPub <- forOf (ptFeesAmountL . _Coin) pub $ \fee -> do
            when (fee == 0) $ error "Fees were not expected to be absent"
            subtracted <- pick $ choose (1, fee)
            return (fee - subtracted)
        let tw = signPubTx author badPub
        lift . throwsPrism (_PublicationError . _PublicationFeeIsTooLow) $
            submitPub tw

    it "Forking publications chain isn't fine" $ witnessProperty $ do
        author <- pick selectAuthor
        chainLen <- pick $ choose (2, 4)
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
        author <- pick selectAuthor
        chainLen <- pick $ choose (2, 5)
        pubs     <- pick $ genPublicationChain chainLen author
        loopPoint <- pick $ elements (init pubs)

        let badPubs = pubs & _tailNE . _last . ptHeaderL .~ ptHeader loopPoint
        let badTws  = map (signPubTx author) badPubs

        lift $ do
            mapM_ submitPub (init badTws)
            throwsPrism (_PublicationError . _PublicationLocalLoop) $
                submitPub (last badTws)

    it "Duplicated tx causes failure" $ witnessProperty $ do
        -- this is mostly a special case of the previous test,
        -- but for clarity let's do this check
        author <- pick selectAuthor
        pub :| [] <- pick $ genPublicationChain 1 author
        let tw = signPubTx author pub

        lift $ submitPub tw
        whenM (pick arbitrary) $
            lift $ void . applyBlock =<< createBlock runSdM 0
        lift . throwsPrism (_PublicationError . _PublicationLocalLoop) $
            submitPub tw

    it "Wrong signature is not fine" $ witnessProperty $ do
        author <- pick selectAuthor
        pub :| [] <- pick $ genPublicationChain 1 author
        let saneTw = signPubTx author pub
        otherTw   <- pick arbitrary
        mixTw     <- pick $ arbitraryUniqueMixture saneTw otherTw
        lift $ throwsSome $ submitPub mixTw
