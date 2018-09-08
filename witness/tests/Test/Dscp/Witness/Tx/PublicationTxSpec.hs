{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Test.Dscp.Witness.Tx.PublicationTxSpec where

import Control.Lens (ix, (^?!), _last)
import qualified GHC.Exts as Exts
import Test.QuickCheck.Modifiers (Positive (..))
import Test.QuickCheck.Monadic (pre)

import Dscp.Core
import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness
import Test.Dscp.Witness.Mode

genPublicationChain :: Positive Int -> SecretKey -> Gen (NonEmpty PublicationTx)
genPublicationChain (Positive n) secret
    | n <= 0 = error "genPublicationChain: n <= 0"
    | otherwise = do
        let addr = mkAddr (toPublic secret)
        pheaders <- vectorUnique n
        let siblings = map (take 2) $ map reverse $ tail $
                       Exts.fromList $ inits pheaders
        return $ Exts.fromList $ siblings <&> \curHeaders ->
            PublicationTx
            { ptAuthor = addr
            , ptPrevBlock = curHeaders ^? ix 1
            , ptBlock = curHeaders ^?! ix 0
            }

author :: SecretKey
author = detGen 21 $ elements testGenesisSecrets

deriving instance Num a => Num (Positive a)

submitPubChain
    :: (MempoolCtx ctx m, WithinWriteSDLock)
    => NonEmpty PublicationTxWitnessed -> m ()
submitPubChain = mapM_ $ \tx -> addTxToMempool (GPublicationTxWitnessed tx)

spec :: Spec
spec = describe "Publication tx expansion + validation" $ do
    it "First correct tx is fine" $ witnessProperty $ do
        pub :| [] <- pick $ genPublicationChain 1 author
        let tw = signPubTx author pub
        lift . noThrow $ submitPubChain (one tw)

    it "Consequent txs are fine" $ witnessProperty $ do
        chainLen <- pick arbitrary
        pubs     <- pick $ genPublicationChain chainLen author
        let tws = map (signPubTx author) pubs
        lift . noThrow $ submitPubChain tws

    it "Tx with wrong previous hash isn't fine" $ witnessProperty $ do
        chainLen <- pick $ Positive <$> choose (1, 4)
        pubs     <- pick $ genPublicationChain chainLen author
        badPubs  <- pick $ shuffleNE pubs
        pre (pubs /= badPubs)
        let badTws = map (signPubTx author) badPubs
        lift $ throwsSome $ submitPubChain badTws

    it "Foreign author in the chain is not fine" $ witnessProperty $ do
        otherSecret <- pick (arbitrary `suchThat` (/= author))
        let otherAddr = mkAddr (toPublic otherSecret)
        chainLen    <- pick $ Positive <$> choose (2, 5)
        pubs        <- pick $ genPublicationChain chainLen author
        let badPubs = pubs & _tailNE . _last . ptAuthorL .~ otherAddr
        let badTws = map (signPubTx author) badPubs
        lift $ throwsSome $ submitPubChain badTws

    it "Forking publications chain isn't fine" $ witnessProperty $ do
        chainLen <- pick $ Positive <$> choose (2, 4)
        pubs     <- pick $ genPublicationChain chainLen author

        forkPub' <- pick $ elements (toList pubs)
        forkPub <- pick arbitrary <&> \ptBlock -> forkPub'{ ptBlock }
        pre (forkPub /= forkPub')
        let badPubs = pubs <> one forkPub
        let badTws = map (signPubTx author) badPubs
        lift $ throwsSome $ submitPubChain badTws

    it "Loops are not fine" $ witnessProperty $ do
        chainLen <- pick $ Positive <$> choose (2, 5)
        pubs     <- pick $ genPublicationChain chainLen author
        loopPoint <- pick $ elements (init pubs)
        let badPubs = pubs & _tailNE . _last . ptBlockL .~ ptBlock loopPoint
        let badTws = map (signPubTx author) badPubs
        lift $ throwsSome $ submitPubChain badTws

    it "Wrong signature is not fine" $ witnessProperty $ do
        pub :| [] <- pick $ genPublicationChain 1 author
        let saneTw = signPubTx author pub
        otherTw   <- pick arbitrary
        mixTw     <- pick $ arbitraryUniqueMixture saneTw otherTw
        lift $ throwsSome $ submitPubChain (one mixTw)
