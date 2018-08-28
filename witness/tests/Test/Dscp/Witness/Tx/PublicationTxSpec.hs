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
        pheaders <- vectorOf n arbitrary
        let siblings = map (take 2) $ reverse $ tail $
                       Exts.fromList $ inits pheaders
        return $ Exts.fromList $ siblings <&> \curHeaders ->
            PublicationTx
            { ptAuthor = addr
            , ptPrevBlock = curHeaders ^? ix 1
            , ptBlock = curHeaders ^?! ix 0
            }

deriving instance Num a => Num (Positive a)

-- TODO: remove this once addTxToMempool throws error on validation fail (DSCP-209)
ololo :: Monad m => m Bool -> m ()
ololo action = action >>= bool (error "Lool") pass

signSubmitPubChain
    :: (MempoolCtx ctx m, WithinWriteSDLock)
    => NonEmpty PublicationTxWitnessed -> m ()
signSubmitPubChain txs =
    forM_ txs $ \tx -> ololo $ addTxToMempool (GPublicationTxWitnessed tx)

-- TODO: check publications are unique?

spec :: Spec
spec = describe "Publication tx expansion + validation" $ do
    it "First correct tx is fine" $ witnessProperty_ $ do
        secret    <- pick arbitrary
        pub :| [] <- pick $ genPublicationChain 1 secret
        let tw = signPubTx secret pub
        lift $ signSubmitPubChain (one tw)

    it "Consequent txs are fine" $ witnessProperty_ $ do
        secret   <- pick arbitrary
        chainLen <- pick arbitrary
        pubs     <- pick $ genPublicationChain chainLen secret
        let tws = map (signPubTx secret) pubs
        lift $ signSubmitPubChain tws

    it "Tx with wrong previous hash isn't fine" $ witnessProperty $ do
        secret   <- pick arbitrary
        chainLen <- pick $ Positive <$> choose (1, 4)
        pubs     <- pick $ genPublicationChain chainLen secret
        badPubs  <- pick $ shuffleNE pubs
        pre (pubs /= badPubs)
        let badTws = map (signPubTx secret) badPubs
        lift $ throwsSome $ signSubmitPubChain badTws

    it "Foreign author in the chain is not fine" $ witnessProperty $ do
        secret      <- pick arbitrary
        otherSecret <- pick (arbitrary `suchThat` (/= secret))
        let otherAddr = mkAddr (toPublic otherSecret)
        chainLen    <- pick arbitrary
        pubs        <- pick $ genPublicationChain chainLen secret
        let badPubs = pubs & _tailNE . _last . ptAuthorL .~ otherAddr
        let badTws = map (signPubTx secret) badPubs
        lift $ throwsSome $ signSubmitPubChain badTws

    it "Forking publications chain isn't fine" $ witnessProperty $ do
        secret   <- pick arbitrary
        chainLen <- pick $ Positive <$> choose (1, 4)
        pubs     <- pick $ genPublicationChain chainLen secret

        forkPoint <- pick $ Positive <$> choose (1, getPositive chainLen)
        let forkedPub' = pubs ^?! ix (getPositive $ forkPoint - 1)
        forkedPub <- pick arbitrary <&> \ptBlock -> forkedPub'{ ptBlock }
        let badPubs = pubs <> one forkedPub
        let badTws = map (signPubTx secret) badPubs
        lift $ throwsSome $ signSubmitPubChain badTws

    it "Wrong signature is not fine" $ witnessProperty $ do
        secret    <- pick arbitrary
        pub :| [] <- pick $ genPublicationChain 1 secret
        let saneTw = signPubTx secret pub
        otherTw   <- pick arbitrary
        mixTw     <- pick $ arbitraryMixture saneTw otherTw
                            `suchThat` (/= saneTw)
        lift $ throwsSome $ ololo $ addTxToMempool (GPublicationTxWitnessed mixTw)
