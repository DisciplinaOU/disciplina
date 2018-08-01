{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Testing utilities.

module Dscp.Util.Test
       ( module Dscp.Util.Test
       , module T
       ) where

import Prelude hiding (show)

import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Exception.Safe (catchJust)
import Control.Lens (Prism')
import Crypto.Random (ChaChaDRG, MonadPseudoRandom)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.Hashable as H
import Data.Typeable (typeRep)
import GHC.Show (Show (show))
import Test.Hspec as T
import Test.QuickCheck as T (Arbitrary (..), Fixed (..), Gen, Property, Testable (..), choose,
                             elements, expectFailure, forAll, frequency, infiniteList, ioProperty,
                             label, listOf, listOf1, oneof, property, sublistOf, suchThat,
                             suchThatMap, vectorOf, (.&&.), (===), (==>))
import Test.QuickCheck (sized)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Instances as T ()
import Test.QuickCheck.Property as T (rejected)
import Test.QuickCheck.Random (QCGen, mkQCGen)

import Dscp.Crypto.Impl (PublicKey, SecretKey, keyGen, withIntSeed)

-- | Extensional equality combinator.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
f .=. g = \a ->
  let fa = f a
      ga = g a

  in  fa === ga

infixr 5 .=.

-- | Run 'Crypto.Random.MonadRandom' within 'Gen'.
-- Perhaps later we should refuse using with function for the sake of
-- pregenerated lists of items.
genSecureRandom :: MonadPseudoRandom ChaChaDRG a -> Gen a
genSecureRandom rand = arbitrary <&> \seed -> withIntSeed seed rand

data GenCtx = GenCtx QCGen Int

-- | Allows to use given generator when all you have is opportunity to produce
-- 'Arbitrary' values.
-- Use it when really necesssary, produced values won't be displayed on faliure.
delayedGen :: Gen a -> GenCtx -> a
delayedGen gen (GenCtx seed size) = unGen gen seed size

instance Arbitrary GenCtx where
    arbitrary = MkGen GenCtx

instance Show GenCtx where
    show _ = "<some generated values>"

-- | Run 'Gen' with seed.
detGen :: Int -> Gen a -> a
detGen seed gen = unGen gen (mkQCGen seed) 100

-- | Generalized 'detGen'.
detGenG :: Hashable seed => seed -> Gen a -> a
detGenG seed = detGen (H.hash seed)

noneof :: (Arbitrary a, Eq a, Show a) => [a] -> Gen a -> Gen a
noneof set' gen = do
    res <- gen `suchThat` (`notElem` set')
    -- Debug.traceShow (res, "<-/-", set) $
    return res

vectorUniqueOf :: (Arbitrary a, Eq a, Show a) => Int -> Gen a -> Gen [a]
vectorUniqueOf n gen = loop [] n
  where
    loop acc 0 = return acc
    loop acc k = do
        next <- noneof acc gen
        loop (next : acc) (k - 1)

vectorUnique :: (Arbitrary a, Eq a, Show a) => Int -> Gen [a]
vectorUnique n = vectorUniqueOf n arbitrary

listUnique :: (Arbitrary a, Eq a, Show a) => Gen [a]
listUnique = sized $ \n -> do
    k <- choose (0, n)
    vectorUniqueOf k arbitrary

-- | Create public key from seed
mkPubKey :: Char -> PublicKey
mkPubKey seed = fst (mkKeyPair seed)

-- | Create private key from seed
mkPrivKey :: Char -> SecretKey
mkPrivKey seed = snd (mkKeyPair seed)

-- | Create key pair from seed
mkKeyPair :: Char -> (PublicKey, SecretKey)
mkKeyPair seed = swap $ withIntSeed (fromIntegral $ ord seed) keyGen

data AssertionFailed = AssertionFailed String
    deriving (Show, Typeable)

instance Exception AssertionFailed

assertThat :: MonadThrow m => Bool -> String -> m ()
assertThat True _ = return ()
assertThat _    e = throwM (AssertionFailed e)

throws :: forall e m . (MonadCatch m, Exception e) => m () -> m Bool
throws action = do
    (action >> return False) `catch` \(_ :: e) ->
        return True

throwsPrism
    :: forall e m a b.
      (MonadCatch m, Exception e)
    => Prism' e a -> m b -> m Bool
throwsPrism excL action = do
    catchJust (^? excL) (action $> False) (\_ -> return True)

----------------------------------------------------------------------------
-- Roundtrips
----------------------------------------------------------------------------




serialiseRoundtrip
    :: forall a. (Arbitrary a, Serialise a, Eq a, Show a)
    => Property
serialiseRoundtrip = property $ \(s :: a) ->
    deserialise (serialise s) === s

serialiseRoundtripProp
    :: forall a. (Arbitrary a, Serialise a, Eq a, Show a, Typeable a)
    => Spec
serialiseRoundtripProp =
    it (show $ typeRep $ Proxy @a) $ serialiseRoundtrip @a

aesonRoundtrip
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a)
    => Property
aesonRoundtrip = property $ \(s :: a) -> do
    eitherDecode (encode s) === Right s

aesonRoundtripProp
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a, Typeable a)
    => Spec
aesonRoundtripProp =
    it (show $ typeRep $ Proxy @a) $ aesonRoundtrip @a
