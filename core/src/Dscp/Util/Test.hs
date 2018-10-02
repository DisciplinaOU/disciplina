{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports             #-}
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

import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Exception.Safe (catchJust)
import Control.Lens (LensLike')
import "cryptonite" Crypto.Random (ChaChaDRG, MonadPseudoRandom)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.Hashable as H
import qualified Data.Text.Buildable
import Data.Typeable (typeRep)
import Fmt ((+|), (+||), (|+), (||+))
import qualified GHC.Exts as Exts
import qualified GHC.Generics as G
import qualified Loot.Log as Log
import System.Random (Random)
import Test.Hspec as T
import Test.QuickCheck as T (Arbitrary (..), Fixed (..), Gen, Property, Testable (..), conjoin,
                             cover, elements, expectFailure, forAll, frequency, infiniteList,
                             infiniteListOf, ioProperty, label, listOf, listOf1, oneof, property,
                             sublistOf, suchThat, suchThatMap, vectorOf, (.&&.), (===), (==>))
import Test.QuickCheck (shuffle, sized)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Arbitrary.Generic as T (genericArbitrary, genericShrink)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Instances as T ()
import Test.QuickCheck.Monadic as T (PropertyM, monadic, pick, stop)
import Test.QuickCheck.Property as T (failed, rejected, succeeded)
import Test.QuickCheck.Property (reason)
import Test.QuickCheck.Random (QCGen, mkQCGen)
import qualified Text.Show

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

choose :: (Ord a, Show a, Random a) => (a, a) -> Gen a
choose range@(l, r)
    | l > r = error $ "choose: invalid range " <> show range
    | otherwise = Q.choose (l, r)

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

listUniqueOf :: (Arbitrary a, Eq a, Show a) => Gen a -> Gen [a]
listUniqueOf gen = sized $ \n -> do
    k <- choose (0, n)
    vectorUniqueOf k gen

listUnique :: (Arbitrary a, Eq a, Show a) => Gen [a]
listUnique = listUniqueOf arbitrary

takeSome :: [a] -> Gen (NonEmpty a)
takeSome l = sized $ \n -> do
    k <- choose (1, n)
    return (take k l) `suchThatMap` nonEmpty

shuffleNE :: NonEmpty a -> Gen (NonEmpty a)
shuffleNE = fmap Exts.fromList . shuffle . toList

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

throwsSome :: forall m . (MonadCatch m) => m () -> m Bool
throwsSome = throws @SomeException

-- | Sad that we need this function, but QuickCheck does not report test input
-- data if exception was thrown.
noThrow :: MonadCatch m => m () -> m Property
noThrow action = property <$> do
    (action $> succeeded)
        `catchAny` \e -> pure failed{ reason = "Exception thrown: " <> show e }

-- | Checks whether exception is thrown and matches given prism.
-- Very specific lens is consumed to allow mappend of several lenses.
throwsPrism
    :: forall e m a b.
      (MonadCatch m, Exception e)
    => LensLike' (Const (First a)) e a -> m b -> m Property
throwsPrism excL action =
    catchJust (^? excL) (action $> property False) (\_ -> pure (property True))
        `catchAny`
        \e -> pure $ property failed{ reason = "Exception thrown: " <> show e }

expectOne :: Text -> [a] -> a
expectOne _    [x] = x
expectOne desc xs  =
    error $ "expectOne: " <> pretty (length xs) <> " entities (" <> desc <> ")"

counterexample :: Testable prop => Text -> prop -> Property
counterexample desc prop = Q.counterexample (toString desc) prop

-- | Generate smaller amounts of data.
pickSmall :: (Monad m, Show a) => Gen a -> PropertyM m a
pickSmall = pick . Q.resize 5

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

-- | When warning or error are logged, this exception is thrown.
data TestLoggedError = TestLoggedError
    { tleLvl :: Log.Level
    , tleMsg :: Text
    }

instance Exception TestLoggedError

instance Show TestLoggedError where
    show = toString . pretty

instance Buildable TestLoggedError where
    build TestLoggedError{..} =
        "Bad situation was logged with level " +|| tleLvl ||+ ": " +| tleMsg |+ ""

-- | Logging suitable for all tests.
-- We ensure that no warnings are printed, becase generally seeing warnings in
-- production is a bad sign.
testLogging :: Log.Logging IO
testLogging =
    Log.Logging
    { Log._log = \lvl _ msg ->
        when (lvl >= Log.Warning) $
            throwM $ TestLoggedError lvl msg
    , Log._logName = return $ error "Loger name requested in test"
    }

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

----------------------------------------------------------------------------
-- Generics fun
----------------------------------------------------------------------------

-- | Combines two objects, randomly borrowing a field from one of them for each
-- field of the constructed object.
-- Default implementation automatically combines product types.
class ArbitraryMixture a where
    arbitraryMixture :: a -> a -> Gen a
    default arbitraryMixture
        :: (Generic a, GArbitraryMixture (G.Rep a))
        => a -> a -> Gen a
    arbitraryMixture x y = G.to <$> gArbitraryMixture (G.from x) (G.from y)

-- | Implementation of 'ArbitraryMixture' for "primitive" types.
primitiveArbitraryMixture :: a -> a -> Gen a
primitiveArbitraryMixture a b = elements [a, b]

-- | Similar to 'arbitraryMixture', makes sure that mixture differs from the
-- original entities.
arbitraryUniqueMixture :: (ArbitraryMixture a, Eq a) => a -> a -> Gen a
arbitraryUniqueMixture a b =
    arbitraryMixture a b `suchThat` \r -> r /= a && r /= b

instance ArbitraryMixture a => ArbitraryMixture [a] where
    arbitraryMixture l1 l2 = forM (zip l1 l2) $ uncurry arbitraryMixture

instance ArbitraryMixture a => ArbitraryMixture (Maybe a) where
    arbitraryMixture (Just a) (Just b) = Just <$> arbitraryMixture a b
    arbitraryMixture _        _        = pure Nothing

instance ArbitraryMixture Integer where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture Word32 where
    arbitraryMixture = primitiveArbitraryMixture

-- | Helps to automatically derive 'ArbitraryMixture' for product types.
class GArbitraryMixture (rep :: * -> *) where
    gArbitraryMixture :: rep p -> rep p -> Gen (rep p)

instance ArbitraryMixture inner => GArbitraryMixture (G.Rec0 inner) where
    gArbitraryMixture (G.K1 a) (G.K1 b) = G.K1 <$> arbitraryMixture a b

instance GArbitraryMixture G.U1 where
    gArbitraryMixture G.U1 G.U1 = pure G.U1

instance GArbitraryMixture inner => GArbitraryMixture (G.M1 ty meta inner) where
    gArbitraryMixture (G.M1 a) (G.M1 b) = G.M1 <$> gArbitraryMixture a b

instance (GArbitraryMixture l, GArbitraryMixture r) =>
         GArbitraryMixture (l G.:*: r) where
    gArbitraryMixture (l1 G.:*: l2) (r1 G.:*: r2) =
        (G.:*:) <$> gArbitraryMixture l1 r1 <*> gArbitraryMixture l2 r2
