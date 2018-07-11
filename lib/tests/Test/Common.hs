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

module Test.Common
       ( module Test.Common
       , module Control.Lens
       , module T
       , module Dscp.Util
       ) where

import Prelude hiding (show)

import Control.Exception.Safe (catchJust)
import Control.Lens (Prism')
import Crypto.Random (ChaChaDRG, MonadPseudoRandom)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import GHC.Show (Show (show))

import Dscp.Core (Assignment (..), AssignmentType (..), Course, Grade, SignedSubmission (..),
                  Submission (..), SubmissionWitness (..), mkAddr, offlineHash)
import Dscp.Crypto (PublicKey, SecretKey, hash, keyGen, sign, withIntSeed)
import qualified Dscp.Crypto as Crypto
import Dscp.Educator (PrivateTx (..))
import Dscp.Util (HasId (..))

import Test.Hspec as T (Expectation, Spec, describe, it, shouldBe, shouldSatisfy, specify)
import Test.QuickCheck as T (Arbitrary (..), Gen, Property, Testable (..), choose, elements,
                             expectFailure, forAll, infiniteList, ioProperty, label, listOf,
                             listOf1, oneof, property, suchThat, suchThatMap, vectorOf, (.&&.),
                             (===), (==>))
import Test.QuickCheck (sized)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Instances as T ()
import Test.QuickCheck.Property as T (rejected)
import Test.QuickCheck.Random (QCGen, mkQCGen)
import Test.Tasty as T (TestName, TestTree, defaultMain, testGroup)

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
genSecureRandom rand = arbitrary <&> \seed -> Crypto.withIntSeed seed rand

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

{-

data Sandbox = Sandbox
    { sWorld        :: Witness.WorldState
    , sTransaction  :: [Witness.WithProof Witness.Transaction]
    , alice         :: Witness.Entity
    , eve           :: Witness.Entity
    , bob           :: Witness.Entity
    , initialAmount :: Accounts.Amount
    }

instance Show Sandbox where
    show (Sandbox world transactions a e b lim) =
        concat
          [ "Sandbox { world = "
          , show world
          , ", transactions = \n"
          , unlines $ map (^.Witness.wpBody.to show.to ("\t" ++)) transactions
          , ", alice = "
          , show a
          , ", eve = "
          , show e
          , ", bob = "
          , show b
          , ", lim = "
          , show lim
          , " }"
          ]

instance Arbitrary Sandbox where
    arbitrary = do
        actors <- vectorUnique 3

        let [alice', eve', bob'] = actors

        let world = fairWorld 10 actors

        transactions <- generateTransactions world alice' [eve', bob']

        return $ Sandbox world transactions alice' eve' bob' 10

      where
        generateTransactions world actor rest = do
            pairs <- vectorOf 2 $ vectorOf 5 $ oneof
                [ Witness.TransferTokens <$> elements rest <*> pure 1
                , Witness.Publicate      <$> arbitrary
                ]

            let server = Witness.Server world

            return $ unsafePerformPureWorldT actor server $ do
                for pairs $ \changes -> do
                    transaction <- Witness.plan changes
                    Witness.playTransaction transaction

        --accountCreation :: Integer -> [Witness.Entity] -> Gen [Witness.Change]
        --accountCreation 0     _           = return []
        --accountCreation count excludedSet = do
        --    entity <- noneof excludedSet
        --    rest   <- accountCreation (count - 1) (entity : excludedSet)
        --    return (Witness.CreateAccount entity def : rest)

fairWorld :: Accounts.Amount -> [Witness.Entity] -> Witness.WorldState
fairWorld amount actors =
    let
      (world, _) = unsafePerformIO $ do
        (AVL.runOnEmptyCache :: AVL.HashMapStore Witness.Hash' AVL.NullStore Witness.WorldState -> IO (Witness.WorldState, AVL.Storage Witness.Hash')) $ do
            Witness.evalWorldT def (Witness.Server Witness.emptyWorldState) $ do
                Witness.giveEach actors amount

    in
        world

unsafePerformPureWorldT :: forall side a . Witness.Entity -> side -> Witness.WorldT side (AVL.HashMapStore Witness.Hash' AVL.NullStore) a -> a
unsafePerformPureWorldT who side action =
    let
      (a, _) = unsafePerformIO $ do
        AVL.runOnEmptyCache $ do
            Witness.evalWorldT who side $ do
                action
    in
        a

instance Arbitrary Witness.Entity where
  arbitrary = Witness.Entity <$> (noneof [0] arbitrary)

instance Arbitrary Witness.Publication where
  arbitrary = Crypto.unsafeHash <$> (arbitrary :: Gen Int)

worldTProperty
    :: Testable prop
    => side
    -> Witness.WorldT side (AVL.HashMapStore Witness.Hash' AVL.NullStore) prop
    -> Property
worldTProperty side what = ioProperty $ do
    (prop, _) <- AVL.runOnEmptyCache $ Witness.evalWorldT def side what
    return prop

-}

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

-- | Create a private transaction
mkPrivateTx :: Id Course -- ^ course id
            -> Grade -- ^ grade
            -> PublicKey -- ^ public key to derive address from
            -> (PublicKey, SecretKey) -- ^ witness key pair
            -> PrivateTx
mkPrivateTx courseId grade addrKey (witnessPKey, witnessSKey) =
    PrivateTx { _ptSignedSubmission = mkSignedSubmission
              , _ptGrade = grade
              , _ptTime = time
              }
  where
     time :: UTCTime
     time = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2018-03-04"

     mkSignedSubmission :: SignedSubmission
     mkSignedSubmission = SignedSubmission
       { _ssSubmission = mkSubmission
       , _ssWitness = mkSubmissionWitness
       }

     mkSubmission :: Submission
     mkSubmission = Submission
       { _sStudentId = mkAddr addrKey
       , _sContentsHash = offlineHash
       , _sAssignment = mkAssignment
       }

     mkSubmissionWitness :: SubmissionWitness
     mkSubmissionWitness = SubmissionWitness
       { _swKey = witnessPKey
       , _swSig = sign witnessSKey (hash mkSubmission)
       }

     mkAssignment :: Assignment
     mkAssignment = Assignment
       { _aCourseId = courseId
       , _aContentsHash = offlineHash
       , _aType = Regular
       , _aDesc = ""
       }

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
