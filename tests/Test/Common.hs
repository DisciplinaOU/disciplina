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
       , module Universum
       ) where

import Data.List (unlines)
import GHC.Show (Show (show))
import Universum hiding (show, unlines)

import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)

import Crypto.Error (CryptoFailable (..))
import Dscp.Core (Assignment (..), AssignmentType (..), CourseId (..), Grade (..),
                  SignedSubmission (..), Submission (..), SubmissionType (..),
                  SubmissionWitness (..), mkAddr)
import Dscp.Crypto (AbstractPK (..), AbstractSK (..), PublicKey, SecretKey, hash, sign)
import Dscp.Educator (PrivateTx (..))

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as C

-- import Control.Arrow (second)
import Control.Lens (each, to)

-- import Data.Bits                                 (xor)
import Data.Default as T (Default (def))
-- import Data.Function                             (on)
-- import Data.List                                 (sortBy, nubBy)
-- import Data.Monoid                               ((<>))
-- import Data.Ord                                  (comparing)
import Data.Traversable (for)

import System.IO.Unsafe

import qualified Data.Tree.AVL as AVL
import qualified Dscp.Crypto as Crypto
import qualified Dscp.Witness as Witness
--import qualified Debug.Trace           as Debug

import Test.Hspec as T (Expectation, Spec, describe, it, shouldBe, shouldSatisfy, specify)
import Test.QuickCheck as T (Arbitrary (..), Gen, Property, Testable (..), elements, expectFailure,
                             ioProperty, oneof, suchThat, vectorOf, (===), (==>))
import Test.QuickCheck.Instances as T ()
import Test.Tasty as T (TestName, TestTree, defaultMain, testGroup)

-- | Extensional equality combinator.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
f .=. g = \a ->
  let fa = f a
      ga = g a

  in  fa === ga

infixr 5 .=.

data Sandbox = Sandbox
    { sWorld        :: Witness.WorldState
    , sTransaction  :: [Witness.WithProof Witness.Transaction]
    , alice         :: Witness.Entity
    , eve           :: Witness.Entity
    , bob           :: Witness.Entity
    , initialAmount :: Witness.Amount
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
        actors <- vectorUniqueOf 3

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

fairWorld :: Witness.Amount -> [Witness.Entity] -> Witness.WorldState
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

noneof :: (Arbitrary a, Eq a, Show a) => [a] -> Gen a
noneof set' = do
    res <- arbitrary `suchThat` (`notElem` set')
    -- Debug.traceShow (res, "<-/-", set) $
    return res

vectorUniqueOf :: (Arbitrary a, Eq a, Show a) => Int -> Gen [a]
vectorUniqueOf = loop []
  where
    loop acc 0 = return acc
    loop acc n = do
        next <- noneof acc
        loop (next : acc) (n - 1)

instance Arbitrary Witness.Entity where
  arbitrary = Witness.Entity <$> (noneof [0])

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

-- | Create public key from seed
mkPubKey :: Char -> PublicKey
mkPubKey seed = fst (mkKeyPair seed)

-- | Create private key from seed
mkPrivKey :: Char -> SecretKey
mkPrivKey seed = snd (mkKeyPair seed)

-- | Create key pair from seed
mkKeyPair :: Char -> (PublicKey, SecretKey)
mkKeyPair seed =
  let (CryptoPassed x) = Ed25519.secretKey (C.replicate 32 seed)
  in (AbstractPK (Ed25519.toPublic x), AbstractSK x)

-- | Create a private transaction
mkPrivateTx :: CourseId -- ^ course id
            -> Grade -- ^ grade
            -> PublicKey -- ^ public key to derive address from
            -> (PublicKey, SecretKey) -- ^ witness key pair
            -> PrivateTx
mkPrivateTx courseId grade addrKey (witnessPKey, witnessSKey) =
    PrivateTx { _ptxSignedSubmission = mkSignedSubmission
              , _ptxGrade = grade
              , _ptxTime = time
              }
  where
     time :: UTCTime
     time = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2018-03-04"

     mkSignedSubmission :: SignedSubmission
     mkSignedSubmission = SignedSubmission
       { ssSubmission = mkSubmission
       , ssWitness = mkSubmissionWitness
       }

     mkSubmission :: Submission
     mkSubmission = Submission
       { sStudentId = mkAddr addrKey
       , sType = Digital
       , sAssignment = mkAssignment
       }

     mkSubmissionWitness :: SubmissionWitness
     mkSubmissionWitness = SubmissionWitness
       { _swKey = witnessPKey
       , _swSig = sign witnessSKey (hash mkSubmission)
       }

     mkAssignment :: Assignment
     mkAssignment = Assignment
       { aCourseId = courseId
       , aType = Regular
       , aAssignment = ""
       }
