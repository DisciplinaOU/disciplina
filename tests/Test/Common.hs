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

import qualified Prelude (show, unlines)
import Universum

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
import qualified Disciplina.WorldState as World
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
    { sWorld        :: World.WorldState
    , sTransaction  :: [World.WithProof World.Transaction]
    , alice         :: World.Entity
    , eve           :: World.Entity
    , bob           :: World.Entity
    , initialAmount :: World.Amount
    }

instance Show Sandbox where
    show (Sandbox world transactions a e b lim) =
        concat
          [ "Sandbox { world = "
          , show world
          , ", transactions = \n"
          , Prelude.unlines $ map (^.World.wpBody.to Prelude.show.to ("\t" ++)) transactions
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
                [ World.TransferTokens <$> elements rest <*> pure 1
                , World.Publicate      <$> arbitrary
                ]

            let server = World.Server world

            return $ unsafePerformPureWorldT actor server $ do
                for pairs $ \changes -> do
                    transaction <- World.plan changes
                    World.playTransaction transaction

        --accountCreation :: Integer -> [World.Entity] -> Gen [World.Change]
        --accountCreation 0     _           = return []
        --accountCreation count excludedSet = do
        --    entity <- noneof excludedSet
        --    rest   <- accountCreation (count - 1) (entity : excludedSet)
        --    return (World.CreateAccount entity def : rest)

fairWorld :: World.Amount -> [World.Entity] -> World.WorldState
fairWorld amount actors =
    let
      (world, _) = unsafePerformIO $ do
        (AVL.runOnEmptyCache :: AVL.HashMapStore World.Hash AVL.NullStore World.WorldState -> IO (World.WorldState, AVL.Storage World.Hash)) $ do
            World.evalWorldT def (World.Server World.emptyWorldState) $ do
                World.giveEach actors amount

    in
        world

unsafePerformPureWorldT :: forall side a . World.Entity -> side -> World.WorldT side (AVL.HashMapStore World.Hash AVL.NullStore) a -> a
unsafePerformPureWorldT who side action =
    let
      (a, _) = unsafePerformIO $ do
        AVL.runOnEmptyCache $ do
            World.evalWorldT who side $ do
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

instance Arbitrary World.Entity where
  arbitrary = World.Entity <$> (noneof [0])

instance Arbitrary World.Publication where
  arbitrary = World.hash <$> (arbitrary :: Gen Int)

worldTProperty
    :: Testable prop
    => side
    -> World.WorldT side (AVL.HashMapStore World.Hash AVL.NullStore) prop
    -> Property
worldTProperty side what = ioProperty $ do
    (prop, _) <- AVL.runOnEmptyCache $ World.evalWorldT def side what
    return prop
