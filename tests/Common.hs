
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Common (module Common, module Control.Lens, module T) where

import qualified Prelude (show)
import Universum

-- import Control.Arrow (second)
import Control.Lens (to, each)

-- import Data.Bits                                 (xor)
import Data.Default                         as T (Default(def))
import Data.Foldable                        as T (for_)
-- import Data.Function                             (on)
-- import Data.List                                 (sortBy, nubBy)
-- import Data.Monoid                               ((<>))
-- import Data.Ord                                  (comparing)

import System.IO.Unsafe

import qualified Data.Tree.AVL         as AVL
import qualified Disciplina.WorldState as World
import qualified Debug.Trace           as Debug

import Test.Framework                       as T (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 as T (testProperty)
import Test.QuickCheck                      as T ( Arbitrary (..), Gen, Property
                                                 , (===), (==>), elements
                                                 , vectorOf, oneof, suchThat
                                                 , Testable
                                                 , ioProperty
                                                 )
import Test.QuickCheck.Instances            as T ()

-- | Extensional equality combinator.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
f .=. g = \a ->
  let fa = f a
      ga = g a

  in  fa === ga

infixr 5 .=.

data Sandbox = Sandbox
    { sWorld       :: World.WorldState
    , sTransaction :: [World.WithProof World.Transaction]
    }

instance Show Sandbox where
    show (Sandbox world transactions) =
        concat
          [ "Sandbox { world = "
          , show world
          , ", transactions = "
          , map (^.World.wpBody) transactions^.to Prelude.show
          , " }"
          ]

instance Arbitrary Sandbox where
    arbitrary = do
        actors @ [alice, eve, bob] <- vectorUniqueOf 3

        let world  = actors `World.giveEach` 10 -- bucks

        transactions <- generateTransactions world alice [eve, bob]

        return $ Sandbox world transactions

      where
        generateTransactions world actor rest =
            vectorOf 2 $ do
                changes <- vectorOf 5 $ oneof
                    [ World.TransferTokens <$> elements rest <*> pure 1
                    , World.Publicate      <$> arbitrary
                    ]

                accountCreations <- accountCreation 5 (actor : rest)

                let server = World.Server world

                return $ unsafePerformIO $ do
                    World.evalWorldM actor server $ do
                        transaction <- World.plan (changes <> accountCreations)
                        World.connectTransaction transaction

        accountCreation 0     _           = return []
        accountCreation count excludedSet = do
            entity <- noneof excludedSet
            rest   <- accountCreation (count - 1) (entity : excludedSet)
            return (World.CreateAccount entity def : rest)

        noneof :: (Arbitrary a, Eq a, Show a) => [a] -> Gen a
        noneof set = do
            res <- arbitrary `suchThat` (`notElem` set)
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
  arbitrary = World.Entity <$> arbitrary

instance Arbitrary World.Publication where
  arbitrary = World.hash <$> (arbitrary :: Gen Int)

worldMProperty
    :: Testable prop
    => side
    -> World.WorldM side prop
    -> Property
worldMProperty side what = ioProperty $ World.evalWorldM def side what
