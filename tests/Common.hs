
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Common (module Common, module Control.Lens, module T) where

import Universum

import Control.Lens hiding (locus, elements, Empty)

-- import Data.Bits                                 (xor)
-- import Data.Default                              (Default(def))
-- import Data.Foldable                             ()
-- import Data.Function                             (on)
-- import Data.List                                 (sortBy, nubBy)
-- import Data.Monoid                               ((<>))
-- import Data.Ord                                  (comparing)

import Test.Framework                       as T (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 as T (testProperty)
import Test.QuickCheck                      as T ( Arbitrary (..), Gen, Property
                                                 , (===), (==>), elements )
import Test.QuickCheck.Instances            as T ()

-- | Extensional equality combinator.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
f .=. g = \a ->
  let fa = f a
      ga = g a

  in  fa === ga

infixr 5 .=.

