{-# LANGUAGE TypeApplications #-}

-- | Specific hash function implementations used in Disciplina

module Disciplina.Crypto.Hash.Impl
       ( Hash
       , hash
       , unsafeHash
       ) where

import Data.ByteArray (ByteArrayAccess)

import Disciplina.Crypto.Hash.Class (HashFunction (..))
import Disciplina.Crypto.Hash.Hashable (HashableFunc (..))

type Hash a = AbstractHash HashableFunc a

hash :: forall a. ByteArrayAccess a => a -> Hash a
hash = abstractHash @HashableFunc @a

unsafeHash :: forall a b. ByteArrayAccess a => a -> Hash b
unsafeHash = unsafeAbstractHash @HashableFunc @a @b
