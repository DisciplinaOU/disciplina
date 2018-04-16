{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

-- | Abstract hash interface

module Disciplina.Crypto.Hash.Class
       ( HashFunction (..)
       ) where

import Data.ByteArray (ByteArrayAccess)

class HashFunction hf where
    type AbstractHash hf a :: *

    unsafeAbstractHash :: forall a b. ByteArrayAccess a => a -> AbstractHash hf b

    abstractHash :: forall a. ByteArrayAccess a => a -> AbstractHash hf a
    abstractHash = unsafeAbstractHash @hf @a @a
