-- | Instances for binary serialisation for 'Dscp.Crypto' datatypes
--
-- We provide them for every possible framework, but expect serialise
-- instances to come from 'FromByteArray'.

module Dscp.Crypto.Serialise
       ( Raw
       , encodeBA
       , decodeBA
       ) where

import Universum

import Codec.Serialise (Serialise (..), serialise)
import Codec.Serialise.Decoding (Decoder, decodeBytes, decodeWordOf)
import Codec.Serialise.Encoding (Encoding, encodeBytes, encodeWord)
import Data.ByteArray (ByteArrayAccess, convert)

import Dscp.Crypto.ByteArray
import Dscp.Crypto.Hash
import Dscp.Crypto.Impl
import Dscp.Crypto.MerkleTree
import Dscp.Crypto.Signing

---------------------------------------------------------------
-- Hashes
---------------------------------------------------------------

instance FromByteArray (AbstractHash hf a) => Serialise (AbstractHash hf a) where
    encode = encodeBA
    decode = decodeBA

-- | If 'LByteString' can be hashed, then every 'Serialise' instance can.
instance {-# OVERLAPPABLE #-}
    (Serialise a, HashFunc hf, HasAbstractHash hf LByteString) =>
        HasAbstractHash hf a where
    unsafeAbstractHash = unsafeAbstractHash . serialise

----------------------------------------------------------------
-- Signatures
----------------------------------------------------------------

instance FromByteArray (AbstractSK ss) => Serialise (AbstractSK ss) where
    encode = encodeBA
    decode = decodeBA

instance FromByteArray (AbstractPK ss) => Serialise (AbstractPK ss) where
    encode = encodeBA
    decode = decodeBA

instance FromByteArray (AbstractSig ss a) => Serialise (AbstractSig ss a) where
    encode = encodeBA
    decode = decodeBA

instance Serialise msg => Serialise (Signed msg)

-- If 'LByteString' can be signed/verified via signature scheme `ss`,
-- then every 'Serialise' instance can.
--
-- This is a "base" instance, it's supposed to be overlapped with
-- standalone instances like "HasAbstractSignature ss ByteString"
-- etc.
instance {-# OVERLAPPABLE #-}
    (Serialise a, SignatureScheme ss, HasAbstractSignature ss LByteString) =>
        HasAbstractSignature ss a where
    unsafeAbstractSign sk = unsafeAbstractSign sk . serialise
    unsafeAbstractVerify pk = unsafeAbstractVerify pk . serialise

----------------------------------------------------------------------------
-- Merkle trees
----------------------------------------------------------------------------

{-
Custom CBOR tag values are picked from on of `Unassigned` ranges
not yet claimed in the large `First Come First Served` range:
https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml#tags
-}

elStubTag :: Word
elStubTag = 1042

mSigTag :: Word
mSigTag = 1043

instance Serialise ElementStub where
    encode _ = encodeWord elStubTag
    decode = ElementStub <$ decodeWordOf elStubTag

instance Serialise (MerkleSignature a) where
    encode MerkleSignature {..} =
        encodeWord mSigTag <> encode msSize <> encode msHash
    decode =
        MerkleSignature <$ decodeWordOf mSigTag <*> decode <*> decode

instance Serialise a => Serialise (MerkleNode a)
instance Serialise a => Serialise (MerkleTree a)
instance Serialise a => Serialise (MerkleProof a)

deriving instance Serialise (EmptyMerkleTree a)
deriving instance Serialise (EmptyMerkleProof a)

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

encodeBA :: ByteArrayAccess x => x -> Encoding
encodeBA = encodeBytes . convert

decodeBA :: FromByteArray x => Decoder s x
decodeBA = either fail pure . fromByteArray =<< decodeBytes
