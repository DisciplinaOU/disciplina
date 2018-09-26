{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RecursiveDo           #-}

-- | Utility functions for binary serialisation

module Dscp.Util.Serialise
       ( serialise'
       , deserialise'
       , deserialiseOrFail'
       , encodeCrcProtected
       , decodeCrcProtected

       , encodeWithRemainder
       , decodeRemainder

       , Mem1 (..)
       , Mem2 (..)
       , example
       ) where

import Codec.CBOR.Term (Term (TNull), decodeTerm, encodeTerm)
import Codec.Serialise (DeserialiseFailure, Serialise (..), deserialise, deserialiseOrFail,
                        serialise)
import qualified Codec.Serialise.Decoding as D
import qualified Codec.Serialise.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default (..))
import Data.Digest.CRC32 (crc32)
import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.Record

---------------------------------------------------------------------------
-- Versions of 'serialise' functions for strict bytestrings
---------------------------------------------------------------------------

serialise' :: Serialise a => a -> ByteString
serialise' = LBS.toStrict . serialise

deserialise' :: Serialise a => ByteString -> a
deserialise' = deserialise . LBS.fromStrict

deserialiseOrFail' :: Serialise a => ByteString -> Either DeserialiseFailure a
deserialiseOrFail' = deserialiseOrFail . LBS.fromStrict

---------------------------------------------------------------------------
-- CRC protection
---------------------------------------------------------------------------

encodeCrcProtected :: Serialise a => a -> E.Encoding
encodeCrcProtected a =
    E.encodeListLen 2 <> E.encodeBytes body <> E.encodeWord32 (crc32 body)
  where
    body = serialise' a

decodeCrcProtected :: Serialise a => D.Decoder s a
decodeCrcProtected = do
    len <- D.decodeListLen
    unless (len == 2) $ fail "CRC protected: unexpected list length"
    bs <- D.decodeBytes
    expectedCrc <- D.decodeWord32
    let actualCrc = crc32 bs
    when (actualCrc /= expectedCrc) . fail $
        "decodeCrcProtected: invalid CRC, expected " ++ show expectedCrc ++
        ", actual " ++ show actualCrc
    either (fail . show) pure $ deserialiseOrFail' bs

---------------------------------------------------------------------------
-- Generalized Serialise instances
---------------------------------------------------------------------------

{-
This encoding is dictated by needs of forward-compatibility.

As previous, we encode complex objects as lists, but now we are lenient to
the case when list contains more entries than we expect (when it describes
object from the future version). In some sense, encoded list length stands for
the version of the object of given type.

Disadvantage of this approach is that we can only extend a datatype, replacing
or removing fields is not possible without breaking compatibility.

-}

-- | Redundant terms which came from data of higher version, we don't know what
-- to do with them, but we want to keep them to later evaluate
-- forward-compatible hashes and signatures.
--
-- It is isomorphic to 'ByteString', but we keep it as terms because
-- "serialise" library will hardly work with actual ByteStrings.
newtype Remainder = Remainder [Term]
    deriving (Show, Eq, Ord)

instance Default Remainder where
    def = Remainder []

-- | Encode given complex data with remainder. You need only to 'encode'
-- your datatype fields, length of list representing the type is filled for you.
encodeWithRemainder :: Word -> Remainder -> E.Encoding -> E.Encoding
encodeWithRemainder mainTermsN (Remainder terms) mainEncoder =
    E.encodeListLen (mainTermsN + remainderSize) <>
    mainEncoder <>
    mconcat (map encodeTerm terms)
  where
    remainderSize = fromIntegral $ length terms :: Word

-- | Decode given complex data with remainder.
-- You have to provide number of decoded fields and previously decoded
-- length of list which represents the datatype.
decodeRemainder :: Word -> Int -> D.Decoder s Remainder
decodeRemainder (fromIntegral -> mainTermsN) totalN = mdo
    unless (totalN >= mainTermsN) $
        fail $ "Length of list representing object (which?) is too small, \
               \expected at least" <> show mainTermsN <> ", but got " <>
               show totalN

    remainder <- replicateM (totalN - mainTermsN) decodeTerm
    return $ Remainder $ force remainder

instance NFData Term where
    rnf = \case
        TNull -> ()
        _     -> ()

---------------------------------------------------------------------------
-- How serialisation would look like
---------------------------------------------------------------------------

data Ololo = Ololo Int Double Remainder

instance Serialise Ololo where
    encode (Ololo i d rema) =
        encodeWithRemainder 2 rema $
            encode i <>
            encode d
    decode = do
        len <- D.decodeListLen
        i <- decode
        d <- decode
        rema <- decodeRemainder 2 len
        return $ Ololo i d rema

-- For more cute serialisation instances and demostration of achieved
-- forward-compatibility see below

---------------------------------------------------------------------------
-- Removing boilerplate
---------------------------------------------------------------------------

{- If the following looks scaring, jump to examples below first -}

-- | Used to do @map encode@ on elements of 'HList'.
data HEncode = HEncode
instance (e ~ E.Encoding, Serialise a) => ApplyAB HEncode a e where
    applyAB _ x = encode x

-- | Encode given 'HList'.
encodeH :: _ => Remainder -> HList l -> E.Encoding
encodeH rema l =
    encodeWithRemainder contentPartLen rema $
        mconcat $ hList2List (hMap HEncode l)
  where
    contentPartLen = hNat2Integral (hLength l)

-- | Used to decode 'HList'.
class HDecodeLenient l where
    -- | Decodes given number of arguments into given elements.
    -- Number of arguments should be equal or larger than expected list size,
    -- extra entries would be throw away.
    hDecode :: Int -> D.Decoder s (HList l, Remainder)

instance HDecodeLenient '[] where
    hDecode n = do
        rema <- replicateM n decodeTerm
        return (HNil, Remainder $ force rema)

instance (Serialise x, HDecodeLenient xs) => HDecodeLenient (x : xs) where
    hDecode n = do
        x <- decode
        (xs, rema) <- hDecode (n - 1)
        return (x .*. xs, rema)

-- | Decode elements to 'HList'. Extra terms will be droped.
decodeHLenient
    :: forall l n s.
       (HLengthEq l n, _)
    => D.Decoder s (HList l, Remainder)
decodeHLenient = do
    let expectedLen = hNat2Integral (Proxy @n)
    len <- D.decodeListLen
    when (len < expectedLen) $
        fail "Too few entries"
    hDecode len

-- | Conversion between tuples and 'HLists', for convenience.
class TupleHList t l | t -> l, l -> t where
    fromHList :: l -> t

instance TupleHList (a, b) (HList '[a, b]) where
    fromHList (a `HCons` b `HCons` HNil) = (a, b)

instance TupleHList (a, b, c) (HList '[a, b, c]) where
    fromHList (a `HCons` b `HCons` c `HCons` HNil) = (a, b, c)

---------------------------------------------------------------------------
-- Example
---------------------------------------------------------------------------

-- | First version of some type, we take place for future extensions.
data Mem1 = Mem1 Int Double Remainder
    deriving (Show, Eq, Generic)

instance Serialise Mem1 where
    encode (Mem1 i d rema) = encodeH rema $ hBuild i d
    decode = do
        ((i, d), rema) <- first fromHList <$> decodeHLenient
        return $ Mem1 i d rema

-- | Second (extended) version of the type.
data Mem2 = Mem2 Int Double Text Remainder
    deriving (Show, Eq)

instance Serialise Mem2 where
    encode (Mem2 i d t rema) = encodeH rema $ hBuild i d t
    decode = do
        ((i, d, t), rema) <- first fromHList <$> decodeHLenient
        return $ Mem2 i d t rema

example :: IO ()
example = do
    let m1 = Mem1 1 2 def
    -- This is obviously valid
    print $ deserialise (serialise m1) == m1
    -- >>> True

    -- Later we extended that datatype (it has another name for unambiguity)
    let m2 = Mem2 1 2 "sobaka" def
    print $ deserialise (serialise m2) == m2
    -- >>> True

    -- And this will still work:
    print $ deserialise (serialise m2) == m1
    -- >>> False, because remainder differs, but deserialisation succeedes

    return ()
