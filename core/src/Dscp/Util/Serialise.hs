{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Utility functions for binary serialisation

module Dscp.Util.Serialise
       ( serialise'
       , deserialise'
       , deserialiseOrFail'
       , encodeCrcProtected
       , decodeCrcProtected

       , Mem1 (..)
       , Mem2 (..)
       , example
       ) where

import Codec.CBOR.Term (decodeTerm)
import Codec.Serialise (DeserialiseFailure, Serialise (..), deserialise, deserialiseOrFail,
                        serialise)
import qualified Codec.Serialise.Decoding as D
import qualified Codec.Serialise.Encoding as E
import qualified Data.ByteString.Lazy as LBS
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
To tolerate all future versions in decoding logic at once without manipulating
with lengths manually, 'HList' is proposed to be used as intermediate structure.

Disadvantage of this approach is that we can only extend a datatype, replacing
or removing fields is not possible without breaking compatibility.

-}

-- | Used to do @map encode@ on elements of 'HList'.
data HEncode = HEncode
instance (e ~ E.Encoding, Serialise a) => ApplyAB HEncode a e where
    applyAB _ x = encode x

-- | Encode given 'HList'.
encodeHList :: _ => HList l -> E.Encoding
encodeHList l =
    let len = hNat2Integral (hLength l)
        content = mconcat $ hList2List $ hMap HEncode l
    in E.encodeListLen len <> content

-- | Used to decode 'HList'.
class HDecodeLenient l where
    -- | Decodes given number of arguments into given elements.
    -- Number of arguments should be equal or larger than expected list size,
    -- extra entries would be throw away.
    hDecode :: Int -> D.Decoder s (HList l)

instance HDecodeLenient '[] where
    hDecode n = HNil <$ replicateM_ n decodeTerm

instance (Serialise x, HDecodeLenient xs) => HDecodeLenient (x : xs) where
    hDecode n = do
        x <- decode
        xs <- hDecode (n - 1)
        return (x .*. xs)

-- | Decode elements to 'HList'. Extra terms will be droped.
decodeHListLenient :: forall l n s. (HLengthEq l n, _) => D.Decoder s (HList l)
decodeHListLenient = do
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

-- | First version of some type.
data Mem1 = Mem1 Int Double
    deriving (Show, Eq)

instance Serialise Mem1 where
    encode (Mem1 i d) = encodeHList $ hBuild i d
    decode = do
        (i, d) <- fromHList <$> decodeHListLenient
        return $ Mem1 i d

-- | Second (extended) version of the type.
data Mem2 = Mem2 Int Double Text
    deriving (Show, Eq)

instance Serialise Mem2 where
    encode (Mem2 i d t) = encodeHList $ hBuild i d t
    decode = do
        (i, d, t) <- fromHList <$> decodeHListLenient
        return $ Mem2 i d t

example :: IO ()
example = do
    let m1 = Mem1 1 2
    -- This is obviously valid
    print $ deserialise (serialise m1) == m1
    -- >>> True

    -- Later we extended that datatype (it has another name for simplicity)
    let m2 = Mem2 1 2 "sobaka"
    print $ deserialise (serialise m2) == m2
    -- >>> True

    -- And this will still work:
    print $ deserialise (serialise m2) == m1
    -- >>> True

    return ()
