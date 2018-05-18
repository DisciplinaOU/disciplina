{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFoldable #-}

-- | Sized Merkle tree implementation.
module Disciplina.Educator.MTree
       ( SizedMerkleRoot(..)
       , SizedMerkleTree (..)
       , mtRoot
       , mkSizedMerkleTree

       , SizedMerkleNode (..)
       , mkBranch
       , mkLeaf
       ) where

import Universum hiding (foldMap, toList)

import Disciplina.Crypto (Hash, hash)
import Disciplina.Crypto.Hash.Class (AbstractHash (..))

import Codec.Serialise (Serialise(..), serialise)
import Data.Bits (Bits (..))
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteString.Builder (Builder, byteString, lazyByteString)
import Data.Foldable (Foldable (..))
import Prelude (show)

import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS


-- | Data type for root of sized merkle tree.
newtype SizedMerkleRoot a = SizedMerkleRoot
    { smrHash :: Hash LBS.ByteString  -- ^ returns root 'Hash' of Merkle Tree
    } deriving (Show, Eq, Ord, Generic, ByteArrayAccess, Foldable, Typeable)


data SizedMerkleTree a
  = SizedMerkleEmpty
  | SizedMerkleTree !Word32 !(SizedMerkleNode a)
  deriving (Eq, Generic)


instance Foldable SizedMerkleTree where
    foldMap _ SizedMerkleEmpty      = mempty
    foldMap f (SizedMerkleTree _ n) = foldMap f n

    null SizedMerkleEmpty = True
    null _           = False

    length SizedMerkleEmpty      = 0
    length (SizedMerkleTree s _) = fromIntegral s

instance Show a => Show (SizedMerkleTree a) where
  show SizedMerkleEmpty = "Empty Sized Merkle tree "
  show (SizedMerkleTree l n) = "Merkle tree: len = " <> Prelude.show l <> " : " <> Prelude.show n
  --show tree = "Merkle tree: "  <> Prelude.show (toList tree)

data SizedMerkleNode a
    = SMerkleBranch { mRoot  :: !(SizedMerkleRoot a)
                    , mLeft  :: !(SizedMerkleNode a)
                    , mSize  :: !Word8
                    , mRight :: !(SizedMerkleNode a)}
    | SMerkleLeaf { mRoot  :: !(SizedMerkleRoot a)
                  , mSize  :: !Word8
                  , mVal   :: !a}
    deriving (Eq, Show, Generic)

instance Foldable SizedMerkleNode where
  foldMap f x = case x of
    SMerkleLeaf {mVal}            -> f mVal
    SMerkleBranch {mLeft, mRight} -> foldMap f mLeft `mappend` foldMap f mRight

toLazyByteString :: Builder -> LBS.ByteString
toLazyByteString = Builder.toLazyByteStringWith (Builder.safeStrategy 1024 4096) mempty

mkLeaf :: Serialise a => a -> SizedMerkleNode a
mkLeaf a =
    SMerkleLeaf
    { mVal  = a
    , mSize = 1
    , mRoot = SizedMerkleRoot $
              hash $ toLazyByteString $ mconcat
                   [ byteString (one 0)
                   , byteString (one 1)
                   , lazyByteString (serialise a) ]
    }

mkBranch :: SizedMerkleNode a -> SizedMerkleNode a -> SizedMerkleNode a
mkBranch a b =
    SMerkleBranch
    { mLeft  = a
    , mRight = b
    , mSize  = mSize a + mSize b
    , mRoot  = SizedMerkleRoot $
               hash $ toLazyByteString $ mconcat
                   [ byteString (one 1)
                   , byteString (one (mSize a + mSize b))
                   , byteString (one (mSize a))
                   , byteString (one (mSize b))
                   , merkleRootToBuilder (mRoot a)
                   , merkleRootToBuilder (mRoot b) ]
    }
  where
    merkleRootToBuilder :: SizedMerkleRoot a -> Builder
    merkleRootToBuilder (SizedMerkleRoot (AbstractHash d)) = byteString (convert d)

-- | Smart constructor for 'SizedMerkleTree'.
mkSizedMerkleTree :: Serialise a => [a] -> SizedMerkleTree a
mkSizedMerkleTree [] = SizedMerkleEmpty
mkSizedMerkleTree ls = SizedMerkleTree (fromIntegral lsLen) (go lsLen ls)
  where
    lsLen = Universum.length ls
    go _  [x] = mkLeaf x
    go len xs = mkBranch (go i l) (go (len - i) r)
      where
        i = powerOfTwo len
        (l, r) = splitAt i xs

-- | Returns root of merkle tree.
mtRoot :: SizedMerkleTree a -> SizedMerkleRoot a
mtRoot SizedMerkleEmpty      = emptyHash
mtRoot (SizedMerkleTree _ x) = mRoot x


emptyHash :: SizedMerkleRoot a
emptyHash = SizedMerkleRoot (hash mempty)

-- | Return the largest power of two such that it's smaller than X.
--
-- >>> powerOfTwo 64
-- 32
-- >>> powerOfTwo 65
-- 64
powerOfTwo :: (Bits a, Num a) => a -> a
powerOfTwo n
    | n .&. (n - 1) == 0 = n `shiftR` 1
    | otherwise = go n
 where
    {- “x .&. (x - 1)” clears the least significant bit:
           ↓
       01101000     x
       01100111     x - 1
       --------
       01100000     x .&. (x - 1)
       I could've used something like “until (\x -> x*2 > w) (*2) 1”,
       but bit tricks are fun. -}
    go w = if w .&. (w - 1) == 0 then w else go (w .&. (w - 1))
