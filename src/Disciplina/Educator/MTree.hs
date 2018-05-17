{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFoldable #-}

-- | Sized Merkle tree implementation.
module Disciplina.Educator.MTree
       ( MerkleRoot(..)
       , MerkleTree (..)
       , mtRoot
       , mkMerkleTree

       , MerkleNode (..)
       , mkBranch
       , mkLeaf
       ) where

import           Universum hiding (foldMap, toList)

import           Data.Bits (Bits (..))
import           Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Builder (Builder, byteString, lazyByteString)
import qualified Data.ByteString.Builder.Extra as Builder
import Data.Foldable (Foldable (..))
--import qualified Data.Text.Buildable as Buildable
--import Prelude (show, Foldable(..))
import Prelude (show)
import Disciplina.Crypto.Hash.Class (AbstractHash (..), HasAbstractHash (..), HashFunc (..))

import Codec.Serialise (Serialise (..), serialise)

import Disciplina.Crypto (Hash, HasHash, hash)


-- | Data type for root of merkle tree.
newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: Hash LBS.ByteString  -- ^ returns root 'Hash' of Merkle Tree
    } deriving (Show, Eq, Ord, Generic, ByteArrayAccess, Foldable, Typeable)


-- | Straightforward merkle tree representation in Haskell.
data MerkleTree a
  = MerkleEmpty
  | MerkleTree Word32 (MerkleNode a)
  deriving (Eq, Generic)


instance Foldable MerkleTree where
    foldMap _ MerkleEmpty      = mempty
    foldMap f (MerkleTree _ n) = foldMap f n

    null MerkleEmpty = True
    null _           = False

    length MerkleEmpty      = 0
    length (MerkleTree s _) = fromIntegral s

instance Show a => Show (MerkleTree a) where
  show MerkleEmpty = "Empty Sized Merkle tree "
  show (MerkleTree l n) = "Merkle tree: len = " <> Prelude.show l <> " : " <> Prelude.show n
  --show tree = "Merkle tree: "  <> Prelude.show (toList tree)

data MerkleNode a
    = MerkleBranch { mRoot  :: MerkleRoot a
                   , mLeft  :: MerkleNode a
                   , mRight :: MerkleNode a}
    | MerkleLeaf { mRoot :: MerkleRoot a
                 , mVal  :: a}
    deriving (Eq, Show, Generic)

instance Foldable MerkleNode where
  foldMap f x = case x of
    MerkleLeaf {mVal}            -> f mVal
    MerkleBranch {mLeft, mRight} -> foldMap f mLeft `mappend` foldMap f mRight


toLazyByteString :: Builder -> LBS.ByteString
toLazyByteString = Builder.toLazyByteStringWith (Builder.safeStrategy 1024 4096) mempty

-- TODO, this is cheating, saying that a ~ ByteString
mkLeaf :: (a ~ ByteString , HasHash a) => a -> MerkleNode a
mkLeaf a =
    MerkleLeaf
    { mVal  = a
    , mRoot = MerkleRoot $
              hash (toLazyByteString (byteString (one 0) <>  lazyByteString (serialise a)))
    }

mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch a b =
    MerkleBranch
    { mLeft  = a
    , mRight = b
    , mRoot  = MerkleRoot $
               hash $ toLazyByteString $ mconcat
                   [ byteString (one 1)
                   , merkleRootToBuilder (mRoot a)
                   , merkleRootToBuilder (mRoot b) ]

    }
  where
    merkleRootToBuilder :: MerkleRoot a -> Builder
    merkleRootToBuilder (MerkleRoot (AbstractHash d)) = byteString (convert d)

-- | Smart constructor for 'SizedMerkleTree'.
--mkMerkleTree :: HasHash a => [a] -> MerkleTree a
mkMerkleTree [] = MerkleEmpty
mkMerkleTree ls = MerkleTree (fromIntegral lsLen) (go lsLen ls)
  where
    lsLen = Universum.length ls
    go _  [x] = mkLeaf x
    go len xs = mkBranch (go i l) (go (len - i) r)
      where
        i = powerOfTwo len
        (l, r) = splitAt i xs

-- | Returns root of merkle tree.
mtRoot :: MerkleTree a -> MerkleRoot a
mtRoot MerkleEmpty      = emptyHash
mtRoot (MerkleTree _ x) = mRoot x


emptyHash :: MerkleRoot a
emptyHash = MerkleRoot (hash mempty)

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
