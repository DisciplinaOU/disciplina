{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}

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
    } deriving (Show, Eq, Ord, Generic, ByteArrayAccess
               ,Foldable, Serialise, Typeable)


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
    length (SizedMerkleTree l _) = fromIntegral l

instance Show a => Show (SizedMerkleTree a) where
  show SizedMerkleEmpty = "Empty Sized Merkle tree "
  show (SizedMerkleTree l n) = "Sized Merkle tree: len = " <> Prelude.show l <> " : " <> Prelude.show n
  --show tree = "Merkle tree: "  <> Prelude.show (toList tree)

data SizedMerkleNode a
    = SizedMerkleBranch { mRoot  :: !(SizedMerkleRoot a)
                        , mSize  :: !Word8
                        , mLeft  :: !(SizedMerkleNode a)
                        , mRight :: !(SizedMerkleNode a)}
    | SizedMerkleLeaf { mRoot  :: !(SizedMerkleRoot a)
                      , mSize  :: !Word8
                      , mVal   :: !a}
    deriving (Eq, Show, Generic)

instance Foldable SizedMerkleNode where
  foldMap f x = case x of
    SizedMerkleLeaf {mVal} -> f mVal
    SizedMerkleBranch {mLeft, mRight} -> foldMap f mLeft `mappend` foldMap f mRight

toLazyByteString :: Builder -> LBS.ByteString
toLazyByteString = Builder.toLazyByteStringWith (Builder.safeStrategy 1024 4096) mempty

mkLeaf :: Serialise a => a -> SizedMerkleNode a
mkLeaf a =
    SizedMerkleLeaf
    { mVal  = a
    , mSize = 1
    , mRoot = SizedMerkleRoot $
              hash $ toLazyByteString $ mconcat
                   [ byteString (one 0)
                   , byteString (one 1)
                   , lazyByteString (serialise a) ]
    }

mkBranch :: SizedMerkleNode a -> SizedMerkleNode a -> SizedMerkleNode a
mkBranch l r =
    SizedMerkleBranch
    { mLeft  = l
    , mRight = r
    , mSize  = mSize l + mSize r
    , mRoot  = mkBranchRootHash (mSize l) (mSize r) (mRoot l) (mRoot r)
    }

mkBranchRootHash :: Word8 -- | left branch size
                 -> Word8 -- | right branch size
                 -> SizedMerkleRoot a -- | left merkel root
                 -> SizedMerkleRoot a -- | right merkel root
                 -> SizedMerkleRoot a
mkBranchRootHash sl sr l r =
    SizedMerkleRoot $ hash $ toLazyByteString $ mconcat
        [ byteString (one 1)
        , byteString (one (sl + sr))
        , byteString (one sl)
        , byteString (one sr)
        , merkleRootToBuilder l
        , merkleRootToBuilder r ]
  where
    merkleRootToBuilder :: SizedMerkleRoot a -> Builder
    merkleRootToBuilder (SizedMerkleRoot (AbstractHash d)) = byteString (convert d)

mkSizedMerkleTree :: (Serialise a, Foldable t) => t a -> SizedMerkleTree a
mkSizedMerkleTree x = mkSizedMerkleTree' (toList x)

-- | Smart constructor for 'SizedMerkleTree'.
mkSizedMerkleTree' :: Serialise a => [a] -> SizedMerkleTree a
mkSizedMerkleTree' [] = SizedMerkleEmpty
mkSizedMerkleTree' ls = SizedMerkleTree (fromIntegral lsLen) (go lsLen ls)
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

newtype MerkleProof a = MerkleProof { getMerkleProof :: [ProofElem a] }
  deriving (Show, Eq, Ord, Generic, Serialise)

data ProofElem a = ProofElem
  { nodeRoot    :: SizedMerkleRoot a
  , nodeSize    :: Word8
  , siblingRoot :: SizedMerkleRoot a
  , siblingSize :: Word8
  , nodeSide    :: Side
  } deriving (Show, Eq, Ord, Generic, Serialise)

data Side = L | R
  deriving (Show, Eq, Ord, Generic, Serialise)

-- | Construct merkle proof by recusive walking the tree and collecting ProofElem
merkleProof :: forall a. SizedMerkleTree a -> SizedMerkleRoot a -> MerkleProof a
merkleProof SizedMerkleEmpty _ = MerkleProof []
merkleProof (SizedMerkleTree _ rootNode) leafRoot = MerkleProof $ constructPath [] rootNode
  where
    constructPath :: [ProofElem a] -> SizedMerkleNode a -> [ProofElem a]
    constructPath pElems (SizedMerkleLeaf leafRoot' _ _)
      | leafRoot == leafRoot' = pElems
      | otherwise             = []
    constructPath pElems (SizedMerkleBranch bRoot _ ln rn) = lPath ++ rPath
      where
        lProofElem = ProofElem (mRoot ln) (mSize ln) (mRoot rn) (mSize rn) L
        rProofElem = ProofElem (mRoot rn) (mSize rn) (mRoot ln) (mSize ln) R

        lPath = constructPath (lProofElem:pElems) ln
        rPath = constructPath (rProofElem:pElems) rn

-- | Validate a merkle tree proof
validateMerkleProof :: forall a. MerkleProof a -> SizedMerkleTree a -> SizedMerkleRoot a -> Bool
validateMerkleProof (MerkleProof proofElems) (SizedMerkleTree _ treeNode) leafRoot =
    validate proofElems leafRoot
  where
    validate :: [ProofElem a] -> SizedMerkleRoot a -> Bool
    validate [] proofRoot = proofRoot == mRoot treeNode
    validate (pElem:pElems) proofRoot
      | proofRoot /= nodeRoot pElem = False
      | otherwise = validate pElems $ hashProofElem pElem

    hashProofElem :: ProofElem a -> SizedMerkleRoot a
    hashProofElem (ProofElem pRoot pSize sibRoot sibSize side) =
      case side of
        L -> mkBranchRootHash pSize sibSize pRoot sibRoot
        R -> mkBranchRootHash sibSize pSize sibRoot pRoot

-- | tests
testTree :: SizedMerkleTree ByteString
testTree = mkSizedMerkleTree ["a","b","c","d","e"]

proof :: MerkleProof ByteString
proof = merkleProof testTree (mRoot (mkLeaf "d"))
proof2 = merkleProof testTree (mRoot (mkLeaf "e"))

valid :: Bool
valid = validateMerkleProof proof testTree (mRoot (mkLeaf "d"))

valid2 = validateMerkleProof proof testTree (mRoot (mkLeaf "a"))
valid3 = validateMerkleProof proof testTree (mRoot (mkLeaf "b"))
valid4 = validateMerkleProof proof testTree (mRoot (mkLeaf "c"))
valid5 = validateMerkleProof proof testTree (mRoot (mkLeaf "d"))
valid6 = validateMerkleProof proof testTree (mRoot (mkLeaf "e"))

valid7 = validateMerkleProof proof2 testTree (mRoot (mkLeaf "h"))
valid8 = validateMerkleProof proof2 testTree (mRoot (mkLeaf "e"))
