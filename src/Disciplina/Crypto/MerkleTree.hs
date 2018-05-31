{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Sized Merkle tree implementation.
module Disciplina.Crypto.MerkleTree
       ( MerkleSignature(..)
       , MerkleTree (..)
       , getMerkleRoot
       , fromFoldable
       , fromContainer

       , MerkleProof (..)
       , mkMerkleProof
       , validateMerkleProof

       , MerkleNode (..)
       , drawMerkleTree
       , mkBranch
       , mkLeaf
       ) where

import Universum

import Disciplina.Crypto.Serialise ()
import Disciplina.Crypto.Impl (Hash, HasHash, hash, unsafeHash)
import Disciplina.Crypto.Hash.Class (AbstractHash (..))

import Codec.Serialise (Serialise(..))
import Data.Bits (Bits (..))
import Data.ByteArray (convert)
import Data.ByteString.Builder (Builder, byteString)
import Data.Tree as Tree (Tree(Node), drawTree)

import qualified Data.Foldable as F (Foldable (..))
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS

-- | Data type for root of sized merkle tree.
data MerkleSignature a = MerkleSignature
    { smrHash :: Hash LBS.ByteString  -- ^ returns root 'Hash' of Merkle Tree
    , smrSize :: Word8 -- ^ size of root node,
                       -- size is defined as number of leafs in this subtree
    } deriving (Show, Eq, Ord, Generic, Serialise, Functor, Typeable)


data MerkleTree a
  = MerkleEmpty
  | MerkleTree !(MerkleNode a)
  deriving (Eq, Show, Generic)


instance Foldable MerkleTree where
    foldMap _ MerkleEmpty    = mempty
    foldMap f (MerkleTree n) = F.foldMap f n

    null MerkleEmpty      = True
    null _                = False

    length MerkleEmpty    = 0
    length (MerkleTree n) = fromIntegral (smrSize (mRoot n))

deriving instance Container (MerkleTree a)

type LeafIndex = Int

data MerkleNode a
    = MerkleBranch { mRoot  :: !(MerkleSignature a)
                   , mLeft  :: !(MerkleNode a)
                   , mRight :: !(MerkleNode a)}
    | MerkleLeaf { mRoot  :: !(MerkleSignature a)
                 , mIndex :: !LeafIndex
                 , mVal   :: !a}
    deriving (Eq, Show, Functor, Generic)

instance Foldable MerkleNode where
  foldMap f x = case x of
    MerkleLeaf {mVal} -> f mVal
    MerkleBranch {mLeft, mRight} -> F.foldMap f mLeft `mappend` F.foldMap f mRight

mkLeaf :: HasHash a => (LeafIndex, a) -> MerkleNode a
mkLeaf (i, a) = MerkleLeaf
    { mVal  = a
    , mIndex = i
    , mRoot = MerkleSignature (unsafeHash a) -- unsafeHash since we need to hash to ByteString
                              1 -- size of leaf node is 1
    }

mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch l r =
    MerkleBranch
    { mLeft  = l
    , mRight = r
    , mRoot  = mkBranchRootHash (mRoot l) (mRoot r)
    }

mkBranchRootHash :: MerkleSignature a -- ^ left merkle root
                 -> MerkleSignature a -- ^ right merkle root
                 -> MerkleSignature a
mkBranchRootHash (MerkleSignature (AbstractHash hl) sl)
                 (MerkleSignature (AbstractHash hr) sr)
  = MerkleSignature
     (hash $ toLazyByteString $ mconcat
        [ byteString (one sl)
        , byteString (one sr)
        , byteString (convert hl)
        , byteString (convert hr) ])
     (sl + sr)
  where
    toLazyByteString :: Builder -> LBS.ByteString
    toLazyByteString = Builder.toLazyByteStringWith (Builder.safeStrategy 1024 4096) mempty

-- | Smart constructor for MerkleTree.
fromFoldable :: (HasHash a, Foldable t) => t a -> MerkleTree a
fromFoldable = fromList . zip [0,1..] . F.toList

-- | Smart constructor for MerkleTree.
fromContainer :: (HasHash (Element t), Container t) => t -> MerkleTree (Element t)
fromContainer = fromFoldable . toList

fromList :: HasHash a => [(LeafIndex, a)] -> MerkleTree a
fromList [] = MerkleEmpty
fromList ls = MerkleTree (go lsLen ls)
  where
    lsLen = Universum.length ls
    go :: HasHash a => Int -> [(LeafIndex, a)] -> MerkleNode a
    go _  [x] = mkLeaf x
    go len xs = mkBranch (go i l) (go (len - i) r)
      where
        i = powerOfTwo len
        (l, r) = splitAt i xs

-- | Returns root of merkle tree.
getMerkleRoot :: MerkleTree a -> MerkleSignature a
getMerkleRoot MerkleEmpty    = emptyHash
getMerkleRoot (MerkleTree x) = mRoot x

emptyHash :: MerkleSignature a
emptyHash = MerkleSignature (hash mempty) 0

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
     -}
    go w = if w .&. (w - 1) == 0 then w else go (w .&. (w - 1))

data MerkleProof a = MerkleProof
  { getMerkleProof     :: [ProofElem a]  -- ^ list of proof elements
  , getMerkleProofRoot :: Maybe (MerkleSignature a) -- ^ leaf root proof is constructed for
  }
  deriving (Show, Eq, Ord, Generic, Serialise)

data ProofElem a = ProofElem
  { nodeRoot    :: MerkleSignature a
  , siblingRoot :: MerkleSignature a
  , nodeSide    :: Side
  } deriving (Show, Eq, Ord, Generic, Serialise)

data Side = L | R
  deriving (Show, Eq, Ord, Generic, Serialise)

-- | Construct merkle proof by recusive walking the tree and collecting ProofElem until we hit
-- matching leafRoot.
mkMerkleProof :: forall a. MerkleTree a -- ^ merkle tree we want to construct a proof from
                        -> LeafIndex    -- ^ leaf index used for proof
                        -> MerkleProof a
mkMerkleProof MerkleEmpty _ = MerkleProof [] Nothing
mkMerkleProof (MerkleTree rootNode) n =
  let (path, proofLeaf) = constructPath [] rootNode
  in MerkleProof path proofLeaf
  where
    constructPath :: [ProofElem a] -> MerkleNode a -> ([ProofElem a], Maybe (MerkleSignature a))
    constructPath pElems (MerkleLeaf leafRoot' leafIndex _)
      | n == leafIndex = (pElems, Just leafRoot')
      | otherwise = ([], Nothing)
    constructPath pElems (MerkleBranch _ ln rn) =
         case (lPath, rPath) of
              ((xs, Nothing), (ys, Nothing)) -> (xs ++ ys, Nothing)
              ((xs, Just l), (ys, _))        -> (xs ++ ys, Just l)
              ((xs, _), (ys, Just r))        -> (xs ++ ys, Just r)
      where
        lProofElem = ProofElem (mRoot ln) (mRoot rn) L
        rProofElem = ProofElem (mRoot rn) (mRoot ln) R

        lPath = constructPath (lProofElem:pElems) ln
        rPath = constructPath (rProofElem:pElems) rn

-- | Validate a merkle tree proof.
validateMerkleProof :: forall a. MerkleProof a -> MerkleTree a -> Bool
validateMerkleProof (MerkleProof proofElems (Just leafRoot)) (MerkleTree treeNode) =
    validate proofElems leafRoot
  where
    validate :: [ProofElem a] -> MerkleSignature a -> Bool
    validate [] proofRoot = proofRoot == mRoot treeNode
    validate (pElem:pElems) proofRoot
      | proofRoot /= nodeRoot pElem = False
      | otherwise = validate pElems $ hashProofElem pElem

    hashProofElem :: ProofElem a -> MerkleSignature a
    hashProofElem (ProofElem pRoot sibRoot side) =
      case side of
        L -> mkBranchRootHash pRoot sibRoot
        R -> mkBranchRootHash sibRoot pRoot

validateMerkleProof (MerkleProof _ Nothing) _ = False

-- | Debug print of tree.
drawMerkleTree :: (Show a, IsString a) => MerkleTree a -> String
drawMerkleTree MerkleEmpty = "empty tree"
drawMerkleTree (MerkleTree n) = Tree.drawTree (asTree n)
  where
   asTree :: (Show a, IsString a) => MerkleNode a -> Tree.Tree String
   asTree (MerkleBranch {..}) = Tree.Node (show mRoot) [asTree mLeft, asTree mRight]
   asTree leaf = Tree.Node (show leaf) []
