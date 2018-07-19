{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns    #-}

-- | Sized Merkle tree implementation.
module Dscp.Crypto.MerkleTree
       ( MerkleSignature(..)
       , MerkleTree (..)
       , getMerkleRoot
       , fromFoldable
       , fromContainer
       , fromList

       , MerkleProof (..)
       , drawProofNode
       , mkMerkleProof
       , mkMerkleProofSingle
       , validateMerkleProof
       , getMerkleProofRoot

       , MerkleNode (..)
       , drawMerkleTree
       , mkBranch
       , mkLeaf

       , EmptyMerkleTree
       , getEmptyMerkleTree
       , fillEmptyMerkleTree
       ) where

import Dscp.Crypto.Hash.Class (AbstractHash (..))
import Dscp.Crypto.Impl (HasHash, Hash, hash, unsafeHash)
import Dscp.Crypto.Serialise (Raw)

import Codec.Serialise (Serialise (..))
import Data.Array (array, (!))
import Data.Bits (Bits (..))
import Data.ByteArray (convert)
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.Map as Map ((!))
import qualified Data.Set as Set
import Data.Tree as Tree (Tree (Node), drawTree)

import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F (Foldable (..))

-- | Data type for root of sized merkle tree.
data MerkleSignature a = MerkleSignature
    { mrHash :: !(Hash Raw)  -- ^ returns root 'Hash' of Merkle Tree
    , mrSize :: !Word8 -- ^ size of root node,
                       -- size is defined as number of leafs in this subtree
    } deriving (Show, Eq, Ord, Generic, Serialise, Functor, Foldable, Traversable, Typeable)

data MerkleTree a
    = MerkleEmpty
    | MerkleTree !(MerkleNode a)
    deriving (Eq, Show, Functor, Generic, Serialise)


instance Foldable MerkleTree where
    foldMap _ MerkleEmpty    = mempty
    foldMap f (MerkleTree n) = F.foldMap f n

    null MerkleEmpty = True
    null _           = False

    length MerkleEmpty    = 0
    length (MerkleTree n) = fromIntegral (mrSize (mRoot n))

deriving instance Container (MerkleTree a)

type LeafIndex = Int

data MerkleNode a
    = MerkleBranch
       { mRoot  :: !(MerkleSignature a)
       , mLeft  :: !(MerkleNode a)
       , mRight :: !(MerkleNode a) }
    | MerkleLeaf
       { mRoot  :: !(MerkleSignature a)
       , mIndex :: !LeafIndex
       , mVal   :: !a }
    deriving (Eq, Show, Functor, Generic, Serialise)

instance Foldable MerkleNode where
    foldMap f x = case x of
      MerkleLeaf {mVal}            -> f mVal
      MerkleBranch {mLeft, mRight} -> F.foldMap f mLeft `mappend` F.foldMap f mRight

mkLeaf :: HasHash a => (LeafIndex, a) -> MerkleNode a
mkLeaf (i, a) = MerkleLeaf
    { mVal  = a
    , mIndex = i
    , mRoot = MerkleSignature (unsafeHash a) -- unsafeHash since we need to hash to ByteString
                              1 -- size of leaf node is 1
    }

mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch l r = MerkleBranch
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
fromFoldable = fromList . F.toList

-- | Smart constructor for MerkleTree.
fromContainer :: (HasHash (Element t), Container t) => t -> MerkleTree (Element t)
fromContainer = fromList . toList

fromList :: HasHash a => [a] -> MerkleTree a
fromList [] = MerkleEmpty
fromList ls = MerkleTree (go' 1 (length lsIndexed))
  where
    lsIndexed = zip [1,2..] ls
    arr = array (1, length lsIndexed) lsIndexed
    go' lo hi = if lo == hi
                then mkLeaf (lo - 1, arr ! lo)
                else mkBranch (go' loL hiL) (go' loR hiR)
      where
        -- new borders
        (!loL, !hiL, !loR, !hiR) = (lo, lo + i, lo + i + 1, hi)
        i = powerOfTwo (hi - lo)

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

data MerkleProof a
    = ProofBranch
        { pnSig   :: !(MerkleSignature a)
        , pnLeft  :: !(MerkleProof a)
        , pnRight :: !(MerkleProof a) }
    | ProofLeaf
        { pnSig   :: !(MerkleSignature a)
        , pnIndex :: !LeafIndex
        , pnVal   :: !a
        }
    | ProofPruned
        { pnSig :: !(MerkleSignature a) }
    deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

getMerkleProofRoot :: MerkleProof a -> MerkleSignature a
getMerkleProofRoot = pnSig

mkMerkleProofSingle :: forall a. MerkleTree a -- ^ merkle tree we want to construct a proof from
                              -> LeafIndex -- ^ leaf index used for proof
                              -> Maybe (MerkleProof a)
mkMerkleProofSingle t n = mkMerkleProof t (Set.fromList [n])

mkMerkleProof :: forall a. MerkleTree a -- ^ merkle tree we want to construct a proof from
                        -> Set LeafIndex -- ^ leaf index used for proof
                        -> Maybe (MerkleProof a)
mkMerkleProof MerkleEmpty _ = Nothing
mkMerkleProof (MerkleTree rootNode) n =
    case constructProof rootNode of
      ProofPruned _ -> Nothing
      x             -> Just x
  where
    constructProof :: MerkleNode a -> MerkleProof a
    constructProof (MerkleLeaf {..})
      | Set.member mIndex n = ProofLeaf mRoot mIndex mVal
      | otherwise = ProofPruned mRoot
    constructProof (MerkleBranch mRoot' mLeft' mRight') =
      case (constructProof mLeft', constructProof mRight') of
        (ProofPruned _, ProofPruned _) -> ProofPruned mRoot'
        (pL, pR)                       -> ProofBranch mRoot' pL pR

-- | Validate a merkle tree proof.
validateMerkleProof :: forall a. HasHash a => MerkleProof a -> MerkleSignature a -> Bool
validateMerkleProof proof treeRoot =
    computeMerkleRoot proof == Just treeRoot
  where
    computeMerkleRoot :: MerkleProof a -> Maybe (MerkleSignature a)
    computeMerkleRoot (ProofLeaf {..}) = do
      case MerkleSignature (unsafeHash pnVal) 1 == pnSig of
        True  -> Just pnSig
        False -> Nothing
    computeMerkleRoot (ProofPruned {..}) = Just pnSig
    computeMerkleRoot (ProofBranch pnRoot' pnLeft' pnRight') = do
      pnSigL <- computeMerkleRoot pnLeft'
      pnSigR <- computeMerkleRoot pnRight'
      case mkBranchRootHash pnSigL pnSigR == pnRoot' of
        True  -> Just pnRoot'
        False -> Nothing

-- | Debug print of tree.
drawMerkleTree :: (Show a) => MerkleTree a -> String
drawMerkleTree MerkleEmpty = "empty tree"
drawMerkleTree (MerkleTree n) = Tree.drawTree (asTree n)
  where
    asTree :: (Show a) => MerkleNode a -> Tree.Tree String
    asTree (MerkleBranch {..}) = Tree.Node (show mRoot) [asTree mLeft, asTree mRight]
    asTree leaf                = Tree.Node (show leaf) []

-- | Debug print of proof tree.
drawProofNode :: (Show a) => Maybe (MerkleProof a) -> String
drawProofNode Nothing = "empty proof"
drawProofNode (Just p) = Tree.drawTree (asTree p)
  where
    asTree :: (Show a) => MerkleProof a -> Tree.Tree String
    asTree (ProofLeaf {..}) = Tree.Node ("leaf, " <> show pnSig) []
    asTree (ProofBranch {..}) = Tree.Node ("branch, " <> show pnSig) [asTree pnLeft, asTree pnRight]
    asTree (ProofPruned {..}) = Tree.Node ("pruned, " <> show pnSig) []

-- | Not a `newtype`, because DeriveAnyClass and GeneralizedNewtypeDeriving
--   are in conflict here.
data EmptyMerkleTree a = Empty (MerkleTree ())
    deriving (Eq, Show, Generic, Serialise)

-- | Replaces all values in the tree with '()'.
getEmptyMerkleTree :: MerkleTree a -> EmptyMerkleTree a
getEmptyMerkleTree = Empty . (() <$)

fillEmptyMerkleTree :: Map LeafIndex a -> EmptyMerkleTree a -> Maybe (MerkleProof a)
fillEmptyMerkleTree plugs (Empty sieve) =
    let
        keySet = Set.fromList (keys plugs)
        proof  = mkMerkleProof sieve keySet
        filled = fill <$> proof
    in
        filled
  where
    fill = \case
        ProofBranch sig left right -> ProofBranch (coerseSig sig) (fill left) (fill right)
        ProofLeaf   sig idx  ()    -> ProofLeaf   (coerseSig sig) idx (plugs Map.! idx)
        ProofPruned sig            -> ProofPruned (coerseSig sig)

    coerseSig sig = error "coerseSig: 'MerkleSignature a' has 'a' inside!" <$> sig
