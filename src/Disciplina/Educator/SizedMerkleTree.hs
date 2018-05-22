{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Sized Merkle tree implementation.
module Disciplina.Educator.SizedMerkleTree
       ( SizedMerkleRoot(..)
       , SizedMerkleTree (..)
       , getSizedMerkleRoot
       , mkSizedMerkleTree

       , SizedMerkleNode (..)
       , mkBranch
       , mkLeaf
       ) where

import Universum hiding (foldMap, toList)

import Disciplina.Crypto (Hash, HasHash, hash, unsafeHash)
import Disciplina.Crypto.Hash.Class (HashFunc, AbstractHash (..))

import Codec.Serialise (Serialise(..), serialise)
import Data.Bits (Bits (..))
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteString.Builder (Builder, byteString, lazyByteString)
import Data.Foldable (Foldable (..))
import Prelude (show)

import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS

-- | Data type for root of sized merkle tree.
data SizedMerkleRoot a = SizedMerkleRoot
    { smrHash :: Hash LBS.ByteString  -- ^ returns root 'Hash' of Merkle Tree
    , smrSize :: Word8 -- ^ size of root node,
                       -- size is defined as number of leafs in this subtree
    } deriving (Show, Eq, Ord, Generic, ByteArrayAccess , Serialise, Typeable)


data SizedMerkleTree a
  = SizedMerkleEmpty
  | SizedMerkleTree !(SizedMerkleNode a)
  deriving (Eq, Generic)


instance Foldable SizedMerkleTree where
    foldMap _ SizedMerkleEmpty      = mempty
    foldMap f (SizedMerkleTree n) = foldMap f n

    null SizedMerkleEmpty = True
    null _                = False

    length SizedMerkleEmpty      = 0
    length (SizedMerkleTree n) = fromInteger (toInteger (smrSize (mRoot n)))

instance Show a => Show (SizedMerkleTree a) where
  show SizedMerkleEmpty = "Empty Sized Merkle tree "
  show (SizedMerkleTree n) = "Sized Merkle tree:  " <> Prelude.show n
  --show tree = "Merkle tree: "  <> Prelude.show (toList tree)

type LeafIndex = Int

data SizedMerkleNode a
    = SizedMerkleBranch { mRoot  :: !(SizedMerkleRoot a)
                        , mLeft  :: !(SizedMerkleNode a)
                        , mRight :: !(SizedMerkleNode a)}
    | SizedMerkleLeaf { mRoot  :: !(SizedMerkleRoot a)
                      , mIndex :: !LeafIndex
                      , mVal   :: !a}
    deriving (Eq, Show, Generic)

instance Foldable SizedMerkleNode where
  foldMap f x = case x of
    SizedMerkleLeaf {mVal} -> f mVal
    SizedMerkleBranch {mLeft, mRight} -> foldMap f mLeft `mappend` foldMap f mRight

toLazyByteString :: Builder -> LBS.ByteString
toLazyByteString = Builder.toLazyByteStringWith (Builder.safeStrategy 1024 4096) mempty

mkLeaf :: HasHash a => (LeafIndex, a) -> SizedMerkleNode a
mkLeaf (i, a) =
    SizedMerkleLeaf
    { mVal  = a
    , mIndex = i
    , mRoot = SizedMerkleRoot (unsafeHash a) -- unsafeHash since we need to hash to ByteString
                              1 -- size of leaf node is 1
    }

mkBranch :: SizedMerkleNode a -> SizedMerkleNode a -> SizedMerkleNode a
mkBranch l r =
    SizedMerkleBranch
    { mLeft  = l
    , mRight = r
    , mRoot  = mkBranchRootHash (mRoot l) (mRoot r)
    }

mkBranchRootHash :: SizedMerkleRoot a -- ^ left merkle root
                 -> SizedMerkleRoot a -- ^ right merkle root
                 -> SizedMerkleRoot a
mkBranchRootHash (SizedMerkleRoot (AbstractHash hl) sl)
                 (SizedMerkleRoot (AbstractHash hr) sr)
  = SizedMerkleRoot
     (hash $ toLazyByteString $ mconcat
        [ byteString (one sl)
        , byteString (one sr)
        , byteString (convert hl)
        , byteString (convert hr) ])
     (sl + sr)

-- | Smart constructor for 'SizedMerkleTree'.
mkSizedMerkleTree :: (HasHash a, Foldable t) => t a -> SizedMerkleTree a
mkSizedMerkleTree x = mkSizedMerkleTree' (zip [0,1..] (toList x))

mkSizedMerkleTree' :: HasHash a => [(LeafIndex, a)] -> SizedMerkleTree a
mkSizedMerkleTree' [] = SizedMerkleEmpty
mkSizedMerkleTree' ls = SizedMerkleTree (go lsLen ls)
  where
    lsLen = Universum.length ls
    go :: HasHash a => Int -> [(LeafIndex, a)] -> SizedMerkleNode a
    go _  [x] = mkLeaf x
    go len xs = mkBranch (go i l) (go (len - i) r)
      where
        i = powerOfTwo len
        (l, r) = splitAt i xs

-- | Returns root of merkle tree.
getSizedMerkleRoot :: SizedMerkleTree a -> SizedMerkleRoot a
getSizedMerkleRoot SizedMerkleEmpty    = emptyHash
getSizedMerkleRoot (SizedMerkleTree x) = mRoot x

emptyHash :: SizedMerkleRoot a
emptyHash = SizedMerkleRoot (hash mempty) 0

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

data MerkleProof a =
  MerkleProof { getMerkleProof :: [ProofElem a]  -- ^ list of proof elements
              , getMerkleProofRoot :: Maybe (SizedMerkleRoot a) -- ^ leaf root proof is
                                                                -- constructed for
              }
  deriving (Show, Eq, Ord, Generic, Serialise)

data ProofElem a = ProofElem
  { nodeRoot    :: SizedMerkleRoot a
  , siblingRoot :: SizedMerkleRoot a
  , nodeSide    :: Side
  } deriving (Show, Eq, Ord, Generic, Serialise)

data Side = L | R
  deriving (Show, Eq, Ord, Generic, Serialise)

-- | Construct merkle proof by recusive walking the tree and collecting ProofElem until we hit
-- matching leafRoot
mkMerkleProof :: forall a. SizedMerkleTree a -- ^ merkle tree we want to
                                             -- construct a proof from
                        -> LeafIndex         -- ^ leaf index used for proof
                        -> MerkleProof a
mkMerkleProof SizedMerkleEmpty _ = MerkleProof [] Nothing
mkMerkleProof (SizedMerkleTree rootNode) n =
  let (path, proofLeaf) = constructPath [] rootNode
  in MerkleProof path proofLeaf
  where
    constructPath :: [ProofElem a] -> SizedMerkleNode a -> ([ProofElem a], Maybe (SizedMerkleRoot a))
    constructPath pElems (SizedMerkleLeaf leafRoot' leafIndex _)
      | n == leafIndex = (pElems, Just leafRoot')
      | otherwise = ([], Nothing)
    constructPath pElems (SizedMerkleBranch bRoot ln rn) =
         case (lPath, rPath) of
              ((xs, Nothing), (ys, Nothing)) -> (xs ++ ys, Nothing)
              ((xs, Just l), (ys, _))        -> (xs ++ ys, Just l)
              ((xs, _), (ys, Just r))        -> (xs ++ ys, Just r)
      where
        lProofElem = ProofElem (mRoot ln) (mRoot rn) L
        rProofElem = ProofElem (mRoot rn) (mRoot ln) R

        lPath = constructPath (lProofElem:pElems) ln
        rPath = constructPath (rProofElem:pElems) rn

-- | Validate a merkle tree proof
validateMerkleProof :: forall a. MerkleProof a -> SizedMerkleTree a -> Bool
validateMerkleProof (MerkleProof proofElems (Just leafRoot)) (SizedMerkleTree treeNode) =
    validate proofElems leafRoot
  where
    validate :: [ProofElem a] -> SizedMerkleRoot a -> Bool
    validate [] proofRoot = proofRoot == mRoot treeNode
    validate (pElem:pElems) proofRoot
      | proofRoot /= nodeRoot pElem = False
      | otherwise = validate pElems $ hashProofElem pElem

    hashProofElem :: ProofElem a -> SizedMerkleRoot a
    hashProofElem (ProofElem pRoot sibRoot side) =
      case side of
        L -> mkBranchRootHash pRoot sibRoot
        R -> mkBranchRootHash sibRoot pRoot

validateMerkleProof (MerkleProof _ Nothing) _ = False


-- | TODO, create tests and put in test module
testTree :: SizedMerkleTree ByteString
testTree = mkSizedMerkleTree ["a","b","c","d","e"]

proof :: MerkleProof ByteString
proof = mkMerkleProof testTree 0

valid :: Bool
valid = validateMerkleProof proof testTree

--
--valid2 = validateMerkleProof proof testTree (mRoot (mkLeaf "a"))
--valid3 = validateMerkleProof proof testTree (mRoot (mkLeaf "b"))
--valid4 = validateMerkleProof proof testTree (mRoot (mkLeaf "c"))
--valid5 = validateMerkleProof proof testTree (mRoot (mkLeaf "d"))
--valid6 = validateMerkleProof proof testTree (mRoot (mkLeaf "e"))
--
--valid7 = validateMerkleProof proof2 testTree (mRoot (mkLeaf "h"))
--valid8 = validateMerkleProof proof2 testTree (mRoot (mkLeaf "e"))
--
--valid9 = validateMerkleProof proof3 testTree (mRoot (mkLeaf "e"))
