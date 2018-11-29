{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}

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
       , computeMerkleRoot
       , validateMerkleProof
       , getMerkleProofRoot
       , lookup
       , validateElementExistAt

       , MerkleNode (..)
       , drawMerkleTree
       , mkBranch
       , mkLeaf

       , EmptyMerkleTree
       , getEmptyMerkleTree
       , fillEmptyMerkleTree

       , EmptyMerkleProof
       , IndexedList
       , unIndexedList
       , mkIndexedList
       , separateProofAndData
       , mergeProofAndData
       ) where

import Codec.Serialise (Serialise (..))
import Data.ByteArray (convert)
import Data.ByteString.Builder (Builder, byteString, word32LE)
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F (Foldable (..))
import qualified Data.Map as Map ((!))
import qualified Data.Set as Set
import Data.Tree as Tree (Tree (Node), drawTree)
import Fmt (build, (+|), (|+))
import qualified Text.Show

import Dscp.Crypto.Hash.Class (AbstractHash (..))
import Dscp.Crypto.Impl (HasHash, Hash, hash, unsafeHash)
import Dscp.Crypto.Serialise (Raw)

-- | Data type for root of sized merkle tree.
data MerkleSignature a = MerkleSignature
    { mrHash :: !(Hash Raw)  -- ^ returns root 'Hash' of Merkle Tree
    , mrSize :: !Word32      -- ^ size of root node,
                             --   size is defined as number of leafs in this subtree
    } deriving (Eq, Ord, Generic, Serialise, Functor, Foldable, Traversable, Typeable)

instance Buildable (MerkleSignature a) where
    build MerkleSignature{..} =
        "MerkleSignature { hash: " +| mrHash |+ "; size: " +| mrSize |+ " }"

instance Show (MerkleSignature a) where
    show = toString . pretty

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

type LeafIndex = Word32

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

mkLeaf :: HasHash a => LeafIndex -> a -> MerkleNode a
mkLeaf i a = MerkleLeaf
    { mVal   = a
    , mIndex = i
    , mRoot  = MerkleSignature (unsafeHash a) -- unsafeHash since we need to hash to ByteString
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
      [ word32LE sl
      , word32LE sr
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
fromList ls = MerkleTree (nodeFromList ls)

nodeFromList :: HasHash a => [a] -> MerkleNode a
nodeFromList lst = tree
  where
    (tree, []) = go (0, uLen - 1) `runState` lst

    uLen = fromIntegral $ length lst

    go (lo, hi)
        | lo == hi  = mkLeaf lo <$> pop
        | otherwise = mkBranch <$> go (lo, mid) <*> go (mid + 1, hi)
      where
        mid = (lo + hi) `div` 2

    pop = state $ \case
        []    -> error "nodeFromList: impossible"
        c : s -> (c, s)

-- | Returns root of merkle tree.
getMerkleRoot :: MerkleTree a -> MerkleSignature a
getMerkleRoot MerkleEmpty    = emptyHash
getMerkleRoot (MerkleTree x) = mRoot x

emptyHash :: MerkleSignature a
emptyHash = MerkleSignature (hash mempty) 0

data MerkleProof a
    = ProofBranch
        { pnSig   :: !(MerkleSignature a)
        , pnLeft  :: !(MerkleProof a)
        , pnRight :: !(MerkleProof a) }
    | ProofLeaf
        { pnSig :: !(MerkleSignature a)
        , pnVal :: !a
        }
    | ProofPruned
        { pnSig :: !(MerkleSignature a) }
    deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance Serialise a => Serialise (MerkleProof a)

-- TODO: provide a useful instance
instance Buildable (MerkleProof a) where
    build _ = "<merkle proof>"

pnSize :: MerkleProof a -> Word32
pnSize = mrSize . pnSig

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
    constructProof MerkleLeaf {..}
      | Set.member mIndex n = ProofLeaf mRoot mVal
      | otherwise = ProofPruned mRoot
    constructProof (MerkleBranch mRoot' mLeft' mRight') =
      case (constructProof mLeft', constructProof mRight') of
        (ProofPruned _, ProofPruned _) -> ProofPruned mRoot'
        (pL, pR)                       -> ProofBranch mRoot' pL pR

lookup :: LeafIndex -> MerkleProof a -> Maybe a
lookup index = \case
    ProofPruned {}        -> Nothing
    ProofLeaf   { pnVal } -> return pnVal

    ProofBranch { pnSig, pnLeft, pnRight } -> do
        let size = mrSize pnSig
        let border
              | odd size  = (size `div` 2) + 1
              | otherwise =  size `div` 2

        if   border > index
        then lookup  index           pnLeft
        else lookup (index - border) pnRight

validateElementExistAt :: Eq a => LeafIndex -> a -> MerkleProof a -> Bool
validateElementExistAt index value proof = lookup index proof == Just value

-- | Validate a merkle tree proof.
validateMerkleProof :: HasHash a => MerkleProof a -> MerkleSignature a -> Bool
validateMerkleProof proof treeRoot =
    computeMerkleRoot proof == Just treeRoot

-- | Recalculate signatures of every node in the proof tree and
-- return root signature if every inner node passed validation.
computeMerkleRoot :: HasHash a => MerkleProof a -> Maybe (MerkleSignature a)
computeMerkleRoot ProofLeaf {..} =
    if MerkleSignature (unsafeHash pnVal) 1 == pnSig
    then Just pnSig
    else Nothing
computeMerkleRoot ProofPruned {..} = Just pnSig
computeMerkleRoot (ProofBranch pnRoot' pnLeft' pnRight') = do
    pnSigL <- computeMerkleRoot pnLeft'
    pnSigR <- computeMerkleRoot pnRight'
    if mkBranchRootHash pnSigL pnSigR == pnRoot'
        then Just pnRoot'
        else Nothing

-- | Debug print of tree.
drawMerkleTree :: (Show a) => MerkleTree a -> String
drawMerkleTree MerkleEmpty = "empty tree"
drawMerkleTree (MerkleTree n) = Tree.drawTree (asTree n)
  where
    asTree :: (Show a) => MerkleNode a -> Tree.Tree String
    asTree MerkleBranch {..} = Tree.Node (show mRoot) [asTree mLeft, asTree mRight]
    asTree leaf              = Tree.Node (show leaf) []

-- | Debug print of proof tree.
drawProofNode :: (Show a) => Maybe (MerkleProof a) -> String
drawProofNode Nothing = "empty proof"
drawProofNode (Just p) = Tree.drawTree (asTree p)
  where
    asTree :: (Show a) => MerkleProof a -> Tree.Tree String
    asTree ProofLeaf {..}   = Tree.Node ("leaf, "   <> show pnSig) []
    asTree ProofBranch {..} = Tree.Node ("branch, " <> show pnSig) [asTree pnLeft, asTree pnRight]
    asTree ProofPruned {..} = Tree.Node ("pruned, " <> show pnSig) []

-- | Merkle tree with values removed. Used for storing Merkle trees
-- in the Educator database.
newtype EmptyMerkleTree a = EmptyTree (MerkleTree ())
    deriving newtype (Eq, Show, Generic, Serialise)

-- | Helper function which arbitrarily changes the type tag for
-- @'MerkleSignature'@.
coerceSig :: MerkleSignature a -> MerkleSignature b
coerceSig = fmap $ error "coerceSig: 'MerkleSignature a' has 'a' inside!"

-- | Empties out the Merkle tree. Leaves only 'Pruned' nodes.
getEmptyMerkleTree :: MerkleTree a -> EmptyMerkleTree a
getEmptyMerkleTree = EmptyTree . (() <$)

fillEmptyMerkleTree :: Map LeafIndex a -> EmptyMerkleTree a -> Maybe (MerkleProof a)
fillEmptyMerkleTree plugs (EmptyTree sieve) =
    let
        keySet = Set.fromList (keys plugs)
        proof  = mkMerkleProof sieve keySet
        filled = fill <$> proof
    in
        filled
  where
    fill it = evalState (aux it) 0
      where
        aux = \case
          ProofBranch sig left right ->
              ProofBranch (coerceSig sig) <$> aux left <*> aux right

          ProofLeaf sig () ->
              ProofLeaf (coerceSig sig) . (plugs Map.!) <$> next

          ProofPruned sig ->
              ProofPruned (coerceSig sig) <$ skip (mrSize sig)

        next   = state $ \i -> (i,  i + 1)
        skip n = state $ \i -> ((), i + n)

-- | Merkle proof with values at leaves removed and replaced via @Pruned@
-- nodes.
newtype EmptyMerkleProof a = EmptyProof (MerkleProof Void)
    deriving newtype (Eq, Show, Generic, Serialise)

-- | Absurd instance to derive 'Serialise' for 'EmptyMerkleProof' automatically.
instance Serialise Void where
    encode = absurd

-- | List of indexed elements. Invariant: indices always go in ascending
-- order.
newtype IndexedList a = IndexedList
    { unIndexedList :: [(LeafIndex, a)]
    } deriving newtype (Eq, Show, Generic)

-- | Safe constructor for an indexed list. Makes sure the invariant of ascending
-- order is held.
mkIndexedList :: [(LeafIndex, a)] -> IndexedList a
mkIndexedList = IndexedList . sortBy (compare `on` fst)

-- | Splits Merkle proof into signatures and data.
separateProofAndData :: MerkleProof a -> (EmptyMerkleProof a, IndexedList a)
separateProofAndData =
    bimap EmptyProof (IndexedList . reverse . snd) .
    flip runState (0, []) . go
  where
    go (ProofBranch s l r) = ProofBranch (coerceSig s) <$>
        go l <*>
        (modify' (first (+ pnSize l)) *> go r)
    go (ProofPruned s)     = pure $ ProofPruned (coerceSig s)
    go (ProofLeaf s a)     = pickLeaf a $> ProofPruned (coerceSig s)

    pickLeaf a = do
        ix <- gets fst
        modify' $ second ((ix, a) :)

-- | Merges empty proof and list of data elements into one Merkle proof.
mergeProofAndData :: forall a. EmptyMerkleProof a -> IndexedList a -> Maybe (MerkleProof a)
mergeProofAndData (EmptyProof proof) (IndexedList leaves) =
    let (fullProof, (_, left)) = runState (go proof) (0, leaves)
    in if not $ null left
       then Nothing
       else Just fullProof
  where
    go (ProofBranch s l r) = ProofBranch (coerceSig s) <$>
        go l <*>
        (modify' (first (+ pnSize l)) *> go r)
    go (ProofLeaf _ a)     = absurd a
    go (ProofPruned s)     = let s' = coerceSig s in
        if mrSize s' > 1
        then pure $ ProofPruned s'
        else maybe (ProofPruned s') (ProofLeaf s') <$> shiftLeaf

    shiftLeaf = do
        (i, lvs) <- get
        traverse (\a -> modify' (second $ drop 1) $> a) $
            ixedHead i lvs
    ixedHead i = safeHead >=> \(j, a) -> guard (i == j) >> return a
