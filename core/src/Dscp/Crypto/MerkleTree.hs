{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeApplications   #-}

-- | Sized Merkle tree implementation.
module Dscp.Crypto.MerkleTree
       ( MerkleSignature(..)
       , MerkleTree (..)
       , getMerkleRoot
       , fromFoldable
       , fromContainer

       , MerkleProof (..)
       , mpSize
       , merkleProofFromList
       , drawProofNode
       , mkMerkleProof
       , mkMerkleProofSingle
       , validateMerkleProof
       , lookup
       , validateElementExistAt

       , MerkleProofReady
       , mprRoot
       , mprProof
       , mprSize
       , reconstructRoot
       , readyProof
       , mergeProofs

       , MerkleNode (..)
       , drawMerkleTree
       , mkBranch
       , mkLeaf

       , ElementStub (..)
       , EmptyMerkleTree (..)
       , getEmptyMerkleTree
       , fillEmptyMerkleTree

       , EmptyMerkleProof (..)
       , getEmptyMerkleProof
       , mkEmptyMerkleProof
       , separateProofAndData
       , mergeProofAndData

       , merkleFromJson
       , merkleTreeFromJson
       ) where

import Universum
import Data.ByteArray (convert)
import Data.ByteString.Builder (Builder, byteString, word32LE)
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Foldable as F (Foldable (..))
import qualified Data.Binary as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map ((!))
import qualified Data.Set as Set
import Data.Aeson (Value (..))
import Data.Tree as Tree (Tree (Node), drawTree)
import Fmt (Buildable (..), (+|), (|+), pretty)
import qualified GHC.Exts as Exts (IsList (..))
import qualified Text.Show

import Dscp.Crypto.Impl
import Dscp.Util

-- | Data type for root of sized merkle tree.
data MerkleSignature a = MerkleSignature
    { msSize :: !Word32      -- ^ size of root node,
                             --   size is defined as number of leafs in this subtree
    , msHash :: !(Hash Raw)  -- ^ returns root 'Hash' of Merkle Tree
    } deriving (Eq, Ord, Generic, Typeable)

instance Buildable (MerkleSignature a) where
    build MerkleSignature{..} =
        "MerkleSignature { hash: "+|msHash|+"; size: "+|msSize|+" }"

instance Show (MerkleSignature a) where
    show = toString @Text . pretty

-- | Merkle tree over an array of elements of type `a`. Might be empty.
data MerkleTree a
    = MerkleEmpty
    | MerkleTree !(MerkleNode a)
    deriving (Show, Eq, Generic)

instance Foldable MerkleTree where
    foldMap _ MerkleEmpty    = mempty
    foldMap f (MerkleTree n) = F.foldMap f n

    null MerkleEmpty = True
    null _           = False

    length MerkleEmpty    = 0
    length (MerkleTree n) = fromIntegral $ msSize $ mnRoot n

instance Container (MerkleTree a)

-- | We use @'Word32'@ values for indexing leaves in the tree.
type LeafIndex = Word32

-- | Non-empty Merkle subtree.
data MerkleNode a
    = MerkleBranch
       { mnRoot  :: !(MerkleSignature a)
       , mnLeft  :: !(MerkleNode a)
       , mnRight :: !(MerkleNode a) }
    | MerkleLeaf
       { mnRoot :: !(MerkleSignature a)
       , mnVal  :: !a }
    deriving (Show, Eq, Generic)

-- | Helper function for fetching a size of @'MerkleNode'@ subtree.
mnSize :: MerkleNode a -> Word32
mnSize = msSize . mnRoot

instance Foldable MerkleNode where
    foldMap f MerkleLeaf {mnVal} = f mnVal
    foldMap f MerkleBranch {mnLeft, mnRight} =
        F.foldMap f mnLeft `mappend` F.foldMap f mnRight

    length = fromIntegral . mnSize

-- | Constructs a @'MerkleSignature'@ for a hashable object.
-- The signature always have size 1, since the single object
-- is hashed.
mkMerkleSig :: HasHash a => a -> MerkleSignature a
mkMerkleSig a = MerkleSignature 1 (unsafeHash a)

-- | Makes a leaf node of Merkle tree given the element of type `a`.
mkLeaf :: HasHash a => a -> MerkleNode a
mkLeaf a = MerkleLeaf
    { mnVal   = a
    , mnRoot  = mkMerkleSig a
    }

-- | Given two Merkle subtrees, makes a tree which has them
-- as children.
mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch l r = MerkleBranch
    { mnLeft  = l
    , mnRight = r
    , mnRoot  = mkBranchRootHash (mnRoot l) (mnRoot r)
    }

-- | Concatenates two @'MerkleSignature'@s and calculates another
-- @'MerkleSignature'@ on top of them.
mkBranchRootHash
    :: MerkleSignature a -- ^ left merkle root
    -> MerkleSignature a -- ^ right merkle root
    -> MerkleSignature a
mkBranchRootHash (MerkleSignature sl hl) (MerkleSignature sr hr) =
    MerkleSignature
    (sl + sr)
    (hash $ toLazyByteString $ mconcat
        [ word32LE sl
        , byteString (convert hl)
        , word32LE sr
        , byteString (convert hr) ])
  where
    toLazyByteString :: Builder -> LBS.ByteString
    toLazyByteString = Builder.toLazyByteStringWith
        -- This bytestring is build only to be hashed, and then it's
        -- discarded. @'untrimmedStrategy'@ is a recommended strategy
        -- for this case.
        -- All the stuff to hash fits into 72 bytes, but let's allocate
        -- 128 in the first buffer just in case (we don't know inner @'Builder'@)
        -- machinery good enough.
        (Builder.untrimmedStrategy 128 Builder.smallChunkSize)
        mempty

-- | Construct a @'MerkleTree'@ over a @'Foldable'@ container.
fromFoldable :: (HasHash a, Foldable t) => t a -> MerkleTree a
fromFoldable = Exts.fromList . F.toList

-- | Construct a @'MerkleTree'@ over a @'Container'@
fromContainer :: (HasHash (Element t), Container t) => t -> MerkleTree (Element t)
fromContainer = Exts.fromList . toList

-- Construct a @'MerkleTree'@ over a list.
instance HasHash a => Exts.IsList (MerkleTree a) where
    type Item (MerkleTree a) = a
    fromList []       = MerkleEmpty
    fromList (a : as) = MerkleTree $ nodeFromList (a :| as)

    toList = F.toList

-- | Construct some merkle tree-like structure over a non-empty list.
merkleFromList :: (b -> node a) -> (node a -> node a -> node a) -> NonEmpty b -> node a
merkleFromList toLeaf toBranch lst@(a :| as) = tree
  where
    (tree, assert_ null -> ()) = go (0, uLen - 1) `runState` (a : as)

    uLen = length lst

    go (lo, hi)
        | lo == hi  = toLeaf <$> pop
        | otherwise = toBranch <$> go (lo, mid) <*> go (mid + 1, hi)
      where
        mid = (lo + hi) `div` 2

    pop = state $ \case
        []    -> error "nodeFromList: impossible"
        c : s -> (c, s)

-- | Construct a @'MerkleNode'@ over a non-empty list
nodeFromList :: HasHash a => NonEmpty a -> MerkleNode a
nodeFromList = merkleFromList mkLeaf mkBranch

-- | Returns root of merkle tree. Returns @'emptyHash'@, if tree is empty.
getMerkleRoot :: MerkleTree a -> MerkleSignature a
getMerkleRoot MerkleEmpty    = emptyHash
getMerkleRoot (MerkleTree x) = mnRoot x

-- | Fixed signature, which is used as root of the @'MerkleTree'@
-- if it's empty.
emptyHash :: MerkleSignature a
emptyHash = MerkleSignature 0 (hash mempty)

-- | Merkle proof. Contains a subset of leaves of @'MerkleTree'@ and
-- roots of pruned subtrees.
data MerkleProof a
    = ProofBranch
        { mpLeft  :: !(MerkleProof a)
        , mpRight :: !(MerkleProof a) }
    | ProofLeaf
        { mpVal :: !a
        }
    | ProofPruned
        { mpSig :: !(MerkleSignature a) }
    deriving (Show, Eq, Generic)

-- TODO: provide a useful instance
instance Buildable (MerkleProof a) where
    build _ = "<merkle proof>"

instance Foldable MerkleProof where
    foldMap _ ProofPruned{} = mempty
    foldMap f (ProofLeaf v) = f v
    foldMap f (ProofBranch l r) =
        F.foldMap f l `mappend` F.foldMap f r

instance Container (MerkleProof a)

-- | Calculates the size of Merkle subtree corresponding to
-- given Merkle proof.
mpSize :: MerkleProof a -> Word32
mpSize (ProofPruned s)   = msSize s
mpSize (ProofLeaf _)     = 1
mpSize (ProofBranch l r) = mpSize l + mpSize r

-- | Datatype for a @'MerkleProof'@, root hash for which has
-- already been pre-calculated and which is ready for validation.
--
-- Constructor for this datatype is not exported - it should be
-- possible to get the value of this datatype only via
-- calculating all the inner nodes of an existing @'MerkleProof'@
data MerkleProofReady a = UnsafeMerkleProofReady
    { mprRoot  :: !(MerkleSignature a)
    , mprProof :: !(MerkleProof a)
    } deriving (Show, Eq, Generic)

instance Buildable (MerkleProofReady a) where
    build proofR = "Merkle proof { root = "+|mprRoot proofR|+
                   ", proof = "+|mprProof proofR|+" }"

-- | Reconstructs the root of given @'MerkleProof'@.
reconstructRoot :: HasHash a => MerkleProof a -> MerkleSignature a
reconstructRoot (ProofBranch l r) =
    mkBranchRootHash (reconstructRoot l) (reconstructRoot r)
reconstructRoot (ProofPruned s) = s
reconstructRoot (ProofLeaf a) = mkMerkleSig a

-- | Reconstructs the root of given @'MerkleProof'@ and yields the
-- @'MerkleProofReady'@ object.
readyProof :: HasHash a => MerkleProof a -> MerkleProofReady a
readyProof proof = UnsafeMerkleProofReady (reconstructRoot proof) proof

-- | Helper function to fetch the size of the @'MerkleTree'@ from which
-- the proof were fetched.
mprSize :: MerkleProofReady a -> Word32
mprSize = msSize . mprRoot

-- | Construct a @'MerkleProof'@ over a non-empty list
merkleProofFromList :: HasHash a => NonEmpty a -> MerkleProof a
merkleProofFromList = merkleFromList ProofLeaf ProofBranch

-- | Merges two already validated Merkle proofs, checking that all common
-- inner nodes match.
mergeProofs
    :: MerkleProofReady a
    -> MerkleProofReady a
    -> Either Text (MerkleProofReady a)
mergeProofs a b
    | mprRoot a /= mprRoot b = Left "Proof roots mismatch"
    | otherwise = UnsafeMerkleProofReady (mprRoot a) <$>
                  merge (mprProof a) (mprProof b)
  where
    merge (ProofBranch l r) (ProofBranch l' r') =
        ProofBranch <$> merge l l' <*> merge r r'
    merge ProofBranch{} ProofLeaf{} =
        Left "Inner node on the left is mismatched with leaf on the right"
    merge ProofLeaf{} ProofBranch{} =
        Left "Leaf on the left is mismatched with inner node on the right"
    -- No need to compare values in leaves, because proofs are 'Valid',
    -- and hash function should be collision-resistnant.
    merge a'@ProofLeaf{} ProofLeaf{} = pure a'
    merge a' ProofPruned{} = pure a'
    merge ProofPruned{} b' = pure b'

-- | Picks an element of given @'MerkleTree'@ by index and constructs
-- a @'MerkleProof'@ with this element as the only leaf.
--
-- Returns @'Nothing'@ if the index is outside the tree's bounds or if
-- the tree is empty.
mkMerkleProofSingle :: MerkleTree a -> LeafIndex -> Maybe (MerkleProof a)
mkMerkleProofSingle t = mkMerkleProof t . one

-- | Picks elements of given @'MerkleTree'@ by given indices and
-- constructs a @'MerkleProof'@ with those elements as the only leaves.
--
-- Returns @'Nothing'@ if none of given indices is inside the tree's
-- bounds or if the tree is empty.
mkMerkleProof :: MerkleTree a -> Set LeafIndex -> Maybe (MerkleProof a)
mkMerkleProof MerkleEmpty _ = Nothing
mkMerkleProof (MerkleTree rootNode) idxs' =
    case constructProof 0 rootNode idxs' of
        ProofPruned _ -> Nothing
        branch        -> Just branch
  where
    constructProof :: Word32 -> MerkleNode a -> Set LeafIndex -> MerkleProof a
    constructProof padding node idxs
        | Set.null idxs =
              ProofPruned $ mnRoot node
        | Set.findMax idxs < padding =
              ProofPruned $ mnRoot node
        | Set.findMin idxs >= padding + mnSize node =
              ProofPruned $ mnRoot node
        | otherwise = case node of
              (MerkleLeaf _ v) -> ProofLeaf v
              (MerkleBranch _ l r) ->
                  let pivot = padding + mnSize l
                      (idxsL, idxsR) = splitIdxs pivot idxs
                  in ProofBranch
                     (constructProof padding l idxsL)
                     (constructProof pivot r idxsR)

    splitIdxs idx idxs =
        let (l, idxFound, r) = Set.splitMember idx idxs
        in (l, if idxFound then Set.insert idx r else r)

-- | Lookup an element in @'MerkleProof'@ by index.
-- Returns @'Nothing'@ if a Merkle tree leaf with given index is
-- not included into proof.
--
-- `O(k * h)`, where `k` is number of leaves in the proof and
-- `h` is the height of the proof.
lookup :: LeafIndex -> MerkleProof a -> Maybe a
lookup _ ProofPruned{} = Nothing
lookup _ (ProofLeaf v) = Just v
lookup i (ProofBranch l r) = let lsize = mpSize l in
    if i < lsize
    then lookup i l
    else lookup (i - lsize) r

validateElementExistAt :: Eq a => LeafIndex -> a -> MerkleProof a -> Bool
validateElementExistAt index value proof = lookup index proof == Just value

-- | Validate a pre-calculated Merkle proof against given Merkle root.
validateMerkleProof
    :: HasHash a
    => MerkleProofReady a
    -> MerkleSignature a
    -> Bool
validateMerkleProof proof root = mprRoot proof == root

-- | Debug print of tree.
drawMerkleTree :: Show a => MerkleTree a -> String
drawMerkleTree MerkleEmpty = "empty tree"
drawMerkleTree (MerkleTree n) = Tree.drawTree $ asTree n
  where
    asTree :: Show a => MerkleNode a -> Tree.Tree String
    asTree MerkleBranch {..} =
        Tree.Node (show mnRoot) [asTree mnLeft, asTree mnRight]
    asTree MerkleLeaf {..} =
        Tree.Node ("leaf (" <> show mnRoot <> ", " <> show mnVal <> ")") []

-- | Debug print of proof tree.
drawProofNode :: Show a => MerkleProof a -> String
drawProofNode = Tree.drawTree . asTree
  where
    asTree :: Show a => MerkleProof a -> Tree.Tree String
    asTree ProofLeaf {..}   = Tree.Node ("leaf, " <> show mpVal) []
    asTree ProofBranch {..} = Tree.Node "branch" [asTree mpLeft, asTree mpRight]
    asTree ProofPruned {..} = Tree.Node ("pruned, " <> show mpSig) []

-- | A special unit-isomorphic type which acts as a placeholder for
-- an element in Merkle structure.
data ElementStub = ElementStub
    deriving (Show, Eq, Generic)

-- | Merkle tree with values removed. Used for storing Merkle trees
-- in the Educator database.
newtype EmptyMerkleTree a = EmptyTree
    { unEmptyTree :: MerkleTree ElementStub
    } deriving newtype (Show, Eq, Generic)

-- | Empties out the Merkle tree, putting @'ElementStub'@s in leaves.
getEmptyMerkleTree :: MerkleTree a -> EmptyMerkleTree a
getEmptyMerkleTree = EmptyTree . go
    where
      go MerkleEmpty       = MerkleEmpty
      go (MerkleTree node) = MerkleTree $ goNode node

      goNode (MerkleBranch s l r) =
          MerkleBranch (coerce s) (goNode l) (goNode r)
      goNode (MerkleLeaf s _)     =
          MerkleLeaf (coerce s) ElementStub

-- | Given a @'Map'@ from index to the element, fills @'EmptyMerkleTree'@,
-- yielding a @'MerkleProof'@ which has provided elements placed to their
-- corresponing indices as leaves.
fillEmptyMerkleTree :: Map LeafIndex a -> EmptyMerkleTree a -> Maybe (MerkleProof a)
fillEmptyMerkleTree plugs (EmptyTree sieve) =
    let keySet = Set.fromList (keys plugs)
        proof  = mkMerkleProof sieve keySet
    in fill <$> proof
  where
    fill it = evalState (aux it) 0
      where
        aux (ProofBranch left right) =
            ProofBranch <$> aux left <*> aux right
        aux (ProofLeaf ElementStub) =
            ProofLeaf . (plugs Map.!) <$> next
        aux (ProofPruned sig) =
            ProofPruned (coerce sig) <$ skip (msSize sig)

        next   = state $ \i -> (i,  i + 1)
        skip n = state $ \i -> ((), i + n)

-- | Merkle proof with values at leaves removed and replaced via @Pruned@
-- nodes.
newtype EmptyMerkleProof a = EmptyProof
    { unEmptyProof :: MerkleProof ElementStub
    } deriving newtype (Eq, Show, Generic)

-- | Empties out @'MerkleProof'@, putting @'ElementStub'@s in leaves.
getEmptyMerkleProof :: MerkleProof a -> EmptyMerkleProof a
getEmptyMerkleProof = EmptyProof . go
    where
      go (ProofBranch l r) = ProofBranch (go l) (go r)
      go (ProofPruned s)   = ProofPruned $ coerce s
      go (ProofLeaf _)     = ProofLeaf ElementStub

-- | @'mkMerkleProof'@ for empty trees and proofs
mkEmptyMerkleProof
    :: EmptyMerkleTree a
    -> Set LeafIndex
    -> Maybe (EmptyMerkleProof a)
mkEmptyMerkleProof (EmptyTree tree) idxs =
    EmptyProof <$> mkMerkleProof tree idxs

-- | Splits Merkle proof into signatures and data.
separateProofAndData :: MerkleProof a -> (EmptyMerkleProof a, [a])
separateProofAndData proof = (getEmptyMerkleProof proof, toList proof)

-- | Merges empty proof and list of data elements into one Merkle proof.
-- Returns @'Nothing'@ if there's too few or too many data elements provided.
mergeProofAndData :: forall a. EmptyMerkleProof a -> [a] -> Maybe (MerkleProof a)
mergeProofAndData (EmptyProof proof) =
    evalState (runMaybeT $ go proof)
  where
    go :: MerkleProof ElementStub -> MaybeT (State [a]) (MerkleProof a)
    go (ProofBranch l r) = ProofBranch <$> go l <*> go r
    go (ProofPruned s)   = pure . ProofPruned $ coerce s
    go (ProofLeaf _)     = ProofLeaf <$> MaybeT pickLeaf

    pickLeaf = state $ \case
        []       -> (Nothing, [])
        (a : as) -> (Just a, as)

{-
  Merkles for JSON
-}

-- Just a type tag for JSON merkles
-- data AuthJSON

-- Build a Merkle tree over a bunch of Merkle trees
treeOfTrees :: NonEmpty (MerkleNode a) -> MerkleNode a
treeOfTrees = merkleFromList id mkBranch

jsonValueSig :: HasHash a => a -> MerkleSignature Value
jsonValueSig = MerkleSignature 1 . unsafeHash

kvPairMerkle :: (Text, Value) -> MerkleNode Value
kvPairMerkle (k, v) = mkBranch l r
  where l = MerkleLeaf (jsonValueSig @ByteString $ encodeUtf8 k) Null
        r = merkleFromJson v

merkleFromJson :: Value -> MerkleNode Value
merkleFromJson Null = MerkleLeaf (jsonValueSig @ByteString "null") Null
merkleFromJson a@(Bool b) = MerkleLeaf (jsonValueSig (B.encode b)) a
merkleFromJson a@(Number n) = MerkleLeaf (jsonValueSig (B.encode n))a
merkleFromJson a@(String s) = MerkleLeaf (jsonValueSig (B.encode s)) a
merkleFromJson a@(Array arr) = case map merkleFromJson (toList arr) of
  [] -> MerkleLeaf (jsonValueSig @ByteString "empty_list") a
  (n:ns) -> treeOfTrees (n :| ns)
merkleFromJson a@(Object obj) =
  let ascList = sortOn fst $ HM.toList obj
  in case map kvPairMerkle ascList of
    [] -> MerkleLeaf (jsonValueSig @ByteString "empty_obj") a
    (p:ps) -> treeOfTrees (p :| ps)

merkleTreeFromJson :: Value -> MerkleTree Value
merkleTreeFromJson = MerkleTree . merkleFromJson
