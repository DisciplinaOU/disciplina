
-- | Module for methods related to Activity Type Graph

module Disciplina.Core.ATG
       ( atgDFSTraverse
       , atgDFS
       , ATGIndexed
       , mkATGIndexed
       ) where

import Universum

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Disciplina.Core.Types (ATG (..), ATGEdge (..), ATGNode (..), SubjectId, atgeChild)

type ATGTraverseT = StateT (Set SubjectId)

-- | Depth-first search (generalized)
atgDFSTraverse :: Monad m
    => (SubjectId -> b -> m b)
    -> ATGNode
    -> b
    -> ATGTraverseT m b
atgDFSTraverse mf (ATGNode sId es) acc = do
    visited <- gets (S.member sId)
    if visited
        then return acc
        else do
        modify' (S.insert sId)
        acc' <- lift $ mf sId acc
        foldlM (flip $ atgDFSTraverse mf) acc' $ map (^.atgeChild) es

atgDFSM :: Monad m => (SubjectId -> b -> m b) -> ATGNode -> b -> m b
atgDFSM f node acc = evalStateT (atgDFSTraverse f node acc) mempty

atgDFS :: (SubjectId -> b -> b) -> ATGNode -> b -> b
atgDFS f node = runIdentity . atgDFSM ((Identity .) . f) node

-- | ATG with an index from subject IDs to nodes.
-- Constructor is unsafe, because it's totally possible
-- to construct invalid index manually.
data ATGIndexed = UnsafeATGIndexed
    { _atgiGraph :: !ATG
    , _atgiIndex :: !(Map SubjectId ATGNode)
    } deriving (Show, Eq, Generic)

buildIndex :: ATG -> Map SubjectId ATGNode
buildIndex = foldl' goNode mempty . getATGRoots
  where goNode mp node@(ATGNode sId es) =
            if M.member sId mp
            then mp
            else foldl' goNode (M.insert sId node mp) $
                 map (^.atgeChild) es

mkATGIndexed :: ATG -> ATGIndexed
mkATGIndexed atg = UnsafeATGIndexed atg $ buildIndex atg
