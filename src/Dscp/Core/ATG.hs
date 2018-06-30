
-- | Module for methods related to Activity Type Graph

module Dscp.Core.ATG
       ( ATGIndexed
       , atgiGraph
       , atgiIndex
       , mkATGIndexed
       , hasPathFromM
       , hasPathFromTo
       , activityTypeGraph
       , activityTypeGraphIndexed
       ) where

import Control.Lens (makeLenses)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Dscp.Core.Types (ATG (..), ATGEdge (..), ATGNode (..), SubjectId, atgeChild)
import Dscp.Util (anyMapM)

-- | ATG with an index from subject IDs to nodes.
-- Constructor is unsafe, because it's totally possible
-- to construct invalid index manually.
data ATGIndexed = UnsafeATGIndexed
    { _atgiGraph :: !ATG
    , _atgiIndex :: !(Map SubjectId ATGNode)
    } deriving (Show, Eq, Generic)

makeLenses ''ATGIndexed

buildIndex :: ATG -> Map SubjectId ATGNode
buildIndex = foldl' goNode mempty . getATGRoots
  where goNode mp node@(ATGNode sId es) =
            if M.member sId mp
            then mp
            else foldl' goNode (M.insert sId node mp) $
                 map (^.atgeChild) es

mkATGIndexed :: ATG -> ATGIndexed
mkATGIndexed atg = UnsafeATGIndexed atg $ buildIndex atg

type ATGTraverse = State (Set SubjectId)

-- | Check reachability using DFS
hasPathFromM :: SubjectId -> ATGNode -> ATGTraverse Bool
hasPathFromM sId (ATGNode sId' es)
    | sId == sId' = return True
    | otherwise = gets (S.member sId') >>= \case
          True -> return False
          False -> anyMapM (hasPathFromM sId) $ map (^.atgeChild) es

-- | Given indexed ATG and two subject IDs, determine if
-- there's a path from subject A to subject B.
-- If either of given subjects are not present in ATG, then
-- there's automatically no path.
hasPathFromTo :: ATGIndexed -> SubjectId -> SubjectId -> Bool
hasPathFromTo atgi sFrom sTo =
    case M.lookup sFrom (atgi^.atgiIndex) of
        Nothing   -> False
        Just node -> evalState (hasPathFromM sTo node) mempty

-------------------------------------------------------------
-- Sample ATG
-------------------------------------------------------------

{-
@flyingleafe: Determining edges weight is a non-trivial task,
so for now I leave them all equal to 1.

Sample ATG structure repeats ATG described in yellowpaper, except for 'Education' node,
which was meant to be the common root for everything, but now we don't necessarily
need a common root.
-}

mkATGNode :: SubjectId -> [ATGNode] -> ATGNode
mkATGNode sId = ATGNode sId . map (ATGEdge 1)

atgMathematics, atgComputerScience, atgElementary,
    atgCalculi, atgLogic, atgEngineering, atgTheory,
    atgHighSchoolAlgebra, atgPiCalculus, atgComputabilityTheory
    :: ATGNode

atgMathematics =
    mkATGNode 1 [atgElementary, atgCalculi, atgLogic]
atgComputerScience =
    mkATGNode 2 [atgEngineering, atgTheory]
atgElementary =
    mkATGNode 3 [atgHighSchoolAlgebra]
atgCalculi =
    mkATGNode 4 [atgHighSchoolAlgebra, atgPiCalculus]
atgLogic =
    mkATGNode 5 [atgComputabilityTheory]
atgEngineering =
    mkATGNode 6 []
atgTheory =
    mkATGNode 7 [atgComputabilityTheory]
atgHighSchoolAlgebra =
    mkATGNode 8 []
atgPiCalculus =
    mkATGNode 9 []
atgComputabilityTheory =
    mkATGNode 10 [atgPiCalculus]

activityTypeGraph :: ATG
activityTypeGraph = ATG [atgMathematics, atgComputerScience]

activityTypeGraphIndexed :: ATGIndexed
activityTypeGraphIndexed = mkATGIndexed activityTypeGraph
