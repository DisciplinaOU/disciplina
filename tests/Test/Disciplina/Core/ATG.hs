module Test.Disciplina.Core.ATG where

import Test.Common

import Disciplina.Core (SubjectId)
import qualified Disciplina.Core as Core

pathFromTo :: SubjectId -> SubjectId -> Bool
pathFromTo = Core.hasPathFromTo Core.activityTypeGraphIndexed

(~~>) :: SubjectId -> SubjectId -> Assertion
a ~~> b = assertBool "path doesn't exist" $ pathFromTo a b

(!~>) :: SubjectId -> SubjectId -> Assertion
a !~> b = assertBool "path exists" . not $ pathFromTo a b

test_validPaths :: TestTree
test_validPaths = testGroup "Valid path queries"
    [ testCase "Mathematics is an ancestor of Logic" $ 1 ~~> 5
    , testCase "Algebra isn't ancestor of Mathematics" $ 8 !~> 1
    , testCase "Mathematics isn't ancestor of Engineering" $ 1 !~> 6
    , testCase "Mathematics is an ancestor of pi-calculus" $ 1 ~~> 9
    , testCase "CS is an ancestor of pi-calculus" $ 2 ~~> 9
    ]

test_invalidPaths :: TestTree
test_invalidPaths = testGroup "Invalid path queries"
    [ testCase "First subject isn't in the ATG #1" $ 0 !~> 5
    , testCase "First subject isn't in the ATG #2" $ 17 !~> 3
    , testCase "Second subject isn't in the ATG" $ 4 !~> 19
    , testCase "Both subjects aren't in the ATG" $ 123 !~> 881
    ]
