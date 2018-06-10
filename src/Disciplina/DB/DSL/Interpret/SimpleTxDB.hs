module Disciplina.DB.DSL.Interpret.SimpleTxDB
       ( runSimpleTxDBQuery
       ) where

import Universum

import Control.Lens (_Just, at, filtered, makePrisms, traversed, to)
import Data.Map (Map)
import Data.List (intersect, union)

import Disciplina.Core (CourseId (..), SubjectId, hasPathFromTo, activityTypeGraphIndexed)
import Disciplina.Crypto (hash)
import Disciplina.DB.DSL.Class (MonadSearchTxObj (..), Obj, ObjHashEq (..), QueryObj (..),
                                QueryTx (..), QueryTxs (..), RunQuery (..), TxIdEq (..),
                                TxsFilterExpr (..), WHERE (..))
import Disciplina.Educator (EducatorTxMsg (..), PrivateTx (..), PrivateTxId, PrivateTxPayload (..))

import qualified Data.Map as Map hiding (Map)

-- | Transactions and objects to be stored in database
data SimpleObj = SSTx  { _sstorePTx :: !PrivateTx }
               | SSObj { _sstoreObj :: !Obj }

makePrisms ''SimpleObj

-- | Simple database containing transactions, object
-- and a mapping of course id to subject ids.
data SimpleDB = SimpleDB
    { _getSimpleObj        :: ![SimpleObj]
    , _courseIdToSubjectId :: !(Map CourseId [SubjectId])
    }

-- | Database put in a reader environment
newtype SimpleTxDB a = SimpleTxDB
    { getSimpleTxDB :: Reader SimpleDB a
    } deriving (Functor, Applicative, Monad, MonadReader SimpleDB)

instance MonadSearchTxObj SimpleTxDB where
    runTxQuery = evalSimpleTxQuery
    runTxsQuery = evalSimpleTxsQuery
    runObjQuery = evalSimpleObjQuery

-- | Evaluator for query: find Tx in db with hash == h
evalSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
evalSimpleTxQuery (SELECTTx _ (TxIdEq (h :: PrivateTxId))) = do
    db <- asks _getSimpleObj
    return $ db ^? traversed . _SSTx . filtered (((h==).hash))

-- | Evaluator for query: find Obj in db with obj hash == h
evalSimpleObjQuery :: QueryObj -> SimpleTxDB (Maybe Obj)
evalSimpleObjQuery (SELECTObj _ (ObjHashEq h)) = do
    db <- asks _getSimpleObj
    return $ db ^? traversed . _SSObj . filtered (((h==).hash))

-- | Evaluator for query: find Txs in db with SubjectId == a
evalSimpleTxsQuery :: QueryTxs -> SimpleTxDB [PrivateTx]
evalSimpleTxsQuery (SELECTTxs _ (TxHasSubjectId sId)) = do
    db <- asks _getSimpleObj
    subjToCourseMap <- asks _courseIdToSubjectId
    let txs = db ^.. traversed
                  . _SSTx
                  . filtered (subjectIdHasCourseId subjToCourseMap . _ptxCourseId)
    return txs
  where subjectIdHasCourseId subjToCourseMap courseId =
          case subjToCourseMap ^? at courseId . _Just . to (any (==sId)) of
                 Just True -> True
                 _         -> False

-- | Evaluator for query: find Txs in db with grade == g
evalSimpleTxsQuery (SELECTTxs _ (TxGradeEq grade)) = do
    db <- asks _getSimpleObj
    return $ db ^.. traversed
                 . _SSTx
                 . filtered ((== Just grade).getGrade._ptxPayload)
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _                            = Nothing

-- | Evaluator for query: find Txs in db with grade >= g
evalSimpleTxsQuery (SELECTTxs _ (_ :>= grade)) = do
    db <- asks _getSimpleObj
    return $ db ^.. traversed
                 . _SSTx
                 . filtered ((>= Just grade).getGrade._ptxPayload)
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _                            = Nothing

-- | Evaluator for AND query
evalSimpleTxsQuery (SELECTTxs _ (a :& b)) =
    intersect <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for OR query
evalSimpleTxsQuery (SELECTTxs _ (a :|| b)) =
    union <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for query: find all txs with subjectId which is descendant of sId
evalSimpleTxsQuery (SELECTTxs _ (TxHasDescendantOfSubjectId sId)) = do
    db <- asks _getSimpleObj
    subjToCourseMap <- asks _courseIdToSubjectId
    let txs = db ^.. traversed
                  . _SSTx
                  . filtered (hasDescendantOf subjToCourseMap . _ptxCourseId)
    return txs
  where hasDescendantOf subjToCourseMap courseId =
          case subjToCourseMap ^? at courseId . _Just . to (any (isDescendantOf sId)) of
                 Just True -> True
                 _         -> False

        isDescendantOf x y = hasPathFromTo activityTypeGraphIndexed x y

-- | Run query in SimpleTxDB
runSimpleTxDBQuery :: RunQuery a b => [PrivateTx] -> [Obj] -> a -> b
runSimpleTxDBQuery dbTx dbObj query =
    runReader (getSimpleTxDB . runQuery $ query) mkDb
  where mkDb = SimpleDB (fmap SSTx dbTx <> fmap SSObj dbObj) courseToSubj
        courseToSubj = Map.fromList [ (cId1, [sIdMathematics
                                             ,sIdEngineering
                                             ,sIdLogic
                                             ,sIdCalculi
                                             ,sIdTheory
                                             ,sIdPiCalculus
                                             ,sIdComputabilityTheory
                                             ])
                                    , (cId2, [sIdHighSchoolAlgebra
                                             ,sIdMathematics
                                             ])
                                    , (cId3, [sIdComputerScience])
                                    , (cId4, [sIdElementary])
                                    , (cId5, [sIdEngineering])
                                    ]
        -- Some arbitrarly choosen subject ids.
        -- Taken from Disciplina.Core.ATG
        sIdMathematics = 1
        sIdComputerScience = 2
        sIdElementary = 3
        sIdCalculi = 4
        sIdLogic = 5
        sIdEngineering = 6
        sIdTheory = 7
        sIdHighSchoolAlgebra = 8
        sIdPiCalculus = 9
        sIdComputabilityTheory = 10
        -- Also some arbitrarly choosen course ids.
        cId1 = CourseId 1
        cId2 = CourseId 2
        cId3 = CourseId 3
        cId4 = CourseId 4
        cId5 = CourseId 5
