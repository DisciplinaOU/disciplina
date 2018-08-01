module Dscp.DB.DSL.Interpret.SimpleTxDB
       ( runSimpleTxDBQuery
       ) where

import Control.Lens (filtered, makePrisms, traversed)
import Data.List (intersect, union)
import Data.Map.Strict (Map)

import Dscp.Core (Assignment (..), Course (..), SignedSubmission (..), Subject, Submission (..),
                  activityTypeGraphIndexed, hasPathFromTo)
import Dscp.Core.Foundation.Educator (PrivateTx (..))
import Dscp.Crypto (hash)
import Dscp.DB.DSL.Class (MonadSearchTxObj (..), Obj, ObjHashEq (..), QueryObj (..), QueryTx (..),
                          QueryTxs (..), RunQuery (..), TxIdEq (..), TxsFilterExpr (..), WHERE (..))
import Dscp.Util (HasId (Id))

import qualified Data.Map.Strict as Map hiding (Map)

-- | Transactions and objects to be stored in database
data SimpleObj = SSTx  { _sotorePTx :: !PrivateTx }
               | SSObj { _sotoreObj :: !Obj }

makePrisms ''SimpleObj

-- | Simple database containing transactions, object
-- and a mapping of course id to subject ids.
data SimpleDB = SimpleDB
    { sdbGetSimpleObj        :: ![SimpleObj]
    , sdbCourseIdToSubjectId :: !(Map (Id Course) [Id Subject])
    }

-- | Database put in a reader environment
newtype SimpleTxDB a = SimpleTxDB
    { runSimpleTxDB :: Reader SimpleDB a
    } deriving (Functor, Applicative, Monad, MonadReader SimpleDB)

instance MonadSearchTxObj SimpleTxDB where
    runTxQuery = evalSimpleTxQuery
    runTxsQuery = evalSimpleTxsQuery
    runObjQuery = evalSimpleObjQuery

-- | Evaluator for query: find Tx in db with hash == h
evalSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
evalSimpleTxQuery (SELECTTx _ (TxIdEq (h :: Id PrivateTx))) = do
    db <- asks sdbGetSimpleObj
    return $ db ^? traversed . _SSTx . filtered (((h==).hash))

-- | Evaluator for query: find Obj in db with obj hash == h
evalSimpleObjQuery :: QueryObj -> SimpleTxDB (Maybe Obj)
evalSimpleObjQuery (SELECTObj _ (ObjHashEq h)) = do
    db <- asks sdbGetSimpleObj
    return $ db ^? traversed . _SSObj . filtered (((h==).hash))

-- | Evaluator for query: find Txs in db with SubjectId == a
evalSimpleTxsQuery :: QueryTxs -> SimpleTxDB [PrivateTx]
evalSimpleTxsQuery (SELECTTxs _ (TxHasSubjectId sId)) = do
    db <- asks sdbGetSimpleObj
    subjToCourseMap <- asks sdbCourseIdToSubjectId
    let txs = db ^.. traversed
                  . _SSTx
                  . filtered (subjectIdHasCourseId subjToCourseMap . getTxCourseId)
    return txs
  where subjectIdHasCourseId subjToCourseMap courseId =
          case any (== sId) <$> Map.lookup courseId subjToCourseMap of
                 Just True -> True
                 _         -> False

-- | Evaluator for query: find Txs in db with grade == g
evalSimpleTxsQuery (SELECTTxs _ (_ :== grade)) = do
    db <- asks sdbGetSimpleObj
    return $ db ^.. traversed
                 . _SSTx
                 . filtered ((== grade)._ptGrade)

-- | Evaluator for query: find Txs in db with grade >= g
evalSimpleTxsQuery (SELECTTxs _ (_ :>= grade)) = do
    db <- asks sdbGetSimpleObj
    return $ db ^.. traversed
                 . _SSTx
                 . filtered ((>= grade)._ptGrade)

-- | Evaluator for AND query
evalSimpleTxsQuery (SELECTTxs _ (a :& b)) =
    intersect <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for OR query
evalSimpleTxsQuery (SELECTTxs _ (a :|| b)) =
    union <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for query: find all txs with subjectId which is descendant of sId
evalSimpleTxsQuery (SELECTTxs _ (TxHasDescendantOfSubjectId sId)) = do
    db <- asks sdbGetSimpleObj
    subjToCourseMap <- asks sdbCourseIdToSubjectId
    let txs = db ^.. traversed
                  . _SSTx
                  . filtered (hasDescendantOf subjToCourseMap . getTxCourseId)
    return txs
  where hasDescendantOf subjToCourseMap courseId =
          case any (isDescendantOf sId) <$> Map.lookup courseId subjToCourseMap of
                 Just True -> True
                 _         -> False

        isDescendantOf x y = hasPathFromTo activityTypeGraphIndexed x y

-- | Run query in SimpleTxDB
runSimpleTxDBQuery :: RunQuery a b => [PrivateTx] -> [Obj] -> a -> b
runSimpleTxDBQuery dbTx dbObj query =
    runReader (runSimpleTxDB . runQuery $ query) mkDb
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
        -- Taken from Dscp.Core.ATG
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
        cId1 = Course 1
        cId2 = Course 2
        cId3 = Course 3
        cId4 = Course 4
        cId5 = Course 5

getTxCourseId :: PrivateTx -> Id Course
getTxCourseId tx = _aCourseId (_sAssignment (_ssSubmission (_ptSignedSubmission tx)))
