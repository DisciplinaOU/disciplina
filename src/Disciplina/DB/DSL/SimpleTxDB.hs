module Disciplina.DB.DSL.SimpleTxDB
              where

import Universum
import Disciplina.Crypto.Signing.Class (AbstractPK (..))
import Data.ByteArray (ByteArrayAccess(..))
import Disciplina.DB.DSL.Types (QueryTx(..), QueryTxs(..), WHERE (..)
                               ,TxIdEq(..), TxGrade(..), QueryObj(..)
                               ,TxsFilterExpr(..), ObjHashEq(..))
import Disciplina.Educator.Txs (PrivateTxId(..), PrivateTx(..)
                               ,StudentTxMsg(..), PrivateTxPayload(..))
import Disciplina.Crypto (Hash, hash, PublicKey)
import Disciplina.Core (Address(..), CourseId(..))
import Disciplina.DB.DSL.Interpret (MonadSearchTxObj(..), RunQuery(..))
import qualified Disciplina.Core as Core (Grade(..), SubjectId(..))


instance ByteArrayAccess PrivateTx => ByteArrayAccess PrivateTx

newtype SimpleTxDB m = SimpleTxDB
   { getSimpleTxDB :: Reader [PrivateTx] m
   } deriving (Functor, Applicative, Monad, MonadReader [PrivateTx])


instance MonadSearchTxObj SimpleTxDB where
  runTxQuery = runSimpleTxQuery
  runTxsQuery = undefined
  runObjQuery = undefined


runSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
runSimpleTxQuery (SELECTTx _ (TxIdEq (a :: PrivateTxId)))
  = do
     txs :: [PrivateTx] <- ask
     return (safeHead txs)


foo :: SimpleTxDB (Maybe PrivateTx)
foo = do
  res <- runQuery (SELECTTx WHERE (TxIdEq (hash mkPrivateTx)))
  return res


runSimpleTxDB :: Maybe PrivateTx
runSimpleTxDB = runReader (getSimpleTxDB foo) [mkPrivateTx]


mkPrivateTx :: PrivateTx
mkPrivateTx = PrivateTx {
       _ptxStudentId = studentId
     , _ptxCourseId = courseId
     , _ptxEducatorId = educatorId
     , _ptxPayload = payload
     }
 where
   studentId = Address undefined
   courseId = CourseId { ciSubject = 1
                       , ciId = 2
                       }
   educatorId = Address undefined
   payload = StudentTx { _ptxStudentMsg  = Enroll }

