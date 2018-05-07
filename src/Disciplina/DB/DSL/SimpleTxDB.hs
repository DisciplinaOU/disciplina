module Disciplina.DB.DSL.SimpleTxDB
              where

import Universum

import Crypto.Error (CryptoFailable(..))
import Codec.Serialise (serialise, deserialise)
import Disciplina.Crypto.Signing.Class (AbstractPK(..), AbstractSK(..))
import Data.ByteArray (ByteArrayAccess(..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Disciplina.DB.DSL.Types (QueryTx(..), QueryTxs(..), WHERE(..)
                               ,TxIdEq(..), TxGrade(..), QueryObj(..)
                               ,TxsFilterExpr(..), ObjHashEq(..))
import Disciplina.Educator.Txs (PrivateTxId(..), PrivateTx(..), EducatorTxMsg(..)
                               ,StudentTxMsg(..), PrivateTxPayload(..))
import Disciplina.Crypto (Hash, hash, PublicKey, SecretKey)
import Disciplina.Core (Address(..), CourseId(..), mkAddr, AssignmentId, StudentId)
import Disciplina.DB.DSL.Interpret (MonadSearchTxObj(..), RunQuery(..))
import qualified Disciplina.Core as Core (Grade(..), SubjectId(..))
import qualified Data.ByteString.Char8 as C
import Codec.Serialise (Serialise(..))


import Crypto.Hash.Algorithms (Blake2sp_256(..))
import Disciplina.Crypto.Hash.Cryptonite (CryptoniteFunc (..))

-- | Simple transaction database
newtype SimpleTxDB a = SimpleTxDB
   { getSimpleTxDB :: Reader [PrivateTx] a
   } deriving (Functor, Applicative, Monad, MonadReader [PrivateTx])

instance MonadSearchTxObj SimpleTxDB where
  runTxQuery = runSimpleTxQuery
  runTxsQuery = undefined
  runObjQuery = undefined

instance Serialise Address where
  encode (Address h) = encode h
  decode = Address <$> decode

-- | TODO, where should we put these?
instance Serialise Core.Grade
instance Serialise EducatorTxMsg
instance Serialise StudentTxMsg
instance Serialise PrivateTxPayload
instance Serialise CourseId
instance Serialise PrivateTx

-- | DSL interpreter for SimpleTXDB
runSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
runSimpleTxQuery (SELECTTx _ (TxIdEq (a :: PrivateTxId)))
  = do
     txs :: [PrivateTx] <- ask
     return (safeHead txs)

-- | query
-- given <tx_hash>, I want to be able to get { <tx> | hash(<tx>) = <tx_hash> }
foo :: PrivateTx -> SimpleTxDB (Maybe PrivateTx)
foo tx = runQuery q
  where q = SELECTTx WHERE (TxIdEq (hash tx))


-- | run query
runSimpleTxDB :: PrivateTx -> Maybe PrivateTx
runSimpleTxDB tx = runReader (getSimpleTxDB $ foo (mkPrivateTx 'a' 'k'))
                              privateTxs

tx1 = mkPrivateTx 'a' 'k'

privateTxs :: [PrivateTx]
privateTxs = fmap (uncurry mkPrivateTx) (zip ['a'..'j'] ['k'..'t'])

-- | create key pair from seed
mkKeyPair :: Char -> (PublicKey, SecretKey)
mkKeyPair seed =
  let (CryptoPassed x) = Ed25519.secretKey (C.replicate 32 seed)
  in (AbstractPK (Ed25519.toPublic x) :: PublicKey, AbstractSK x :: SecretKey)

type StudentKey = Char
type EducatorKey = Char

-- | create a simple private transaction
mkPrivateTx :: StudentKey -> EducatorKey -> PrivateTx
mkPrivateTx studentKey educatorKey = PrivateTx {
       _ptxStudentId = mkAddr . fst $ mkKeyPair studentKey
     , _ptxCourseId = courseId
     , _ptxEducatorId = mkAddr . fst $ mkKeyPair educatorKey
     , _ptxPayload = payload
     }
 where
   courseId = CourseId { ciSubject = 1
                       , ciId = 2
                       }
   payload = StudentTx { _ptxStudentMsg  = Enroll }
