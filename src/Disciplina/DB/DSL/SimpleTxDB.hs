module Disciplina.DB.DSL.SimpleTxDB () where

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
import Control.Lens (filtered, traversed)


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

-- | interpreter for SELECTTx WHERE TxIdEq
runSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
runSimpleTxQuery (SELECTTx _ (TxIdEq (h :: PrivateTxId)))
  = do
     -- find Tx in db with hash h
     txs <- ask
     return $ txs ^? traversed . filtered ((==h).hash)

-- | query
-- given <tx_hash>, I want to be able to get { <tx> | hash(<tx>) = <tx_hash> }
findTx :: PrivateTx -> SimpleTxDB (Maybe PrivateTx)
findTx tx = runQuery q
  where q = SELECTTx WHERE (TxIdEq (hash tx))

-- | run query
runFindTx :: PrivateTx -> Maybe PrivateTx
runFindTx tx = runReader (getSimpleTxDB $ findTx tx) simpleTxDB

-- | Should return Nothing since mkPrivateTx 'a' 'l'
-- | does not exist in privateTx db
testFindTx1 :: Maybe PrivateTx
testFindTx1 = runFindTx $ mkPrivateTx 'a' 'l'

-- | Should return Just (mkPrivate 'a' 'k')
testFindTx2 :: Maybe PrivateTx
testFindTx2 = runFindTx $ mkPrivateTx 'a' 'k'


simpleTxDB :: [PrivateTx]
simpleTxDB = fmap (uncurry mkPrivateTx) (zip ['a'..'j'] ['k'..'t'])

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

-- | create key pair from seed
mkKeyPair :: Char -> (PublicKey, SecretKey)
mkKeyPair seed =
  let (CryptoPassed x) = Ed25519.secretKey (C.replicate 32 seed)
  in (AbstractPK (Ed25519.toPublic x) :: PublicKey, AbstractSK x :: SecretKey)

