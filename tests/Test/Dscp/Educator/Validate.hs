module Test.Dscp.Educator.Validate where

import Test.Common

--import Dscp.Core (CourseId (..), Grade (..), SubjectId, mkAddr)
--import Dscp.DB (Obj, ObjHashEq (..), QueryObj (..), QueryTx (..), QueryTxs (..), TxGrade (..),
--                      TxIdEq (..), TxsFilterExpr (..), WHERE (..), runSimpleTxDBQuery)
--import Dscp.Educator (EducatorTxMsg (..), PrivateTx (..), PrivateTxPayload (..),
--                            StudentTxMsg (..))
import Dscp.Educator (validate)
import Dscp.Educator.Block (PrivateBlock, pbBody, pbHeader, pbbTxs, pbhBodyProof)



spec_Transactions :: Spec
spec_Transactions = describe "Validate private transaction" $ do
    it "can validate transaction" $ do
        True `shouldBe` True
