module Test.Dscp.Educator.Other where

import Dscp.DB.SQLite
import Dscp.Util.Test

spec_Operations_with_database_connection :: Spec
spec_Operations_with_database_connection = do
    describe "connStringDatabaseL" $ do
        it "Can view" $
             let connStr = "postgresql:///my-db?host=/tmp/postgres&port=49691"
             in connStr ^. connStringDatabaseL === "my-db"

        it "Can modify" $
             let origin = "postgresql:///my-db?host=/tmp/postgres&port=49691"
                 expected = "postgresql:///my-db1?host=/tmp/postgres&port=49691"
             in (connStringDatabaseL %~ (<> "1")) origin === expected

        it "Empty is fine" $
             let connStr = "postgresql://?host=/tmp/postgres&port=49691"
             in connStr ^. connStringDatabaseL === ""

        it "Host is properly skipped" $
             let connStr = "postgresql://abab:123/nyan?host=/tmp/postgres&port=49691"
             in connStr ^. connStringDatabaseL === "nyan"
