module Test.Dscp.DB.SQLite.Real.Persistency where

import Loot.Base.HasLens (lensOf)
import System.IO.Temp (withSystemTempFile)

import Dscp.DB.SQLite
import Dscp.Educator.DB.Queries
import Dscp.Educator.DB.Resource
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Real.Mode

spec_SQLiteWrapper :: Spec
spec_SQLiteWrapper =
    it "SQL database persistency" . once . ioProperty $ do
        withSystemTempFile "persistency-test-db-XXX.sql" $ \dbFile _hdl -> do
            cid <- runSQLiteMode dbFile $ do
                prepareEducatorSchema =<< view (lensOf @SQLiteDB)
                transactW $ createCourse nullCourse{ cdDesc = "Proper belts placing" }

            runSQLiteMode dbFile $ do
                prepareEducatorSchema =<< view (lensOf @SQLiteDB)
                invoke $ existsCourse cid
