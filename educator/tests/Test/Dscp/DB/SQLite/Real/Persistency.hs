module Test.Dscp.DB.SQLite.Real.Persistency where

import Loot.Base.HasLens (lensOf)

import Dscp.DB.SQLite
import Dscp.Educator.DB.Queries
import Dscp.Educator.DB.Resource
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Real.Mode

spec_SQLiteWrapper :: Spec
spec_SQLiteWrapper =
    it "SQL database persistency" . once . ioProperty $ do
        withTempPostgres $ \testDb -> do
            cid <- runPostgresMode testDb $ do
                prepareEducatorSchema =<< view (lensOf @SQL)
                transact $ createCourse nullCourse{ cdDesc = "Proper belts placing" }

            runPostgresMode testDb $ do
                prepareEducatorSchema =<< view (lensOf @SQL)
                invoke $ existsCourse cid
