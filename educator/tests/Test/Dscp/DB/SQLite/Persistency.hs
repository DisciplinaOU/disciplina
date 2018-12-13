module Test.Dscp.DB.SQLite.Persistency where

import Loot.Base.HasLens (lensOf)

import Dscp.DB.SQLite
import Dscp.Educator.DB.Queries
import Dscp.Educator.DB.Resource
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Mode

spec_Database_persistency :: Spec
spec_Database_persistency = specWithTempPostgresServer $ do
    it "SQL database persistency" $ \testDb -> once . ioProperty $ do
        cid <- runPostgresMode testDb $ do
            prepareEducatorSchema =<< view (lensOf @SQL)
            transact $ createCourse nullCourse{ cdDesc = "Proper belts placing" }

        runPostgresMode testDb $ do
            prepareEducatorSchema =<< view (lensOf @SQL)
            invoke $ existsCourse cid
