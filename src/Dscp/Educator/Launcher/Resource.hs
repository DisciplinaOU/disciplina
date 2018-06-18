
-- | Resources used by Educator node

module Dscp.Educator.Launcher.Resource
       ( EducatorResources (..)
       ) where

import Universum

import Dscp.DB.SQLite (SQLiteDB)
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Launcher.Resource (AllocResource (..))
import qualified Dscp.Witness.Launcher.Resource as Witness

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data EducatorResources = EducatorResources
    { erWitnessResources :: !Witness.WitnessResources
    , erSQLiteDB         :: !SQLiteDB
    }

instance AllocResource EducatorParams EducatorResources where
    allocResource EducatorParams{..} = do
        erWitnessResources <- allocResource epWitnessParams
        erSQLiteDB <- allocResource epSQLiteParams
        return EducatorResources {..}
