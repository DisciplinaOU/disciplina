
-- | Resources used by Educator node

module Disciplina.Educator.Launcher.Resource
       ( EducatorResources (..)
       ) where

import Universum

import Disciplina.Educator.Launcher.Params (EducatorParams (..))
import Disciplina.Launcher.Resource (BasicNodeResources (..), BracketResource (..))

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data EducatorResources = EducatorResources
    { erBasicResources :: !BasicNodeResources
    }

instance BracketResource EducatorParams EducatorResources where
    bracketResourceC EducatorParams{..} = do
        erBasicResources <- bracketResourceC epBasicParams
        return EducatorResources {..}

