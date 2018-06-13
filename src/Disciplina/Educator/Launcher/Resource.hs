
-- | Resources used by Educator node

module Disciplina.Educator.Launcher.Resource
       ( EducatorResources (..)
       ) where

import Universum

import Disciplina.Educator.Launcher.Params (EducatorParams (..))
import Disciplina.Launcher.Resource (AllocResource (..), BasicNodeResources (..))

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data EducatorResources = EducatorResources
    { erBasicResources :: !BasicNodeResources
    }

instance AllocResource EducatorParams EducatorResources where
    allocResource EducatorParams{..} = do
        erBasicResources <- allocResource epBasicParams
        return EducatorResources {..}

