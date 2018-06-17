
-- | Resources used by Educator node

module Disciplina.Educator.Launcher.Resource
       ( EducatorResources (..)
       ) where

import Universum

import Disciplina.Educator.Launcher.Params (EducatorParams (..))
import Disciplina.Launcher.Resource (AllocResource (..))
import qualified Disciplina.Witness.Launcher.Resource as Witness

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data EducatorResources = EducatorResources
    { erWitnessResources :: !Witness.WitnessResources
    }

instance AllocResource EducatorParams EducatorResources where
    allocResource EducatorParams{..} = do
        erWitnessResources <- allocResource epWitnessParams
        return EducatorResources {..}

