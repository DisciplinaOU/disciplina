
-- | Resources used by Educator node

module Disciplina.Educator.Launcher.Resource
       ( EducatorResources (..)
       ) where

import Universum

import Disciplina.Educator.Launcher.Params (EducatorParams (..))
import Disciplina.Educator.Mode (Educator, EducatorCustomContext (..))
import Disciplina.Launcher.Mode (FormNodeContext (..), NodeContext (..))
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

instance FormNodeContext EducatorResources (NodeContext Educator) where
    formNodeContext EducatorResources{..} =
        pure NodeContext
        { _ncLoggerName = bnrLoggerName erBasicResources
        , _ncCustomCtx = EducatorCustomContext
        }

