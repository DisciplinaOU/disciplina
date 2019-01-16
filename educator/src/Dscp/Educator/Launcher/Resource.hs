{-# LANGUAGE OverloadedLabels #-}

-- | Resources used by Educator node

module Dscp.Educator.Launcher.Resource
       ( EducatorResources (..)
       , erWitnessResources
       ) where

import Control.Lens (makeLenses)

import Dscp.Config
import Dscp.DB.SQL (SQL)
import Dscp.Educator.Config
import Dscp.Educator.DB.Resource ()
import Dscp.Educator.Launcher.Marker (EducatorNode)
import Dscp.Resource.AppDir
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys (KeyResources (..), linkStore)
import Dscp.Resource.Network (NetServResources)
import Dscp.Util.HasLens
import qualified Dscp.Witness.Launcher.Resource as Witness

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data EducatorResources = EducatorResources
    { _erWitnessResources :: !Witness.WitnessResources
    , _erDB               :: !SQL
    , _erKeys             :: !(KeyResources EducatorNode)
    }

makeLenses ''EducatorResources
deriveHasLensDirect ''EducatorResources

deriveHasLens 'erWitnessResources ''EducatorResources ''Witness.WitnessResources
deriveHasLens 'erWitnessResources ''EducatorResources ''NetServResources

instance AllocResource (KeyResources EducatorNode) where
    type Deps (KeyResources EducatorNode) = (EducatorConfigRec, AppDir)
    allocResource (educatorCfg, appDir) =
        let baseParams = educatorCfg ^. sub #educator . sub #keys . sub #keyParams
        in buildComponentR "educator keys"
           (withCoreConfig (rcast educatorCfg) $
               linkStore baseParams appDir)
           (const pass)

instance AllocResource EducatorResources where
    type Deps EducatorResources = EducatorConfigRec
    allocResource educatorCfg = do
        let cfg = educatorCfg ^. sub #educator
            witnessCfg = rcast educatorCfg
        _erWitnessResources <- withWitnessConfig witnessCfg $
                               allocResource witnessCfg
        _erDB <- allocResource $ cfg ^. sub #db
        let appDir = Witness._wrAppDir _erWitnessResources
        _erKeys <- allocResource (educatorCfg, appDir)
        return EducatorResources {..}
