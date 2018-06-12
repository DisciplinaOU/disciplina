module Disciplina.Educator.Launcher.Params where

import Disciplina.Launcher.Params (BasicNodeParams)

-- | Contains all initialization parameters of Educator node.
data EducatorParams = EducatorParams
    { epBasicParams :: !BasicNodeParams
    -- ^ Basic parameters for any node
    }
