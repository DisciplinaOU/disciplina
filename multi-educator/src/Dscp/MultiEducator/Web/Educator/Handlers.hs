-- | API handlers specific for multi educator.

module Dscp.MultiEducator.Web.Educator.Handlers
       ( certificatesApiHandlers
       ) where

import Loot.Base.HasLens (lensOf)

import Dscp.DB.SQL
import Dscp.Educator.Web.Educator
import Dscp.MultiEducator.Launcher.Mode
import Dscp.MultiEducator.Web.Educator.API
import Dscp.MultiEducator.Web.Educator.Types

certificatesApiHandlers
    :: forall m ctx. MultiEducatorWorkMode ctx m
    => CertificatesApiHandlers m
certificatesApiHandlers = CertificatesApiEndpoints
    { cGetCertificate = \(CertificateName eId cId) -> do
            db <- view (lensOf @SQL)
            setSchemaName db $ educatorSchemaName eId
            invoke $ educatorGetCertificate cId
    }
