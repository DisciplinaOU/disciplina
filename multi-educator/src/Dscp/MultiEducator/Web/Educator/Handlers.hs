-- | API handlers specific for multi educator.

module Dscp.MultiEducator.Web.Educator.Handlers
       ( certificatesApiHandlers
       ) where

import Dscp.Core.Foundation.Educator
import Dscp.DB.SQL
import Dscp.Educator.Web.Educator
import Dscp.MultiEducator.Launcher.Mode
import Dscp.MultiEducator.Web.Educator.API

certificatesApiHandlers
    :: forall m ctx. MultiEducatorWorkMode ctx m
    => CertificatesApiHandlers m
certificatesApiHandlers = CertificatesApiEndpoints
    { cGetCertificate = \(CertificateName eId cId) -> invoke $ do
            setConnSchemaName $ educatorSchemaName eId
            educatorGetCertificate cId
    }
