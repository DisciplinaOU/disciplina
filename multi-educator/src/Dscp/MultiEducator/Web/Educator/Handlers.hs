-- | API handlers specific for multi educator.

module Dscp.MultiEducator.Web.Educator.Handlers
       ( certificatesApiHandlers
       ) where

import Universum

import Dscp.Core.Foundation.Educator
import Dscp.DB.SQL
import Dscp.Educator.Web.Educator
import Dscp.Educator.Web.Logic
import Dscp.MultiEducator.Launcher.Educator
import Dscp.MultiEducator.Launcher.Mode
import Dscp.MultiEducator.Types
import Dscp.MultiEducator.Web.Educator.API

certificatesApiHandlers
    :: forall m ctx. MultiEducatorWorkMode ctx m
    => CertificatesApiHandlers m
certificatesApiHandlers = CertificatesApiEndpoints
    { cGetCertificate = \(CertificateName eId cId) -> invoke $ do
            -- TODO: something is wrong about that 'CertificateName' keeps 'Text'
            -- instead of 'EducatorUUID' inside, how to properly resolve this?
            setConnSchemaName $ educatorSchemaName (EducatorUUID eId)
            educatorGetCertificate cId
    , cCheckFairCV = pure . checkFairCV
    , cCheckFairCVPDF = checkFairCVPDF
    }
