{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Educator HTTP API definition.

module Dscp.MultiEducator.Web.Educator.API
    ( CertificatesApiEndpoints (..)
    , CertificatesApiHandlers
    , CertificatesAPI
    , FullCertificatesAPI
    , certificatesAPI
    , fullCertificatesAPI
    , ProtectedMultiEducatorAPI
    , MultiEducatorAPI
    , protectedMultiEducatorAPI
    , multiEducatorAPI
    , MultiStudentAPI
    , ProtectedMultiStudentAPI
    , protectedMultiStudentAPI
    , multiStudentAPI
    ) where

import Servant
import Servant.Generic
import Servant.Util (type ( #: ), ExceptionalResponses)

import Dscp.Core.Foundation.Educator
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Educator.API
import Dscp.Educator.Web.Educator.Error
import Dscp.Educator.Web.Student.API
import Dscp.MultiEducator.Types
import Dscp.MultiEducator.Web.Educator.Auth
import Dscp.Web.Swagger.UI
import Dscp.Witness.Web.ContentTypes

---------------------------------------------------------------------------
-- Certificates API
---------------------------------------------------------------------------

-- | Endpoints of public certificate API.
data CertificatesApiEndpoints route = CertificatesApiEndpoints
    { cGetCertificate :: route :- GetCertificatePublic
    } deriving (Generic)

type CertificatesApiHandlers m = CertificatesApiEndpoints (AsServerT m)

type CertificatesAPI =
    ToServant (CertificatesApiEndpoints AsApi)

type FullCertificatesAPI =
    "api" :> "certificates" :> "v1" :>
    WithSwaggerUI CertificatesAPI

-- | Endpoint for getting a certificate by full ID.
type GetCertificatePublic
    = "cert" :> Capture "certificate" CertificateName
    :> Summary "Get the certificate by ID"
    :> Description "Gets the PDF certificate with FairCV JSON included as metadata by ID. \
        \CertificateID is obtained as `base64url(\"<educator-UUID>:<certificate-hash>\")`, \
        \where `<educator-UUID>` is the UUID assigned by AAA microservice to Educator, \
        \and `<certificate-hash>` is a hash of certificate meta."
    :> ExceptionalResponses EducatorAPIError
       '[ 404 #: "Certificate with given ID not found."
        ]
    :> Get '[PDF] PDFBody

certificatesAPI :: Proxy CertificatesAPI
certificatesAPI = Proxy

fullCertificatesAPI :: Proxy FullCertificatesAPI
fullCertificatesAPI = Proxy

---------------------------------------------------------------------------
-- Educator API
---------------------------------------------------------------------------

type MultiEducatorAPI =
    "api" :> "educator" :> "v1" :>
    (WithSwaggerUI ProtectedMultiEducatorAPI)

type ProtectedMultiEducatorAPI =
    Auth' '[MultiEducatorAuth, NoAuth "multi-educator"] EducatorAuthLogin :> RawEducatorAPI

multiEducatorAPI :: Proxy MultiEducatorAPI
multiEducatorAPI = Proxy

protectedMultiEducatorAPI :: Proxy ProtectedMultiEducatorAPI
protectedMultiEducatorAPI = Proxy

---------------------------------------------------------------------------
-- Student API
---------------------------------------------------------------------------

type MultiStudentAPI =
    "api" :> "student" :> "v1" :>
    (WithSwaggerUI ProtectedMultiStudentAPI)

type ProtectedMultiStudentAPI =
    Capture "educator" EducatorUUID :> ProtectedStudentAPI

multiStudentAPI :: Proxy MultiStudentAPI
multiStudentAPI = Proxy

protectedMultiStudentAPI :: Proxy ProtectedMultiStudentAPI
protectedMultiStudentAPI = Proxy
