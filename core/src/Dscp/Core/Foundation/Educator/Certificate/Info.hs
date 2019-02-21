
module Dscp.Core.Foundation.Educator.Certificate.Info where

import Dscp.Core.Foundation.Educator.ItemDesc

-- | Datatype containing information about Educator which issued
-- the certificate, required in order to render a certificate.
data CertificateIssuerInfo = CertificateIssuerInfo
    { ciiName :: ItemDesc
    , ciiUrl  :: ItemDesc
    } deriving (Show, Eq, Generic)

