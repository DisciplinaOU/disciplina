
module Dscp.Core.Foundation.Educator.Certificate.Full where

import Dscp.Core.Foundation.Educator.Certificate.Meta
import Dscp.Core.Foundation.Educator.Certificate.Grade
import Dscp.Util

-- | Datatype which contains all the info about certificate. This
-- datatype represents a request body for 'AddCertificate' endpoint.
data CertificateFullInfo = CertificateFullInfo
    { cfiMeta   :: CertificateMeta
    , cfiGrades :: [CertificateGrade]
    } deriving (Show, Eq, Generic)

instance Buildable CertificateFullInfo where
    build CertificateFullInfo {..} =
        "{ meta = "+|cfiMeta|+
        ", grades = "+|listF cfiGrades|+" }"
