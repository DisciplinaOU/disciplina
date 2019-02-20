
module Dscp.Core.Foundation.Educator.Certificate.Grade where

import Dscp.Core.Foundation.Educator.Certificate.Language
import Dscp.Core.Foundation.Educator.Grade
import Dscp.Core.Foundation.Educator.ItemDesc
import Dscp.Util

-- | Datatype which contains information about the grade which
-- gets included into the certificate.
data CertificateGrade = CertificateGrade
    { cgSubject :: ItemDesc
    , cgLang    :: Language
    , cgHours   :: Int
    , cgCredits :: Maybe Int
    , cgGrade   :: Grade
    } deriving (Show, Eq, Ord, Generic)

instance Buildable CertificateGrade where
    build CertificateGrade {..} =
        "{ subject = "+|cgSubject|+
        ", hours = "+|cgHours|+
        ", credits = "+|cgCredits|+
        ", grade = "+|cgGrade|+" }"

