
module Dscp.Core.Foundation.Educator.Certificate
    ( module M
    , Certificate (..)
    )
    where

import Dscp.Core.Foundation.Educator.Certificate.EducationForm as M
import Dscp.Core.Foundation.Educator.Certificate.Full as M
import Dscp.Core.Foundation.Educator.Certificate.Grade as M
import Dscp.Core.Foundation.Educator.Certificate.Info as M
import Dscp.Core.Foundation.Educator.Certificate.Language as M
import Dscp.Core.Foundation.Educator.Certificate.Meta as M
import Dscp.Crypto
import Dscp.Util

-- | Datatype which combines certificate meta with its ID.
data Certificate = Certificate
    { cId   :: Hash CertificateMeta
    , cMeta :: CertificateMeta
    } deriving (Show, Eq, Generic)

instance Buildable Certificate where
    build Certificate {..} =
        "{ id = "+|cId|+", meta = "+|cMeta|+" }"

