
module Dscp.Core.Foundation.Educator.Certificate
    ( module M
    , Certificate (..)
    )
    where

import qualified Dscp.Core.Foundation.Educator.Certificate.EducationForm as M
import qualified Dscp.Core.Foundation.Educator.Certificate.Full as M
import qualified Dscp.Core.Foundation.Educator.Certificate.Grade as M
import qualified Dscp.Core.Foundation.Educator.Certificate.Language as M
import qualified Dscp.Core.Foundation.Educator.Certificate.Meta as M

-- | Datatype which combines certificate meta with its ID.
data Certificate = Certificate
    { cId   :: Hash CertificateMeta
    , cMeta :: CertificateMeta
    } deriving (Show, Eq, Generic)

