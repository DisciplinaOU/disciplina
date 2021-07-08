
module Dscp.Core.Foundation.Educator.DocumentType where

import Dscp.Crypto
import Dscp.Util

-- | Datatype to represent the notion of "offline"- and "online"-ness
-- of assignments and submissions.
data DocumentType a = Online | Offline
    deriving (Eq, Ord, Show, Enum, Generic)

instance Buildable (DocumentType a) where
    build = genericF

documentType :: Hash Raw -> DocumentType a
documentType h
    | h == offlineHash = Offline
    | otherwise        = Online

-- | A hash which indicates that a submission or an assignment
-- are offline.
-- TODO: make a more comprehensible and easily documentable value?...
offlineHash :: Hash Raw
offlineHash = unsafeHash ("offline" :: ByteString)
