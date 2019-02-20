
module Dscp.Core.Foundation.Educator.DocumentType where

import Dscp.Crypto
import Dscp.Util

-- | Datatype to represent the notion of "offline"- and "online"-ness
-- of assignments and submissions.
data DocumentType = Online | Offline
    deriving (Eq, Ord, Show, Enum, Generic)

instance Buildable DocumentType where
    build = genericF

documentType :: Hash Raw -> DocumentType
documentType h
    | h == offlineHash = Offline
    | otherwise        = Online
