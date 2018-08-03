-- | Instances for web.

module Dscp.Core.Web () where

import Servant.API

import Dscp.Core.Foundation

---------------------------------------------------------------------------
-- FromHttpApiData/ToHttpApiData instances
---------------------------------------------------------------------------

instance ToHttpApiData Address where
    toUrlPiece = addrToText
instance FromHttpApiData Address where
    parseUrlPiece = addrFromText

deriving instance FromHttpApiData Course

instance FromHttpApiData DocumentType where
    parseQueryParam "offline" = Right Offline
    parseQueryParam "online"  = Right Online
    parseQueryParam other     = Left $ "invalid document type: " <> other
