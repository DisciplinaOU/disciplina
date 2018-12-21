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

deriving instance ToHttpApiData Course
deriving instance FromHttpApiData Course

instance ToHttpApiData DocumentType where
    toQueryParam Offline = "offline"
    toQueryParam Online  = "online"

instance FromHttpApiData DocumentType where
    parseQueryParam "offline" = Right Offline
    parseQueryParam "online"  = Right Online
    parseQueryParam other     = Left $ "invalid document type: " <> other

deriving instance ToHttpApiData GTxId
deriving instance FromHttpApiData GTxId
