-- | Instances for web.

module Dscp.Crypto.Web () where

import Servant.API
import Servant.Util (SupportedFilters)

import Dscp.Crypto.Impl
import Dscp.Util

instance FromHttpApiData (Hash a) where
    parseQueryParam = first toText . fromHex

instance ToHttpApiData (Hash a) where
    toQueryParam = toHex

type instance SupportedFilters (Hash a) = SupportedFilters ByteString
