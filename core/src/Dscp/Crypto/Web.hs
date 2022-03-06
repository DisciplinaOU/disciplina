-- | Instances for web.

module Dscp.Crypto.Web () where

import Universum
import Servant.API

import Dscp.Crypto.Impl
import Dscp.Util

instance FromHttpApiData (Hash a) where
    parseQueryParam = first toText . fromHex

instance ToHttpApiData (Hash a) where
    toQueryParam = toHex
