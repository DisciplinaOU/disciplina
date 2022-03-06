-- | Aeson instances for web-related types

module Dscp.Web.Aeson () where

import Universum

import Fmt (pretty)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)

import Dscp.Util (leftToFail)
import Dscp.Web.Metrics
import Dscp.Web.Types

instance ToJSON NetworkAddress where
    toJSON = String . pretty
instance FromJSON NetworkAddress where
    parseJSON = withText "NetworkAddress" $
        leftToFail . parseNetAddr . toString

instance ToJSON MetricsEndpoint where
    toJSON = toJSON . fmap endpointToAddr . unMetricsEndpoint
instance FromJSON MetricsEndpoint where
    parseJSON = fmap (MetricsEndpoint . fmap addrToEndpoint) . parseJSON
