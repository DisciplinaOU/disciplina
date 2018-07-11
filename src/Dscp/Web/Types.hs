
-- | Web-related types

module Dscp.Web.Types
       ( NetworkAddress (..)
       ) where

import Fmt (Buildable (..), (+|), (|+))
import qualified Text.Show

data NetworkAddress = NetworkAddress
    { naHost :: !Text
    , naPort :: !Word16
    } deriving (Eq, Ord, Generic)

instance Buildable NetworkAddress where
    build NetworkAddress {..} = ""+|naHost|+":"+|naPort|+""

instance Show NetworkAddress where
    show = toString . pretty
