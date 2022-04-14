{- |
  Only web instances for `PubAddress`
-}
module Dscp.MultiEducator.Types
       ( -- * Reexports
         PubAddress
       , pubAddrFromText
       ) where

import Universum

import Control.Lens ((?=))
import qualified Data.Swagger as S
import Servant (FromHttpApiData (..), ToHttpApiData (..))

import Dscp.Core.PubChain
import Dscp.Util
import Dscp.Web.Swagger

instance ToHttpApiData PubAddress where
    toUrlPiece = toText

instance FromHttpApiData PubAddress where
    parseUrlPiece = first toText . pubAddrFromText

type instance ParamDescription PubAddress = "Educator's Ethereum address."

instance S.ToParamSchema PubAddress where
    toParamSchema _ = mempty &: do
        S.type_ ?= S.SwaggerString
        S.format ?= "hex"
