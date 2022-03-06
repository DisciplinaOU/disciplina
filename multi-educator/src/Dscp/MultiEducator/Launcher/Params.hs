module Dscp.MultiEducator.Launcher.Params
       ( MultiEducatorKeyParams (..)
       , MultiEducatorAAAConfig
       , MultiEducatorAAAConfigRec
       , MultiEducatorAAAConfigRecP
       ) where

import Data.Aeson (FromJSON (..))
import Loot.Config (Config, PartialConfig, (:::))
import Servant.Client.Core (BaseUrl)
import Universum

import Dscp.MultiEducator.Web.Educator.Auth (MultiEducatorPublicKey)

-- | Educator key parameters.
newtype MultiEducatorKeyParams = MultiEducatorKeyParams
    { unMultiEducatorKeyParams :: FilePath
      -- ^ Path to educator key folder.
    } deriving (Eq, Show, FromJSON)

-- | AAA microservice configuration
type MultiEducatorAAAConfig =
   '[ "serviceUrl" ::: BaseUrl
      -- URL of the microservice
    , "publicKey" ::: MultiEducatorPublicKey
      -- Public key, used for JWT validation
    ]

type MultiEducatorAAAConfigRec = Config MultiEducatorAAAConfig
type MultiEducatorAAAConfigRecP = PartialConfig MultiEducatorAAAConfig
