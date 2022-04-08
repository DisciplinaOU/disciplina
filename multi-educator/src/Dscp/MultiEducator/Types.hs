module Dscp.MultiEducator.Types
    ( EducatorEthAddress (..)
    ) where

import Universum

import Control.Lens ((?=))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Swagger as S
import Fmt (Buildable (..))
import Servant (FromHttpApiData (..), ToHttpApiData (..))

import Dscp.Util
import Dscp.Web.Swagger

-- | Educator id we receive from AAA service.
newtype EducatorEthAddress = EducatorEthAddress Text
    deriving (Show, Eq, Ord, IsString)

instance Buildable EducatorEthAddress where
    build (EducatorEthAddress eId) = "\"" <> build eId <> "\""

deriving instance ToJSON EducatorEthAddress
deriving instance FromJSON EducatorEthAddress

instance ToHttpApiData EducatorEthAddress where
    toUrlPiece (EducatorEthAddress eid) = eid

instance FromHttpApiData EducatorEthAddress where
    parseUrlPiece = pure . EducatorEthAddress

type instance ParamDescription EducatorEthAddress = "Educator's Ethereum address."

instance S.ToParamSchema EducatorEthAddress where
    toParamSchema _ = mempty &: do
        S.type_ ?= S.SwaggerString
