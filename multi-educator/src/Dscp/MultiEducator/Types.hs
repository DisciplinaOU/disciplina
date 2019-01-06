module Dscp.MultiEducator.Types
    ( EducatorUUID (..)
    ) where

import Control.Lens ((.=))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Swagger as S
import Fmt (build)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Util (ParamDescription)

import Dscp.Util

-- | Educator id we receive from AAA service.
newtype EducatorUUID = EducatorUUID Text
    deriving (Show, Eq, Ord, IsString)

instance Buildable EducatorUUID where
    build (EducatorUUID eId) = "\"" <> build eId <> "\""

deriving instance ToJSON EducatorUUID
deriving instance FromJSON EducatorUUID

instance ToHttpApiData EducatorUUID where
    toUrlPiece (EducatorUUID eid) = eid

instance FromHttpApiData EducatorUUID where
    parseUrlPiece = pure . EducatorUUID

type instance ParamDescription EducatorUUID = "Educator identifier."

instance S.ToParamSchema EducatorUUID where
    toParamSchema _ = mempty &: do
        S.type_ .= S.SwaggerString
