module Dscp.MultiEducator.Types
    ( EducatorId (..)
    ) where

import Control.Lens ((.=))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Swagger as S
import Fmt (build)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

import Dscp.Util
import Dscp.Web.Swagger

-- | Educator id we receive from AAA service.
newtype EducatorId = EducatorId Text
    deriving (Show, Eq, Ord, IsString)

instance Buildable EducatorId where
    build (EducatorId eId) = "\"" <> build eId <> "\""

deriving instance ToJSON EducatorId
deriving instance FromJSON EducatorId

instance ToHttpApiData EducatorId where
    toUrlPiece (EducatorId eid) = eid

instance FromHttpApiData EducatorId where
    parseUrlPiece = pure . EducatorId

type instance ParamDescription EducatorId = "Educator identifier."

instance S.ToParamSchema EducatorId where
    toParamSchema _ = mempty &: do
        S.type_ .= S.SwaggerString
