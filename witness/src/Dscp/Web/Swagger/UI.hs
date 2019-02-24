-- | Serving swagger UI.
module Dscp.Web.Swagger.UI
    ( SwaggerUI
    , WithSwaggerUI
    , withSwaggerUI
    ) where

import Control.Lens ((.=), (?=))
import qualified Data.Swagger as S
import qualified Data.Swagger.Internal.Schema as S
import Servant ((:<|>) (..), (:>), Server)
import Servant.Swagger.UI (SwaggerSchemaUI, SwaggerUiHtml, swaggerSchemaUIServer)
import Servant.Util (Tag)

import Dscp.Util

-- | Swagger UI we use across the project.
type SwaggerUI =
    Tag "Documentation" :>
    SwaggerSchemaUI "docs" "swagger.json"

-- | Attach a swagger UI to the given API.
type WithSwaggerUI api = api :<|> SwaggerUI

-- | Attach a UI serving given documentation to the given server.
withSwaggerUI :: S.Swagger -> Proxy api -> Server api -> Server (WithSwaggerUI api)
withSwaggerUI swagger _ server = server :<|> swaggerSchemaUIServer swagger

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance S.ToSchema (SwaggerUiHtml dir api) where
    declareNamedSchema _ =
        S.plain $ mempty &: do
            S.type_ .= S.SwaggerNull
            S.title ?= "Swagger UI page"

instance S.ToSchema S.Swagger where
    declareNamedSchema _ =
        S.plain $ mempty &: do
            S.type_ .= S.SwaggerObject
            S.title ?= "Swagger specification"
            S.description ?= "The specification you are currently reading."
