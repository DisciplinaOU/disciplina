-- | Swagger documentation.
module Dscp.Educator.Web.Educator.Swagger
       ( educatorAPISwagger
       , writeEducatorAPISwagger
       ) where

import Control.Lens (zoom, (.=), (?=))
import qualified Data.ByteString.Lazy as LBS
import Data.Swagger (Scheme (..), Swagger)
import qualified Data.Swagger as S

import Dscp.Educator.Web.Educator.API
import Dscp.Util
import Dscp.Web.Swagger

-- | Swagger documentation for Educator API.
educatorAPISwagger :: Swagger
educatorAPISwagger = toAwesomeSwagger protectedEducatorAPI &: do
    setSerokellDocMeta

    zoom S.info $ do
        S.title .= "Disciplina Educator API"
        S.version .= "1.0.0"

    S.host ?= "localhost:8090"
    S.schemes ?= [Http, Https]

-- | Write documentation to file, for testing purposes.
-- Normally you can use "dscp-swagger" executable to build documentation.
writeEducatorAPISwagger :: FilePath -> IO ()
writeEducatorAPISwagger file =
    liftIO $ LBS.writeFile file (encodeSwagger educatorAPISwagger)
