-- | Swagger documentation.
module Dscp.Educator.Web.Student.Swagger
       ( studentAPISwagger
       , writeStudentAPISwagger
       ) where

import Universum
import Control.Lens (zoom, (.=), (?=))
import qualified Data.ByteString.Lazy as LBS
import Data.Swagger (Scheme (..), Swagger)
import qualified Data.Swagger as S
import Data.Tagged (Tagged (..), untag)

import Dscp.Educator.Web.Student.API
import Dscp.Util
import Dscp.Web.Swagger
import Dscp.Web.Types

-- | Swagger documentation for Student API.
studentAPISwagger :: Maybe NetworkAddress -> Tagged ProtectedStudentAPI Swagger
studentAPISwagger mhost = Tagged $ toAwesomeSwagger fullStudentAPI &: do
    setSerokellDocMeta

    zoom S.info $ do
        S.title .= "Disciplina Student API"
        S.version .= "1.0.0"

    S.host .= fmap networkAddressToSwaggerHost mhost
    S.schemes ?= [Http, Https]

-- | Write documentation to file, for testing purposes.
-- Normally you can use "dscp-swagger" executable to build documentation.
writeStudentAPISwagger :: FilePath -> IO ()
writeStudentAPISwagger file =
    liftIO $ LBS.writeFile file (encodeSwagger . untag $ studentAPISwagger demoAddr)
  where
    demoAddr = Just $ NetworkAddress "localhost" 8090
