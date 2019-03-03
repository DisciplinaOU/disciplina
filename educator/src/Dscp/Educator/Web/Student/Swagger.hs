-- | Swagger documentation.
module Dscp.Educator.Web.Student.Swagger
       ( studentAPISwagger
       , writeStudentAPISwagger
       ) where

import Control.Lens (zoom, (.=), (?=))
import qualified Data.ByteString.Lazy as LBS
import Data.Swagger (Scheme (..), Swagger)
import qualified Data.Swagger as S

import Dscp.Educator.Web.Student.API
import Dscp.Util
import Dscp.Web.Swagger

-- | Swagger documentation for Student API.
studentAPISwagger :: Swagger
studentAPISwagger = toAwesomeSwagger protectedStudentAPI &: do
    setSerokellDocMeta

    zoom S.info $ do
        S.title .= "Disciplina Student API"
        S.version .= "1.0.0"

    S.host ?= "localhost:8090"
    S.schemes ?= [Http, Https]

-- | Write documentation to file, for testing purposes.
-- Normally you can use "dscp-swagger" executable to build documentation.
writeStudentAPISwagger :: FilePath -> IO ()
writeStudentAPISwagger file =
    liftIO $ LBS.writeFile file (encodeSwagger studentAPISwagger)
