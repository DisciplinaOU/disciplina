-- | Swagger documentation of multi-educator APIs.
module Dscp.MultiEducator.Web.Swagger
    ( multiStudentAPISwagger
    , multiEducatorAPISwagger
    , certificatesAPISwagger
    ) where

import Universum

import Control.Lens (zoom, (.=), (?=))
import Data.Swagger (Scheme (..), Swagger)
import qualified Data.Swagger as S
import Data.Tagged (Tagged (..))

import Dscp.MultiEducator.Web.Educator.API
import Dscp.Util
import Dscp.Web.Swagger
import Dscp.Web.Types

-- | Swagger documentation for Student API of multi-educator.
multiEducatorAPISwagger
    :: Maybe NetworkAddress
    -> Tagged ProtectedMultiEducatorAPI Swagger
multiEducatorAPISwagger mhost = Tagged $ toAwesomeSwagger multiEducatorAPI &: do
    setSerokellDocMeta

    zoom S.info $ do
        S.title .= "Disciplina Multi-educator API"
        S.version .= "1.0.0"

    S.host .= fmap networkAddressToSwaggerHost mhost
    S.schemes ?= [Http, Https]

-- | Swagger documentation for Student API of multi-educator.
multiStudentAPISwagger
    :: Maybe NetworkAddress
    -> Tagged ProtectedMultiStudentAPI Swagger
multiStudentAPISwagger mhost = Tagged $ toAwesomeSwagger multiStudentAPI &: do
    setSerokellDocMeta

    zoom S.info $ do
        S.title .= "Disciplina Multi-educator Student API"
        S.version .= "1.0.0"

    S.host .= fmap networkAddressToSwaggerHost mhost
    S.schemes ?= [Http, Https]

-- | Swagger documentation for Student API of multi-educator.
certificatesAPISwagger
    :: Maybe NetworkAddress
    -> Tagged CertificatesAPI Swagger
certificatesAPISwagger mhost = Tagged $ toAwesomeSwagger fullCertificatesAPI &: do
    setSerokellDocMeta

    zoom S.info $ do
        S.title .= "Disciplina Certificates API"
        S.version .= "1.0.0"

    S.host .= fmap networkAddressToSwaggerHost mhost
    S.schemes ?= [Http, Https]
