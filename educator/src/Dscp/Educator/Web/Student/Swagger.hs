-- | Swagger documentation.
module Dscp.Educator.Web.Student.Swagger
       ( studentAPISwagger
       , writeStudentAPISwagger
       ) where

import Control.Lens (at, zoom, (.=), (?=))
import qualified Data.ByteString.Lazy as LBS
import Data.Swagger (Scheme (..), Swagger, ToParamSchema (..), ToSchema (..))
import qualified Data.Swagger as S
import qualified Data.Swagger.Internal.Schema as S

import Dscp.Core
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Error
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Util.Constructors
import Dscp.Web.Swagger

studentAPISwagger :: Swagger
studentAPISwagger = toAwesomeSwagger protectedStudentAPI &: do
    setSerokellDocMeta
    zoom S.info $ do
        S.version .= "1.0.0"

    S.host ?= "localhost:8090"
    S.basePath ?= "/api/educator/v1"
    S.schemes ?= [Http, Https]

writeStudentAPISwagger :: FilePath -> IO ()
writeStudentAPISwagger file = liftIO $ LBS.writeFile file (encodeSwagger studentAPISwagger)

----------------------------------------------------------------------------
-- Param descriptions
----------------------------------------------------------------------------

type instance ParamDescription IsEnrolled =
    "If set to `true`, show only courses in which student is currently \
    \enrolled, if set to `false` - show only available courses, otherwise \
    \should all of them."
type instance ParamDescription IsFinal =
    "Select only final/non-final assignments."

----------------------------------------------------------------------------
-- ToParamSchema instances
----------------------------------------------------------------------------

instance ToParamSchema IsEnrolled where
    toParamSchema = gToParamSchema
instance ToParamSchema IsFinal where
    toParamSchema = gToParamSchema
instance ToParamSchema IsGraded where
    toParamSchema = gToParamSchema

instance ToSchema IsFinal where
    declareNamedSchema = newtypeDeclareNamedSchema @Bool

instance ToSchema NewSubmission where
    declareNamedSchema p =
        inDeclaredSchema (gDeclareNamedSchema p) $
            setExample $ signedSubmissionToRequest signedSubmissionEx

instance ToSchema CourseStudentInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema AssignmentStudentInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema SubmissionStudentInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema GradeInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema BlkProofInfo where
    declareNamedSchema = gDeclareNamedSchema

instance EnumHasDescription WrongSubmissionSignature where
    enumDocDescription p = errorCaseDocDesc @UnsafeFiller p $ \case
        SubmissionSignatureInvalid{} ->
            "Signature does not match to claimed submission author."
        FakeSubmissionSignature{} ->
            "Claimed owner of submission does not match to authenticated user."

instance EnumHasDescription StudentAPIError where
    enumDocDescription = gEnumDocDesc $ \case
        BadSubmissionSignature err -> enumDocDescription (proxyOf err)
        SomeDomainError err -> enumDocDescription (proxyOf err)
        SomeGeneralBackendError err -> enumDocDescription (proxyOf err)

instance ToSchema StudentAPIError where
    declareNamedSchema _ = return . S.named "ErrResponse" $ mempty &: do
        S.type_ .= S.SwaggerObject
        S.required .= ["error"]
        zoom S.properties $ do
            at "error" ?= errSchema
        S.description ?= "Describes a respose error."
      where
        errSchema = S.toSchemaRef (Proxy @String) &: do
            zoom _Inline $ do
                setDocEnumDescription @StudentAPIError
