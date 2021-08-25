-- | Core swagger instances.
module Dscp.Web.Swagger.Instances.Core () where

import Control.Lens ((.=), (?=))
import Data.Swagger (ToParamSchema (..), ToSchema (..))
import qualified Data.Swagger as S
import qualified Data.Swagger.Internal.ParamSchema as S
import qualified Data.Swagger.Internal.Schema as S
import GHC.TypeLits (KnownSymbol)
import Servant.Util (ParamDescription, paramDescription)

import Dscp.Core
import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Aeson
import Dscp.Util.Constructors
import Dscp.Web.Swagger.Generation
import Dscp.Web.Types

----------------------------------------------------------------------------
-- Param descriptions
----------------------------------------------------------------------------

type instance ParamDescription ItemDesc =
    "Element description. '\\0' symbols are now allowed here."
type instance ParamDescription Timestamp =
    "Time in ISO format. \
    \Passed value is automatically rounded to microseconds precision."
type instance ParamDescription Address =
    "Disciplina address."
type instance ParamDescription (Hash ()) =
    "Hex-encoded blake2b hash value."
type instance ParamDescription (Hash Assignment) =
    "Hex-encoded blake2b hash value of assignment."
type instance ParamDescription (Hash Submission) =
    "Hex-encoded blake2b hash value of submission."
type instance ParamDescription (Hash PrivateBlockHeader) =
    "Hex-encoded blake2b hash value of private block."
type instance ParamDescription (Hash CertificateMeta) =
    "Hex-encoded blake2b hash value of certificate meta."
type instance ParamDescription Course =
    "Course ID."
type instance ParamDescription Subject =
    "Subject ID."
type instance ParamDescription Grade =
    "Grade for submission."
type instance ParamDescription (DocumentType Assignment) =
    "Type of assignment."
type instance ParamDescription (DocumentType Submission) =
    "Type of submission."
type instance ParamDescription SubmissionWitness =
    "Concatenated student public key and submission hash signed with \
    \student secret key."
type instance ParamDescription CertificateName =
    "Full certificate ID"

----------------------------------------------------------------------------
-- ToParamSchema instances
----------------------------------------------------------------------------

timestampFormat :: IsString s => s
timestampFormat = "yyyy-mm-ddThh:MM:ss.ffffffZ"

instance ToParamSchema Address where
    toParamSchema _ = mempty &: do
        S.type_ .= S.SwaggerString
        S.format ?= "base58"

instance ToParamSchema (Hash a) where
    toParamSchema _ = mempty &: do
        S.type_ .= S.SwaggerString
        S.format ?= "hex"

instance ToParamSchema Course where
    toParamSchema _ = idParamSchema

instance ToParamSchema Subject where
    toParamSchema _ = idParamSchema

instance ToParamSchema ItemDesc where
    toParamSchema _ = toParamSchema (Proxy @Text)

instance ToParamSchema Timestamp where
    toParamSchema _ = S.timeParamSchema timestampFormat

instance ToParamSchema (DocumentType a) where
    toParamSchema = gToParamSchema

----------------------------------------------------------------------------
-- ToSchema instances
----------------------------------------------------------------------------

instance ToSchema ItemDesc where
    declareNamedSchema p =
        declareSimpleSchema "Description" $ mempty &: do
            S.type_ .= S.SwaggerString
            S.description ?= paramDescription p

instance ToSchema Timestamp where
    declareNamedSchema p =
        declareSimpleSchema "Timestamp" $ S.timeSchema timestampFormat &: do
            S.description ?= paramDescription p
            setExample timestampEx

instance ToSchema Address where
    declareNamedSchema p =
        declareSimpleSchema "Address" $ S.byteSchema &: do
            S.description ?= paramDescription p
            setExample addressEx

instance KnownSymbol (ParamDescription (Hash a)) => ToSchema (Hash a) where
    declareNamedSchema p =
        declareSimpleSchema "Hash" $ S.byteSchema &: do
            S.description ?= paramDescription p
            setExample $ hash @Text "example"

instance {-# OVERLAPPING #-} ToSchema (Hash Raw) where
    declareNamedSchema _ = do
        inDeclaredSchema (declareNamedSchema (Proxy @(Hash ()))) $ do
             setExample offlineHash
             S.description ?= "Contents hash (e.g. contents of assignment is a problem \
                              \statement, contents of submission is a text of solution."

instance ToSchema Course where
    declareNamedSchema = idDeclareNamedSchema

instance ToSchema Subject where
    declareNamedSchema = idDeclareNamedSchema

instance ToSchema a => ToSchema (EncodeSerialised Base64Encoded a) where
    declareNamedSchema _ = declareNamedSchema (Proxy @a)

instance ToSchema Grade where
    declareNamedSchema p =
        declareSimpleSchema "Grade" $ mempty &: do
            S.type_ .= S.SwaggerInteger
            S.minimum_ ?= gradeToNum minBound
            S.maximum_ ?= gradeToNum maxBound
            S.description ?= paramDescription p

instance ToSchema SubmissionWitness where
    declareNamedSchema p =
        declareSimpleSchema "SubmissionWitness" $ S.byteSchema &: do
            S.description ?= paramDescription p

instance ToSchema (EmptyMerkleProof PrivateTx) where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToSchema PrivateTx where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance S.ToParamSchema CertificateName where
    toParamSchema _ = mempty &: do
        S.type_ .= S.SwaggerString
        S.format ?= "base64url"
        S.pattern ?= ".pdf$"

----------------------------------------------------------------------------
-- Error description
----------------------------------------------------------------------------

instance EnumHasDescription GeneralBackendError where
    enumDocDescription p = errorCaseDocDesc @UnsafeFiller p $ \case
        InvalidFormat{} -> "Invalid format of one of parameters."
        ServiceUnavailable{} -> "Number of incoming requests is exceeded."
