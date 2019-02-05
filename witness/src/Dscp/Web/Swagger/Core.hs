-- | Core swagger instances.
module Dscp.Web.Swagger.Core () where

import Control.Lens ((.=), (?=))
import Data.Swagger (ToParamSchema (..), ToSchema (..))
import qualified Data.Swagger as S
import qualified Data.Swagger.Internal.ParamSchema as S
import qualified Data.Swagger.Internal.Schema as S

import Dscp.Core
import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Aeson
import Dscp.Util.Constructors
import Dscp.Web.Swagger.Util
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
type instance ParamDescription (Hash a) =
    "Hex-encoded blake2b hash value."
type instance ParamDescription Course =
    "Course ID."
type instance ParamDescription Subject =
    "Subject ID."
type instance ParamDescription Grade =
    "Grade for submission."
type instance ParamDescription DocumentType =
    "Type of assignment or submission."
type instance ParamDescription SubmissionWitness =
    "Concatenated student public key and submission hash signed with \
    \student secret key."

----------------------------------------------------------------------------
-- ToParamSchema instances
----------------------------------------------------------------------------

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

instance ToParamSchema Timestamp where
    toParamSchema _ = S.timeParamSchema timestampFormat

instance ToParamSchema DocumentType where
    toParamSchema = gToParamSchema

----------------------------------------------------------------------------
-- ToSchema instances
----------------------------------------------------------------------------

instance ToSchema ItemDesc where
    declareNamedSchema p =
        declareSimpleSchema "Description" $ mempty &: do
            S.type_ .= S.SwaggerString
            setParamDescription p

instance ToSchema Timestamp where
    declareNamedSchema p =
        declareSimpleSchema "Timestamp" $ S.timeSchema timestampFormat &: do
            setParamDescription p
            setExample timestampEx

instance ToSchema Address where
    declareNamedSchema p =
        declareSimpleSchema "Address" $ S.byteSchema &: do
            setParamDescription p
            setExample addressEx

instance ToSchema (Hash a) where
    declareNamedSchema p =
        declareSimpleSchema "Hash" $ S.byteSchema &: do
            setParamDescription p
            setExample $ hash @Text "example"

instance {-# OVERLAPPING #-} ToSchema (Hash Raw) where
    declareNamedSchema _ = do
        inDeclaredSchema (declareNamedSchema (Proxy @(Hash ()))) $ do
             setExample offlineHash

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
            setParamDescription p

instance ToSchema SubmissionWitness where
    declareNamedSchema p =
        declareSimpleSchema "SubmissionWitness" $ S.byteSchema &: do
            setParamDescription p

instance ToSchema (EmptyMerkleProof PrivateTx) where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToSchema PrivateTx where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

----------------------------------------------------------------------------
-- Error description
----------------------------------------------------------------------------

instance EnumHasDescription GeneralBackendError where
    enumDocDescription p = errorCaseDocDesc @UnsafeFiller p $ \case
        InvalidFormat{} -> "Invalid format of one of parameters."
        ServiceUnavailable{} -> "Number of incoming requests is exceeded."
