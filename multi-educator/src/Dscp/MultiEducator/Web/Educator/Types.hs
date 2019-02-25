-- | API handlers specific for multi educator.

module Dscp.MultiEducator.Web.Educator.Types
       ( CertificateName (..)
       ) where

import Data.List (span)
import Fmt (build, (+|), (|+))
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import System.FilePath (splitExtension)

import Dscp.Core.Foundation
import Dscp.Util
import Dscp.Util.Test

data CertificateName = CertificateName
    { cnEducatorId    :: Text
    , cnCertificateId :: Id CertificateMeta
    } deriving (Show, Eq, Generic)

instance Buildable CertificateName where
    build (CertificateName eId cId) =
        "certificate { educator-id = "+|eId|+", hash = "+|build cId|+"}"

instance Arbitrary CertificateName where
    arbitrary = CertificateName
        <$> (toBase64Url @ByteString <$> arbitrary)
            -- avoiding having ':' in educator ID
        <*> arbitrary

instance ToHttpApiData CertificateName where
    toUrlPiece (CertificateName eId cId) =
        toBase64Url @ByteString (encodeUtf8 $ eId <> ":" <> toHex cId) <> ".pdf"

instance FromHttpApiData CertificateName where
    parseUrlPiece txt = do
        let (name', ext) = splitExtension $ toString txt
        when (ext /= ".pdf") $
            fail "Wrong extension, `.pdf` is expected"
        name <- decodeUtf8 <$> leftToFail (fromBase64Url @ByteString $ toText name')
        let (eId, cId'') = span (/= ':') name
            cId' = drop 1 cId''
        cId <- parseUrlPiece $ toText cId'
        return (CertificateName (toText eId) cId)
