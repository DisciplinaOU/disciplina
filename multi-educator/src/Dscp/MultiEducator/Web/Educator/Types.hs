-- | API handlers specific for multi educator.

module Dscp.MultiEducator.Web.Educator.Types
       ( CertificateName (..)
       ) where

import Data.List (span)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import System.FilePath (splitExtension)

import Dscp.Core.Foundation
import Dscp.Util

data CertificateName = CertificateName
    { cnEducatorId    :: Text
    , cnCertificateId :: Id CertificateMeta
    } deriving (Show, Eq, Generic)

instance ToHttpApiData CertificateName where
    toUrlPiece (CertificateName eId cId) =
        toBase64Url @ByteString (encodeUtf8 $ eId <> ":" <> toHex cId) <> ".pdf"

instance FromHttpApiData CertificateName where
    parseUrlPiece txt = do
        let (name, ext) = splitExtension $ toString txt
        when (ext /= ".pdf") $
            fail "Wrong extension, `.pdf` is expected"
        let (eId, cId'') = span (/= ':') name
            cId' = drop 1 cId''
        cId <- parseUrlPiece $ toText cId'
        return (CertificateName (toText eId) cId)
