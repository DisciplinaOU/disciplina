-- | Instances and reexports for web.

module Dscp.Core.Web
       ( BaseUrl (..)
       , parseBaseUrl
       , showBaseUrl

       , FromHttpApiData (..)
       , ToHttpApiData (..)
       ) where

import Data.List (span)
import Servant.API
import Servant.Client.Core (BaseUrl (..), parseBaseUrl, showBaseUrl)
import System.FilePath (splitExtension)

import Dscp.Core.Foundation
import Dscp.Crypto.Web ()
import Dscp.Util

---------------------------------------------------------------------------
-- FromHttpApiData/ToHttpApiData instances
---------------------------------------------------------------------------

instance ToHttpApiData Address where
    toUrlPiece = addrToText
instance FromHttpApiData Address where
    parseUrlPiece = addrFromText

deriving instance ToHttpApiData Course
deriving instance FromHttpApiData Course

instance ToHttpApiData (DocumentType a) where
    toQueryParam Offline = "offline"
    toQueryParam Online  = "online"

instance FromHttpApiData (DocumentType a) where
    parseQueryParam "offline" = Right Offline
    parseQueryParam "online"  = Right Online
    parseQueryParam other     = Left $ "invalid document type: " <> other

deriving instance ToHttpApiData GTxId
deriving instance FromHttpApiData GTxId

deriving instance ToHttpApiData Timestamp
instance FromHttpApiData Timestamp where
    parseQueryParam t =
        -- Rounding here since Postgres rounds all timestamps it receives anyway
        toTimestamp <$> parseQueryParam t

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
