-- | Instances and reexports for web.

module Dscp.Core.Web
       ( BaseUrl (..)
       , parseBaseUrl
       , showBaseUrl

       , FromHttpApiData (..)
       , ToHttpApiData (..)
       ) where

import Universum
import Data.List (span)
import Servant.API
import Servant.Client.Core (BaseUrl (..), parseBaseUrl, showBaseUrl)
import System.FilePath (splitExtension)

import Dscp.Core.PubChain
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

instance ToHttpApiData PubAddress where
    toUrlPiece = toText
instance FromHttpApiData PubAddress where
    parseUrlPiece = first toText . pubAddrFromText

deriving instance ToHttpApiData Entity
deriving instance FromHttpApiData Entity

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
    toUrlPiece (CertificateName eAddr cId) =
        toBase64Url @ByteString (encodeUtf8 $ toText eAddr <> ":" <> toHex cId) <> ".pdf"

instance FromHttpApiData CertificateName where
    parseUrlPiece txt = do
        let (name', ext) = splitExtension $ toString txt
        when (ext /= ".pdf") $
            Left "Wrong extension, `.pdf` is expected"
        name <- bimap toText decodeUtf8 $ fromBase64Url @ByteString $ toText name'
        let (eAddr', cId'') = span (/= ':') name
            cId' = drop 1 cId''
        cId <- parseUrlPiece $ toText cId'
        eAddr <- parseUrlPiece $ toText eAddr'
        return $ CertificateName eAddr cId
