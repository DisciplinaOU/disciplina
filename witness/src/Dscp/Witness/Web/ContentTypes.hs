module Dscp.Witness.Web.ContentTypes
    ( PDF
    ) where

import Network.HTTP.Media.MediaType ((//))
import Pdf.Scanner (PDFBody (..))
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))

data PDF

instance Accept PDF where
    contentType _ = "application" // "pdf"
instance MimeRender PDF PDFBody where
    mimeRender _ = getPDFBody
instance MimeUnrender PDF PDFBody where
    mimeUnrender _ = Right . PDFBody
