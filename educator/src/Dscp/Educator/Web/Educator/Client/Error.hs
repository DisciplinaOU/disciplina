module Dscp.Educator.Web.Educator.Client.Error
       ( EducatorApiClientError (..)
       , servantToEducatorApiError
       ) where

import qualified Data.Text.Buildable as B
import Servant.Client (ServantError (..))
import qualified Text.Show

import Dscp.Educator.Web.Educator.Error
import Dscp.Web.Class

data EducatorApiClientError
    = EducatorApiClientError !EducatorAPIError
    | SomeClientError !Text

instance Show EducatorApiClientError where
    show = toString . pretty

instance Buildable EducatorApiClientError where
    build = \case
        EducatorApiClientError err -> B.build err
        SomeClientError msg -> B.build msg

instance Exception EducatorApiClientError

servantToEducatorApiError :: ServantError -> EducatorApiClientError
servantToEducatorApiError servantError = maybe
    (SomeClientError $ show servantError)
    EducatorApiClientError
    (fromServantError servantError)
