module Dscp.Educator.Web.Student.Client.Error
       ( StudentApiClientError (..)
       , servantToStudentApiError
       ) where

import qualified Data.Text.Buildable as B
import Servant.Client (ServantError (..))
import qualified Text.Show

import Dscp.Educator.Web.Student.Error
import Dscp.Web.Class

data StudentApiClientError
    = StudentApiClientError !StudentAPIError
    | SomeClientError !Text

instance Show StudentApiClientError where
    show = toString . pretty

instance Buildable StudentApiClientError where
    build = \case
        StudentApiClientError err -> B.build err
        SomeClientError msg -> B.build msg

instance Exception StudentApiClientError

servantToStudentApiError :: ServantError -> StudentApiClientError
servantToStudentApiError servantError = maybe
    (SomeClientError $ show servantError)
    StudentApiClientError
    (fromServantError servantError)
