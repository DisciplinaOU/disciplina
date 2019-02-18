module Dscp.Web.Class
    ( ErrorTag
    , ErrResponse (..)

    , HasErrorTag (..)
    , ToServantErr (..)
    , FromServantErr (..)
    , toServantErrJustTag
    , unexpectedToServantErr
    , processServerErrors

    , ClientError (..)
    , servantToClientError
    ) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (Options (..), deriveJSON)
import qualified Data.Text.Buildable as B
import Servant (Handler, ServantErr (..), err500, throwError)
import Servant.Client (GenResponse (..), ServantError (..))
import qualified Text.Show

-- | Error kind identifier.
type ErrorTag = Text

-- | Contains info about an error in client-convenient form.
data ErrResponse e = ErrResponse
    { erContent :: !(Maybe e)
    , erError   :: !ErrorTag
    } deriving (Show, Eq, Generic)

deriveJSON defaultOptions{ omitNothingFields = True } ''ErrResponse

-- | For error type appearing in API, allows to fetch corresponding error
-- identifier.
class HasErrorTag e where
    errorTag :: e -> ErrorTag

-- | Get a servant error with appropriate HTTP code, but without
-- reason field filled.
class HasErrorTag e => ToServantErr e where
    -- | Get miminal info about the error, consider using
    -- 'Servant.err404'-like constants in an implementation.
    toServantErrNoBody :: e -> ServantErr

    -- | Convert the error to server error.
    toServantErr :: e -> ServantErr
    default toServantErr :: ToJSON e => e -> ServantErr
    toServantErr e =
        (toServantErrNoBody e)
        { errBody = encode $ ErrResponse (Just e) (errorTag e) }

-- | Implementation for 'toServantErr' suitable for case when there is no need
-- to encode the error entirely, only tag is required.
toServantErrJustTag :: ToServantErr e => e -> ServantErr
toServantErrJustTag e =
    (toServantErrNoBody e)
    { errBody = encode $ ErrResponse @() Nothing (errorTag e) }

-- | Extract the error back from the errored response.
class HasErrorTag e => FromServantErr e where
    -- | Convert client error to the given error.
    fromServantError :: ServantError -> Maybe e
    default fromServantError :: FromJSON e => ServantError -> Maybe e
    fromServantError err = do
        FailureResponse Response{..} <- pure err
        errResponse <- decode @(ErrResponse e) responseBody
        return $ erContent errResponse
          ?: error "fromServantError: no error content"

-- | Handle all previously uncaught errors.
unexpectedToServantErr :: SomeException -> ServantErr
unexpectedToServantErr err = err500{ errBody = show err }

-- | Handle all possible exceptions.
-- @error@ parameter stands for basic exception and errors of this type will be
-- displayed nicely, all other will appear as internal exceptions.
processServerErrors
    :: forall error a.
       (ToServantErr error, Exception error)
    => IO a -> Handler a
processServerErrors action =
    liftIO action
        `catch` (throwError . toServantErr @error)
        `catchAny` (throwError . unexpectedToServantErr)

-- | Represents errors as we get them on client.
data ClientError serverErr
    = ApiClientError !serverErr
      -- ^ API-specific exception with known semantics.
    | SomeClientError !ServantError
      -- ^ General errors (connection, authentication failures, e.t.c).

instance Buildable err => Show (ClientError err) where
    show = toString . pretty

instance Buildable err => Buildable (ClientError err) where
    build = \case
        ApiClientError err -> B.build err
        SomeClientError msg -> show msg

instance (Typeable err, Buildable err) => Exception (ClientError err)

-- | Consider cases of client errors.
servantToClientError :: FromServantErr serverErr => ServantError -> ClientError serverErr
servantToClientError err =
    maybe (SomeClientError err) ApiClientError (fromServantError err)
