{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

-- | Some utilites for more flexible servant usage.
-- Copy-pasted with minor changes from my work from /you know there/ (@martoon).

module Dscp.Util.Servant
    ( -- * Automatic requests logging
      LoggingApi
    , ServantLogConfig (..)
    , ForResponseLog (..)
    , buildListForResponse
    , buildForResponse
    , responseTimeMetric

    , SimpleJSON
    ) where

import Prelude hiding (log)

import Control.Exception.Safe (handleAny)
import Control.Lens (makePrisms)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Default (Default (..))
import Data.Reflection (Reifies (..))
import qualified Data.Text as T
import qualified Data.Text.Buildable as B
import qualified Data.Text.Lazy.Builder as B
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt (blockListF, (+|), (|+))
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Loot.Log (Level (Info))
import Mon.Network (Endpoint)
import Network.Wai (Middleware)
import Serokell.Util ()
import Serokell.Util.ANSI (Color (..), colorizeDull)
import Servant.API ((:<|>) (..), (:>), Capture, Description, JSON, NoContent, QueryFlag, QueryParam,
                    ReflectMethod (..), ReqBody, Summary, Verb)
import Servant.API.ContentTypes (Accept (..), MimeRender (..), MimeUnrender (..))
import Servant.Server (Handler (..), HasServer (..), ServantErr (..), Server)
import qualified Servant.Server.Internal as SI

import Dscp.Util (reportTime)

-------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------

inRouteServer
    :: forall api api' ctx env.
       (Proxy api -> SI.Context ctx -> SI.Delayed env (Server api) -> SI.Router env)
    -> (Server api' -> Server api)
    -> (Proxy api' -> SI.Context ctx -> SI.Delayed env (Server api') -> SI.Router env)
inRouteServer routing f = \_ ctx delayed -> routing Proxy ctx (fmap f delayed)

-------------------------------------------------------------------------
-- General useful families
-------------------------------------------------------------------------

-- | Extract right side of type application.
type family ApplicationRS api where
    ApplicationRS (apiType a) = a

-- | Proves info about argument specifier of servant API.
class ApiHasArgClass api where
    -- | For arguments-specifiers of API, get argument type.
    -- E.g. @Capture "cap" Int@ -> @Int@.
    type ApiArg api :: *
    type ApiArg api = ApplicationRS api

    -- | Name of argument.
    -- E.g. name of argument specified by @Capture "nyan"@ is /nyan/.
    apiArgName
        :: Proxy api -> String
    default apiArgName
        -- Note [redundant constraint]
        -- GHC thinks 'api ~ someApiType n a' is a redundant constraint!
        -- It's not: it makes the 'KnownSymbol n' constraint useful.
        :: forall n someApiType a. (KnownSymbol n, api ~ someApiType n a)
        => Proxy api -> String
    apiArgName _ = "'" +| symbolVal (Proxy @n) |+  "' field"

class ServerT (subApi :> res) m ~ (ApiArg subApi -> ServerT res m)
   => ApiHasArgInvariant subApi res m
instance ServerT (subApi :> res) m ~ (ApiArg subApi -> ServerT res m)
      => ApiHasArgInvariant subApi res m

type ApiHasArg subApi res =
    ( ApiHasArgClass subApi
    , ApiHasArgInvariant subApi res Handler
    )

instance KnownSymbol s => ApiHasArgClass (Capture s a)
instance KnownSymbol s => ApiHasArgClass (QueryParam s a) where
    type ApiArg (QueryParam s a) = Maybe a
instance KnownSymbol s => ApiHasArgClass (QueryFlag s) where
    type ApiArg (QueryFlag s) = Bool
    apiArgName _ = "'" +| symbolVal (Proxy @s) |+  "' flag"
instance ApiHasArgClass (ReqBody ct a) where
    apiArgName _ = "request body"

-------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------

-- | Enables logging for server which serves given api.
--
-- `config` is a type at which you have to specify 'ServantLogConfig' via
-- reflection. This way was chosen because the least thing we need in
-- config is 'LoggerName', and we want to have '<>' on 'LoggerName's thus
-- 'KnownSymbol' is not enough.
--
-- This logging will report
--
-- * Request parameters, including request bodies
-- * If execution failed with error, it will be displayed
-- * Details like request method and endpoint execution time
--
-- If user makes request which is not defined it won't be logged. However,
-- I don't find it a great problem, it may impede only in development or on
-- getting acknowledged with api.
data LoggingApi config api

-- | Helper to traverse servant api and apply logging.
data LoggingApiRec config api

newtype ServantLogConfig = ServantLogConfig
    { clcLog :: Level -> Text -> IO ()
    }

-- | Used to incrementally collect info about passed parameters.
data ApiParamsLogInfo
      -- | Parameters gathered at current stage
    = ApiParamsLogInfo [Text]
      -- | Parameters collection failed with reason
      --   (e.g. decoding error)
    | ApiNoParamsLogInfo Text

makePrisms ''ApiParamsLogInfo

instance Default ApiParamsLogInfo where
    def = ApiParamsLogInfo mempty

addParamLogInfo :: Text -> ApiParamsLogInfo -> ApiParamsLogInfo
addParamLogInfo paramInfo = _ApiParamsLogInfo %~ (paramInfo :)

-- | When it comes to logging responses, returned data may be very large.
-- Log space is valuable (already in testnet we got truncated logs),
-- so we have to care about printing only whose data which may be useful.
newtype ForResponseLog a = ForResponseLog { unForResponseLog :: a }

buildListForResponse
    :: Buildable (ForResponseLog x)
    => (forall a. [a] -> [a]) -> ForResponseLog [x] -> B.Builder
buildListForResponse truncList (ForResponseLog l) =
    let lt = truncList l
        diff = length l - length lt
        mMore | diff == 0 = ""
              | otherwise = "\n    and " +| diff |+ " entries more..."
    in  blockListF (map ForResponseLog lt) |+ mMore

buildForResponse :: Buildable a => ForResponseLog a -> B.Builder
buildForResponse = B.build . unForResponseLog

instance ( HasServer (LoggingApiRec config api) ctx
         , HasServer api ctx
         ) =>
         HasServer (LoggingApi config api) ctx where
    type ServerT (LoggingApi config api) m = ServerT api m

    route = inRouteServer @(LoggingApiRec config api) route
            (def, )

    hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- | Version of 'HasServer' which is assumed to perform logging.
-- It's helpful because 'ServerT (LoggingApi ...)' is already defined for us
-- in actual 'HasServer' instance once and forever.
class HasServer api ctx => HasLoggingServer config api ctx where
    routeWithLog
        :: Proxy (LoggingApiRec config api)
        -> SI.Context ctx
        -> SI.Delayed env (Server (LoggingApiRec config api))
        -> SI.Router env

instance HasLoggingServer config api ctx =>
         HasServer (LoggingApiRec config api) ctx where
    type ServerT (LoggingApiRec config api) m =
         (ApiParamsLogInfo, ServerT api m)

    route = routeWithLog

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt <$> s

instance ( HasLoggingServer config api1 ctx
         , HasLoggingServer config api2 ctx
         ) =>
         HasLoggingServer config (api1 :<|> api2) ctx where
    routeWithLog =
        inRouteServer
            @(LoggingApiRec config api1 :<|> LoggingApiRec config api2)
            route $
            \(paramsInfo, f1 :<|> f2) -> (paramsInfo, f1) :<|> (paramsInfo, f2)

instance ( KnownSymbol path
         , HasLoggingServer config res ctx
         ) =>
         HasLoggingServer config (path :> res) ctx where
    routeWithLog =
        inRouteServer @(path :> LoggingApiRec config res) route $
        first updateParamsInfo
      where
        updateParamsInfo =
            let path = toText . symbolVal $ Proxy @path
            in  addParamLogInfo path

-- | Describes a way to log a single parameter.
class ApiHasArgClass subApi =>
      ApiCanLogArg subApi where
    type ApiArgToLog subApi :: *
    type ApiArgToLog subApi = ApiArg subApi

    toLogParamInfo
        :: Buildable (ApiArgToLog subApi)
        => Proxy subApi -> ApiArg subApi -> Text
    default toLogParamInfo
        :: Buildable (ApiArg subApi)
        => Proxy subApi -> ApiArg subApi -> Text
    toLogParamInfo _ param = pretty param

instance KnownSymbol s => ApiCanLogArg (Capture s a)

instance ApiCanLogArg (ReqBody ct a)

instance KnownSymbol cs => ApiCanLogArg (QueryParam cs a) where
    type ApiArgToLog (QueryParam cs a) = a
    toLogParamInfo _ mparam = maybe noEntry pretty mparam
      where
        noEntry = colorizeDull White "-"

instance KnownSymbol cs => ApiCanLogArg (QueryFlag cs) where
    type ApiArgToLog (QueryFlag cs) = Bool

paramRouteWithLog
    :: forall config api subApi res ctx env.
       ( api ~ (subApi :> res)
       , HasServer (subApi :> LoggingApiRec config res) ctx
       , ApiHasArg subApi res
       , ApiHasArg subApi (LoggingApiRec config res)
       , ApiCanLogArg subApi
       , Buildable (ApiArgToLog subApi)
       )
    => Proxy (LoggingApiRec config api)
    -> SI.Context ctx
    -> SI.Delayed env (Server (LoggingApiRec config api))
    -> SI.Router env
paramRouteWithLog =
    inRouteServer @(subApi :> LoggingApiRec config res) route $
        \(paramsInfo, f) a -> (a `updateParamsInfo` paramsInfo, f a)
  where
    updateParamsInfo a =
        let paramVal = toLogParamInfo (Proxy @subApi) a
            paramName = apiArgName $ Proxy @subApi
            paramInfo = paramName |+ ": " +| paramVal |+ ""
        in addParamLogInfo paramInfo

instance ( HasServer (subApi :> res) ctx
         , HasServer (subApi :> LoggingApiRec config res) ctx
         , ApiHasArg subApi res
         , ApiHasArg subApi (LoggingApiRec config res)
         , ApiCanLogArg subApi
         , Buildable (ApiArgToLog subApi)
         , subApi ~ apiType a
         ) =>
         HasLoggingServer config (apiType a :> res) ctx where
    routeWithLog = paramRouteWithLog

instance ( HasLoggingServer config res ctx
         , KnownSymbol s
         ) =>
         HasLoggingServer config (QueryFlag s :> res) ctx where
    routeWithLog = paramRouteWithLog

instance HasLoggingServer config res ctx =>
         HasLoggingServer config (Summary s :> res) ctx where
    routeWithLog = inRouteServer @(Summary s :> LoggingApiRec config res) route identity

instance HasLoggingServer config res ctx =>
         HasLoggingServer config (Description d :> res) ctx where
    routeWithLog = inRouteServer @(Description d :> LoggingApiRec config res) route identity


-- | Unique identifier for request-response pair.
newtype RequestId = RequestId Integer

instance Buildable RequestId where
    build (RequestId i) = "#" +| i |+ ""

-- | We want all servant servers to have non-overlapping ids,
-- so using singleton counter here.
requestsCounter :: TVar Integer
requestsCounter = unsafePerformIO $ newTVarIO 0
{-# NOINLINE requestsCounter #-}

nextRequestId :: MonadIO m => m RequestId
nextRequestId = atomically $ do
    modifyTVar' requestsCounter (+1)
    RequestId <$> readTVar requestsCounter

-- | Modify an action so that it performs all the required logging.
applyServantLogging
    :: ( Reifies config ServantLogConfig
       , ReflectMethod (method :: k)
       )
    => Proxy config
    -> Proxy method
    -> ApiParamsLogInfo
    -> (a -> Text)
    -> Handler a
    -> Handler a
applyServantLogging configP methodP paramsInfo showResponse action = do
    timer <- mkTimer
    reqId <- nextRequestId
    catchErrors reqId timer $ do
        reportRequest reqId
        res <- action
        reportResponse reqId timer res
        return res
  where
    method = decodeUtf8 $ reflectMethod methodP
    cmethod =
        flip colorizeDull method $
            case method of
            "GET"    -> Cyan
            "POST"   -> Yellow
            "PUT"    -> Blue
            "DELETE" -> Red
            _        -> Magenta
    mkTimer :: MonadIO m => m (m Text)
    mkTimer = do
        startTime <- liftIO getPOSIXTime
        return $ do
            endTime <- liftIO getPOSIXTime
            return . show $ endTime - startTime
    log :: Level -> Text -> Handler ()
    log = liftIO ... clcLog $ reflect configP
    eParamLogs :: Either Text Text
    eParamLogs = case paramsInfo of
        ApiParamsLogInfo info -> Right $
            T.intercalate "\n" $ reverse info <&> \securedParamsInfo ->
                "    " +| colorizeDull White ":>"
                |+ " " +| securedParamsInfo |+ ""
        ApiNoParamsLogInfo why -> Left why
    reportRequest :: RequestId -> Handler ()
    reportRequest reqId =
        case eParamLogs of
            Left e ->
                log Info $
                    "\n" +| colorizeDull Red "Unexecuted request due to error"
                    |+ " " +| e |+ ""
            Right paramLogs -> do
                log Info $
                       "\n" +| cmethod
                    |+ " "  +| colorizeDull White ("Request " <> pretty reqId)
                    |+ "\n" +| paramLogs |+ ""
    responseTag reqId = "Response " <> pretty reqId
    reportResponse reqId timer resp = do
        durationText <- timer
        log Info $
            "\n    " +| colorizeDull White (responseTag reqId)
              |+ " " +| (colorizeDull Green "OK")
              |+ " " +| durationText
              |+ " " +| colorizeDull White ">"
              |+ " " +| showResponse resp
              |+ ""
    catchErrors reqId st =
        flip catchError (servantErrHandler reqId st) .
        handleAny (exceptionsHandler reqId st)
    servantErrHandler reqId timer err@ServantErr{..} = do
        durationText <- timer
        let errMsg = errHTTPCode |+ " "  +| errReasonPhrase |+ ":"
        log Info $
            "\n    " +| colorizeDull White (responseTag reqId)
              |+ " " +| durationText
              |+ " " +| colorizeDull Red errMsg
              |+ " " +| decodeUtf8 @Text errBody
              |+ ""
        throwError err
    exceptionsHandler reqId timer e = do
        durationText <- timer
        log Info $
            "\n    " +| colorizeDull Red (responseTag reqId)
              |+ " " +| e
              |+ " " +| durationText
              |+ ""
        throwM e

applyLoggingToHandler
    :: forall config method a.
       ( Buildable (ForResponseLog a)
       , Reifies config ServantLogConfig
       , ReflectMethod method
       )
    => Proxy config -> Proxy (method :: k) -> (ApiParamsLogInfo, Handler a) -> Handler a
applyLoggingToHandler configP methodP (paramsInfo, handler) = do
    applyServantLogging configP methodP paramsInfo (pretty . ForResponseLog) handler

instance ( HasServer (Verb mt st ct a) ctx
         , Reifies config ServantLogConfig
         , ReflectMethod mt
         , Buildable (ForResponseLog a)
         ) =>
         HasLoggingServer config (Verb (mt :: k) (st :: Nat) (ct :: [*]) a) ctx where
    routeWithLog =
        inRouteServer @(Verb mt st ct a) route $
        applyLoggingToHandler (Proxy @config) (Proxy @mt)

instance Buildable (ForResponseLog NoContent) where
    build _ = "<no response>"

instance Buildable (ForResponseLog ()) where
    build _ = "<no response>"

instance Buildable (ForResponseLog Integer) where
    build = buildForResponse

-------------------------------------------------------------------------
-- Deserialisation errors
-------------------------------------------------------------------------

-- | Custom json marker which sends no human-unreadable decoding errors
-- but a given fixed one.
data SimpleJSON err

instance Accept (SimpleJSON err) where
    contentTypes _ = contentTypes (Proxy @JSON)
instance ToJSON a => MimeRender (SimpleJSON err) a where
    mimeRender _ = encode
instance (FromJSON a, Reifies err String) => MimeUnrender (SimpleJSON err) a where
    mimeUnrender _ =
        let errMsg = reflect (Proxy @err)
        in first (\_ -> errMsg) . eitherDecode

-------------------------------------------------------------------------
-- Response time metrics
-------------------------------------------------------------------------

responseTimeMetric :: Maybe Endpoint -> Middleware
responseTimeMetric endpoint app = \request f ->
    reportTime "disciplina.timer.http_request" endpoint  (app request f)
