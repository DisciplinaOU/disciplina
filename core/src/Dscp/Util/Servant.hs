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
    , buildShortResponseList
    , buildLongResponseList
    , ApiHasArgClass (..)
    , ApiCanLogArg (..)

    , SimpleJSON

      -- * CORS
    , methodsCoveringAPI

      -- * Sorting query parameters
    , SortingParams
    , TyNamedParam (..)
    , type (?:)
    , SortingSpec (..)
    , SortingOrder (..)
    , SortingItemTagged (..)
    , SortingItem (..)

      -- * Utilities for clients
    , AsClientT
    ) where

import Prelude hiding (log)

import Control.Exception.Safe (handleAny)
import Control.Lens (makePrisms)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Char (isAlphaNum)
import Data.Default (Default (..))
import qualified Data.List as L
import Data.Reflection (Reifies (..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Buildable as B
import qualified Data.Text.Lazy.Builder as B
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt (Buildable (..), Builder, blockListF, fmt, (+|), (|+))
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Loot.Log (Severity (Info))
import Network.HTTP.Types.Method (Method, StdMethod)
import Serokell.Util ()
import Serokell.Util.ANSI (Color (..), colorizeDull)
import Servant.API (FromHttpApiData (..))
import Servant.API ((:<|>) (..), (:>), Capture, Description, JSON, NoContent, QueryFlag, QueryParam,
                    ReflectMethod (..), ReqBody, Summary, Verb)
import Servant.API.ContentTypes (Accept (..), MimeRender (..), MimeUnrender (..))
import Servant.Client.Core (Client, HasClient (..))
import Servant.Generic ((:-))
import Servant.Server (Handler (..), HasServer (..), ServantErr (..), Server, Tagged (..), unTagged)
import qualified Servant.Server.Internal as SI
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

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
    { clcLog :: Severity -> Text -> IO ()
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
    log :: Severity -> Text -> Handler ()
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

buildShortResponseList :: Buildable a => ForResponseLog [a] -> Builder
buildShortResponseList = blockListF . (take 4) . unForResponseLog

buildLongResponseList :: Buildable a => ForResponseLog [a] -> Builder
buildLongResponseList = blockListF . (take 8) . unForResponseLog

---------------------------------------------------------------------------
-- Deserialisation errors
---------------------------------------------------------------------------

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
-- CORS methods coherence
-------------------------------------------------------------------------

-- | Whether a given type item is element of a given list.
type family IsElemBool (x :: StdMethod) (l :: [StdMethod]) :: Bool where
    IsElemBool x (x : _) = 'True
    IsElemBool x (y : xs) = IsElemBool x xs
    IsElemBool x '[] = 'False

type family FailOnDissallowedMethod (method :: StdMethod) (allowed :: Bool) :: Constraint where
    FailOnDissallowedMethod _ 'True = ()
    FailOnDissallowedMethod m 'False = TypeError
        ( 'Text "Method " ':$$: 'ShowType m ':$$: 'Text " is not allowed, but appears in API"
        )

-- | Ensure that the given api uses only methods from the list provided.
type family ContainsOnlyMethods (methods :: [StdMethod]) api :: Constraint where
    ContainsOnlyMethods ms ((path :: Symbol) :> sub) = ContainsOnlyMethods ms sub
    ContainsOnlyMethods ms (part :> sub) = ContainsOnlyMethods ms sub
    ContainsOnlyMethods ms (api1 :<|> api2) = (ContainsOnlyMethods ms api1,
                                               ContainsOnlyMethods ms api2)
    ContainsOnlyMethods ms (Verb m _ _ _) = FailOnDissallowedMethod m (IsElemBool m ms)

-- | 'ReflectMethod' lifted to method lists.
class ReflectMethods (methods :: [StdMethod]) where
    reflectMethods :: Proxy methods -> [Method]
instance ReflectMethods '[] where
    reflectMethods _ = []
instance (ReflectMethod m, ReflectMethods ms) => ReflectMethods (m ': ms) where
    reflectMethods _ = reflectMethod @m Proxy : reflectMethods @ms Proxy

-- | For the given list of methods, ensure only they are used in API, and get corresponding
-- 'Method' terms.
--
-- A primary use case for this function is specifying CORS methods where we need to think
-- about each single method we allow, thus expecting methods list to be specified manually.
methodsCoveringAPI
    :: forall methods api.
       (ContainsOnlyMethods methods api, ReflectMethods methods)
    => [Method]
methodsCoveringAPI = reflectMethods @methods Proxy

---------------------------------------------------------------------------
-- Sorting parameters
---------------------------------------------------------------------------


-- | Pair of type and its name as it appears in API.
data TyNamedParam a = TyNamedParam Symbol a

-- | Convenient type alias for 'TyNamedParam'.
type (?:) = 'TyNamedParam

{- | Servant API combinator which allows to accept sorting parameters as a query parameter.

Example: with the following combinator

@
SortingParams ["time" ?: Timestamp, "name" ?: Text]
@

the endpoint can parse "sort_by=-time,+name" or "sort_by=desc(time),asc(name)" formats,
which would mean sorting by mentioned fields lexicographically. All sorting
subparameters are optional, as well as entire "sort_by" parameter.

Your handler will be provided with 'SortingSpec' argument which can later be passed
in an appropriate function to perform sorting.
-}
data SortingParams (allowed :: [TyNamedParam *])

-- | Order of sorting.
data SortingOrder
    = Descendant
    | Ascendant
    deriving (Show, Eq, Enum)

-- | For a given field, user-supplied order of sorting.
-- This type is primarly for internal use, see 'SortingItemTagged'.
data SortingItem = SortingItem
    { siName  :: Text
      -- ^ Name of parameter.
      -- Always matches one in @param@, but we keep it here as well for convenience.
    , siOrder :: SortingOrder
      -- ^ Sorting order on the given parameter.
    } deriving (Show)

-- | Version 'SortingItem' which remembers its name and parameter type at type level.
-- In functions which belong to public API you will most probably want to use this datatype
-- as a safer variant of 'SortingIte,'.
newtype SortingItemTagged (param :: TyNamedParam *) = SortingItemTagged
    { untagSortingItem :: SortingItem
    } deriving (Show)

instance Buildable SortingItem where
    build SortingItem{..} =
        let order = case siOrder of { Ascendant -> "⯅ "; Descendant -> "⯆ " }
        in order <> build siName

deriving instance Buildable (SortingItemTagged param)

-- | Extract info from 'SortingParams'.
class ReifySortingParams (params :: [TyNamedParam *]) where
    -- | Get all expected parameter names.
    reifySortingParamsNames :: Set Text

instance ReifySortingParams '[] where
    reifySortingParamsNames = mempty

instance (KnownSymbol name, ReifySortingParams params, SortParamsContainNoName params name) =>
         ReifySortingParams ('TyNamedParam name p ': params) where
    reifySortingParamsNames =
        toText (symbolVal @name Proxy) `S.insert` reifySortingParamsNames @params

type family SortParamsContainNoName (params :: [TyNamedParam *]) name :: Constraint where
    SortParamsContainNoName '[] name = ()
    SortParamsContainNoName ('TyNamedParam name p ': params) name =
        TypeError ('Text "Duplicate name in sorting parameters " ':$$: 'ShowType name)
    SortParamsContainNoName ('TyNamedParam name p ': params) name' =
        SortParamsContainNoName params name'

-- | Tagged, because we want to retain list of allowed fields for parsing
-- (in @instance FromHttpApiData@).
type TaggedSortingItemsList allowed = Tagged (allowed :: [TyNamedParam *]) [SortingItem]

-- | How servant sees 'SortParams' under the hood.
type SortParamsExpanded allowed subApi =
    QueryParam "sort_by" (TaggedSortingItemsList allowed) :> subApi

{- | What is passed to an endpoint, contains all sorting parameters provided by a user.

Following properties hold:
1. Each entry in the underlying list has a unique name ('siName' field).
2. Entries correspond to @params@ type, i.e. any 'SortingItem' entry of the underlying
list with name "N" will be present in @params@ and typed with the same type variable
which it is in pair with within @params@.

Not all parameters specified by @params@ phantom type can be present, e.g. the underlying
list will be empty if user didn't pass "sort_by" query parameter at all.
-}
newtype SortingSpec (params :: [TyNamedParam *]) = SortingSpec
    { unSortingSpec :: [SortingItem]
    } deriving (Default)

-- | Ensure no name in entires repeat.
sortingCheckDuplicates :: [SortingItem] -> Either Text ()
sortingCheckDuplicates items =
    let names = map siName items
        duplicate = safeHead . mapMaybe (safeHead . drop 1) . L.group $ sort names
    in maybe pass (\n -> Left $ "Duplicated field " <> show n) duplicate

-- | Consumes "sort_by" query parameter and fetches sorting parameters contained in it.
instance ( HasServer subApi ctx
         , ReifySortingParams params
         ) =>
         HasServer (SortingParams params :> subApi) ctx where
    type ServerT (SortingParams params :> subApi) m =
        SortingSpec params -> ServerT subApi m

    route =
        inRouteServer @(SortParamsExpanded params subApi) route $
        \handler rawSortItems -> handler (SortingSpec $ fmap unTagged rawSortItems ?: [])

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy @subApi) pc nt . s

-- | Parse 'sort_by' query param.
-- Following the format described in "Sorting" section of https://www.moesif.com/blog/technical/api-design/REST-API-Design-Filtering-Sorting-and-Pagination/
instance ReifySortingParams allowed =>
         FromHttpApiData (TaggedSortingItemsList allowed) where
    parseUrlPiece =
        first (toText . P.parseErrorPretty) . second Tagged .
        P.parse parser "sort_by"
      where
        parser = do
            items <- P.sepBy itemParser (P.char ',')
            either (fail . toString) pure $ sortingCheckDuplicates items
            P.eof
            return items

        itemParser :: P.Parsec Void Text SortingItem
        itemParser = asum
            [ do
                siOrder <- asum
                    [ Ascendant <$ P.char '+'
                    , Descendant <$ P.char '-'
                    ] <?> "ordering sign (+/-)"
                siName <- paramNameParser
                return SortingItem{..}

            , do
                siOrder <- asum
                    [ Ascendant <$ P.string' "asc"
                    , Descendant <$ P.string' "desc"
                    ] <?> "ordering keyword (asc/desc)"
                siName <- P.char '(' *> paramNameParser <* P.char ')'
                return SortingItem{..}
            ]

        allowedParams = reifySortingParamsNames @allowed

        paramNameParser = do
            name <- P.takeWhile1P (Just "sorting item name") isAlphaNum <?> "parameter name"
            unless (name `S.member` allowedParams) $
                fail $ "unknown parameter " <> show name <>
                       " (expected one of " <> show (toList allowedParams) <> ")"
            return name

instance ( HasLoggingServer config subApi ctx
         , ReifySortingParams params
         ) =>
         HasLoggingServer config (SortingParams params :> subApi) ctx where
    routeWithLog =
        inRouteServer @(SortingParams params :> LoggingApiRec config subApi) route $
        \(paramsInfo, handler) sorting@(SortingSpec params) ->
            let text = fmt . mconcat $ "sorting: " : L.intersperse " " (map build params)
            in (addParamLogInfo text paramsInfo, handler sorting)

-- | We do not yet support passing sorting parameters in client.
-- We seem to be too far away from writing server-client tests in Haskell for now,
-- and I'm not sure whether this will ever be useful (@martoon).
instance HasClient m subApi =>
         HasClient m (SortingParams params :> subApi) where
    type Client m (SortingParams params :> subApi) = Client m subApi
    clientWithRoute mp _ req = clientWithRoute mp (Proxy @subApi) req

---------------------------------------------------------------------------
-- Client stuff
---------------------------------------------------------------------------

-- todo: not needed with servant-client-0.14 (lts-12)
data AsClientT (m :: * -> *)
type instance AsClientT m :- api = Client m api
