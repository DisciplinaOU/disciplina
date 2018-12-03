-- | Educator API errors

module Dscp.Educator.Web.Educator.Error
       ( APIError (..)

       , ErrResponse (..)

       , DSON

       , toServantErr
       , unexpectedToServantErr
       ) where

import Control.Lens (makePrisms)
import Data.Reflection (Reifies (..))
import Data.Typeable (cast)
import Dscp.DB.SQLite (SQLRequestsNumberExceeded)
import Dscp.Educator.DB (DomainError)
import Servant (ServantErr (..), err400, err503)

import Dscp.Educator.Web.Util
import Dscp.Util.Servant
import Dscp.Web.Class

-- | Any error backend may return.
data APIError
    = SomeDomainError DomainError
      -- ^ Something not found or already exists.
    | InvalidFormat
      -- ^ Failed to decode something.
    | ServiceUnavailable !Text
      -- ^ Service is overloaded with requests.
    deriving (Show, Eq, Generic)

makePrisms ''APIError

instance Exception APIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , SomeDomainError <$> fromException e
        , ServiceUnavailable . pretty @SQLRequestsNumberExceeded <$> fromException e
        ]

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance HasErrorTag APIError where
    errorTag = \case
        InvalidFormat        -> "InvalidFormat"
        SomeDomainError err  -> domainErrorToShortJSON err
        ServiceUnavailable{} -> "ServiceUnavailable"

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

instance ToServantErr APIError where
    toServantErrNoBody = \case
        InvalidFormat        -> err400
        ServiceUnavailable{} -> err503
        SomeDomainError err  -> domainToServantErrNoReason err
    toServantErr = toServantErrJustTag

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data FaucetDecodeErrTag
instance Reifies FaucetDecodeErrTag String where
    reflect _ = decodeUtf8 . errBody $ toServantErr InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON FaucetDecodeErrTag
