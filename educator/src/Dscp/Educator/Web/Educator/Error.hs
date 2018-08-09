-- | Educator API errors

module Dscp.Educator.Web.Educator.Error
       ( APIError (..)

       , ErrResponse (..)

       , DSON

       , toServantErr
       , unexpectedToServantErr
       ) where

import Control.Lens (makePrisms)
import Data.Aeson (ToJSON (..), encode)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveToJSON)
import Data.Reflection (Reifies (..))
import Servant (ServantErr (..), err500)

import Dscp.Util.Servant

-- | Any error backend may return.
data APIError
    = SomeErrors
    | InvalidFormat
      -- ^ Failed to decode something.
    deriving (Show, Eq, Generic, Typeable)

makePrisms ''APIError

instance Exception APIError

-- | Contains info about error in client-convenient form.
data ErrResponse = ErrResponse
    { erError :: !APIError
    } deriving (Show, Eq, Generic)

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveToJSON defaultOptions ''ErrResponse

instance ToJSON APIError where
    toJSON = error "Not implemented"

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

-- | Get HTTP error code of error.
toServantErrNoReason :: APIError -> ServantErr
toServantErrNoReason = error "Not implemented"

-- | Make up error which will be returned to client.
toServantErr :: APIError -> ServantErr
toServantErr err = (toServantErrNoReason err){ errBody = encode $ ErrResponse err }

unexpectedToServantErr :: SomeException -> ServantErr
unexpectedToServantErr err = err500{ errBody = show err }

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data FaucetDecodeErrTag
instance Reifies FaucetDecodeErrTag String where
    reflect _ = decodeUtf8 $ encode InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON FaucetDecodeErrTag
