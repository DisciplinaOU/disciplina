-- | Faucet API errors

module Dscp.Faucet.Web.Error
       ( FaucetAPIError (..)
       , DSON

       , toServantErr
       , unexpectedToServantErr
       ) where

import Control.Lens (makePrisms)
import Data.Data (Data)
import Data.Reflection (Reifies (..))
import Servant (ServantErr (..), err400, err403, err503)
import Servant.Util (SimpleJSON)

import Dscp.Web.Class

-- | Any error backend may return.
data FaucetAPIError
    = InvalidFormat
      -- ^ Failed to parse something
    | AddressAlreadyGifted
      -- ^ Request to submit money to given address was already processed
      -- previously.
    | SourceAccountExhausted
      -- ^ Source address has not enough money for further use.
    deriving (Show, Eq, Generic, Typeable, Data)

makePrisms ''FaucetAPIError

instance Exception FaucetAPIError

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

instance HasErrorTag FaucetAPIError where
    -- HTTP error is enough to distinguish possible errors for now
    errorTag _ = ""

instance ToServantErr FaucetAPIError where
    toServantErrNoBody = \case
        InvalidFormat -> err400
        AddressAlreadyGifted -> err403
        SourceAccountExhausted -> err503
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
