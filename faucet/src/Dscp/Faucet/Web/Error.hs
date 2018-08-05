-- | Faucet API errors

module Dscp.Faucet.Web.Error
       ( APIError (..)

       , toServantErr
       , unexpectedToServantErr
       ) where

import Control.Lens (makePrisms)
import Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
import Data.Data (Data, toConstr)
import Servant (ServantErr (..), err403, err500, err503)

-- | Any error backend may return.
data APIError
    = AddressAlreadyGifted
      -- ^ Request to submit money to given address was already processed
      -- previously.
    | SourceAccountExhausted
      -- ^ Source address has not enough money for further use.
    deriving (Show, Eq, Generic, Typeable, Data)

makePrisms ''APIError

instance Exception APIError

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance ToJSON APIError where
    toJSON err = object
        [ "error" .= String (show $ toConstr err) ]

---------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------

-- | Get HTTP error code of error.
toServantErrNoReason :: APIError -> ServantErr
toServantErrNoReason = \case
    AddressAlreadyGifted -> err403
    SourceAccountExhausted -> err503

-- | Make up error which will be returned to client.
toServantErr :: APIError -> ServantErr
toServantErr err = (toServantErrNoReason err){ errBody = encode err }

unexpectedToServantErr :: SomeException -> ServantErr
unexpectedToServantErr err = err500{ errBody = show err }
