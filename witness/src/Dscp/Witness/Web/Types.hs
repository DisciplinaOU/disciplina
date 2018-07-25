module Dscp.Witness.Web.Types
    ( Balances (..)
    , AccountState (..)
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant (FromHttpApiData (..))

import Dscp.Core.Address
import Dscp.Core.Aeson ()
import Dscp.Core.Types

-- | All balances related to account.
data Balances = Balances
    { bConfirmed :: !Coin
      -- ^ Only looking at blocks

    -- , bTotal     :: !Coin
      -- ^ From blocks + mempool
    }

-- | All what user may wish to know about an account.
data AccountState = AccountState
    { asBalances  :: Balances
      -- TODO: add transactions list
    }

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''Balances
deriveJSON defaultOptions ''AccountState

---------------------------------------------------------------------------
-- FromHttpApiData instances
---------------------------------------------------------------------------

instance FromHttpApiData Address where
    parseUrlPiece = addrFromText
