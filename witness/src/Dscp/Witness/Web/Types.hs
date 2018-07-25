module Dscp.Witness.Web.Types
    ( Balances (..)
    , AccountState (..)
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import Servant (FromHttpApiData (..))

import Dscp.Core.Address
import Dscp.Core.Aeson ()
import Dscp.Core.Types
import Dscp.Util.Servant (ForResponseLog (..))

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
-- Buildable instances
---------------------------------------------------------------------------

instance Buildable Balances where
    build Balances{..} = "{ confirmed = " +| bConfirmed |+ " }"

instance Buildable AccountState where
    build AccountState{..} =
        "{ balances = " +| asBalances |+ " }"
instance Buildable (ForResponseLog AccountState) where
    build (ForResponseLog AccountState{..}) =
        -- will differ once transaction list in included
        "{ balances = " +| asBalances |+ " }"

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
