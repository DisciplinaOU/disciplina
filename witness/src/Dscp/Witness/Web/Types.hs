module Dscp.Witness.Web.Types
    ( Balances (..)
    , AccountState (..)
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import Servant (FromHttpApiData (..))

import Dscp.Core
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
    , asNextNonce :: Integer
      -- TODO: add transactions list
    }

---------------------------------------------------------------------------
-- Buildable instances
---------------------------------------------------------------------------

instance Buildable Balances where
    build Balances{..} = "{ confirmed = " +| bConfirmed |+ " }"

instance Buildable AccountState where
    build AccountState{..} =
        "{ balances = " +| asBalances |+
        ", next nonce = " +| asNextNonce |+
        " }"
instance Buildable (ForResponseLog AccountState) where
    build (ForResponseLog AccountState{..}) =
        -- will differ once transaction list in included
        "{ balances = " +| asBalances |+
        ", next nonce = " +| asNextNonce |+
        " }"

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
