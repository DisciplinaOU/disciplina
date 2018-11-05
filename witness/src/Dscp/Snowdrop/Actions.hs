-- | DB-related actions.

module Dscp.Snowdrop.Actions
    ( SDVars
    , SDActionsM (..)
    , initSDActions
    , sdActionsComposition
    , initAccounts
    ) where

import qualified Data.Map as Map
import Data.Time.Clock (UTCTime (..))
import UnliftIO (MonadUnliftIO)

import qualified Snowdrop.Block as SD
import qualified Snowdrop.Execution as SD (CompositeChgAccum, DbAccessActions (..),
                                           DbModifyActions (..), SumChangeSet,
                                           constructCompositeActions)
import qualified Snowdrop.Util as SD (ExecM, RIO (..))

import Dscp.Core
import Dscp.Crypto
import qualified Dscp.DB.CanProvideDB as DB
import Dscp.Snowdrop.Configuration (BlockPlusAVLComposition, Ids (..), Values (..))
import Dscp.Snowdrop.Serialise ()
import Dscp.Snowdrop.Storage.Avlp
import Dscp.Snowdrop.Storage.PluginBased
import Dscp.Snowdrop.Types
import Dscp.Witness.AVL (AvlHash (..), AvlProof)
import Dscp.Witness.Config

-- It should be something more complex than IO.
type SDVars = SDActionsM SD.ExecM

deriving instance MonadUnliftIO (SD.RIO ctx)

type SDDataActions  m = SD.DbModifyActions (AVLChgAccum AvlHash Ids Values) Ids Values m AvlProof
type SDBlockActions m = SD.DbModifyActions (SD.SumChangeSet Ids Values) Ids Values m ()

-- Parameter m will be instantiated with RIO Context when the context is defined.
data SDActionsM m = SDActionsM
    { nsStateDBActions  :: SDDataActions m
    , nsBlockDBActions  :: SDBlockActions m
    , nsSDParamsBuilder :: SD.OSParamsBuilder
    }

initSDActions
    ::  forall n m
    .   ( MonadUnliftIO n
        , MonadCatch n
        , HasWitnessConfig
        , MonadIO m
        , DB.ProvidesPlugin m
        )
    =>  m (SDActionsM n)
initSDActions = do
    plugin       <- DB.providePlugin
    stateHandler <- mkAvlDbModifyActions @AvlHash @Ids @Values plugin
    blockHandler <- blockActions plugin

    let startTime         = UTCTime            (toEnum 0) (toEnum 0)
    let nsSDParamsBuilder = SD.OSParamsBuilder (const $ SD.OSParams startTime startTime)
    let sdActions         = SDActionsM          stateHandler blockHandler nsSDParamsBuilder

    pure sdActions

initAccounts :: HasWitnessConfig => Map Ids Values
initAccounts = Map.fromList
    [
      ( AccountInIds (AccountId genesisBlockAddress)
      , AccountOutVal (Account (coinToInteger totalCoins) 0)
      )
    ]
  where
    genesisBlockAddress = mkAddr $ toPublic genesisSk
    totalCoins = totalCoinsAddrMap $ giAddressMap genesisInfo

sdActionsComposition
  :: MonadCatch m
  => SDActionsM m
  -> SD.DbAccessActions
      (SD.CompositeChgAccum
          (SD.SumChangeSet Ids Values)
          (AVLChgAccum AvlHash Ids Values)
          BlockPlusAVLComposition)
      Ids
      Values
      m
sdActionsComposition SDActionsM{..} =
    SD.constructCompositeActions
        @BlockPlusAVLComposition
        (SD.dmaAccessActions nsBlockDBActions)
        (SD.dmaAccessActions nsStateDBActions)
