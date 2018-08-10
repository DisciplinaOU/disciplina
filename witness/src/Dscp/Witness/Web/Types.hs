module Dscp.Witness.Web.Types
    ( Balances (..)
    , BlockInfo (..)
    , AccountInfo (..)
    , TxInfo (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.=), (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (Options (..), deriveJSON)
import Fmt (blockListF, build, (+|), (|+))

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
data BlockInfo = BlockInfo
    { biHeaderHash :: HeaderHash
    , biHeader :: Header
    , biIsGenesis :: Bool
    , biTransactions :: Maybe [TxInfo]
    }

data AccountInfo = AccountInfo
    { aiBalances  :: Balances
    , aiNextNonce :: Integer
    , aiTransactions :: Maybe [TxInfo]
    }

newtype TxInfo = TxInfo GTx
    deriving (Show, Eq, Buildable)

---------------------------------------------------------------------------
-- Buildable instances
---------------------------------------------------------------------------


instance Buildable (ForResponseLog BlockInfo) where
    build (ForResponseLog BlockInfo{..}) =
        "{ headerHash = " +| biHeaderHash |+
        ", header = " +| biHeader |+
        " }"
instance Buildable (ForResponseLog [BlockInfo]) where
    build (ForResponseLog blocks) = blockListF $ map biHeaderHash blocks

instance Buildable Balances where
    build Balances{..} = "{ confirmed = " +| bConfirmed |+ " }"

instance Buildable (ForResponseLog AccountInfo) where
    build (ForResponseLog AccountInfo{..}) =
        -- will differ once transaction list in included
        "{ balances = " +| aiBalances |+
        ", next nonce = " +| aiNextNonce |+
        " }"

deriving instance Buildable (ForResponseLog TxInfo)

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''Balances
deriveJSON defaultOptions{ omitNothingFields = True } ''BlockInfo
deriveJSON defaultOptions{ omitNothingFields = True } ''AccountInfo

instance ToJSON TxInfo where
    toJSON (TxInfo gTx) = case gTx of
        GMoneyTx tx -> object
            [ "txId" .= toTxId tx
            , "txType" .= ("money" :: Text)
            , "money" .= tx
            ]
        GPublicationTx pTx -> object
            [ "txId" .= toPtxId pTx
            , "txType" .= ("publication" :: Text)
            , "publication" .= pTx
            ]

instance FromJSON TxInfo where
    parseJSON = withObject "tx info" $ \o -> do
        txType :: Text <- o .: "txType"
        TxInfo <$> case txType of
            "money" -> GMoneyTx <$> o .: "money"
            "publication" -> GPublicationTx <$> o .: "publication"
            other -> fail $ "invalid transaction type: " ++ toString other
