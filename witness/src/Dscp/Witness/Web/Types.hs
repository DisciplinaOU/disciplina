{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData    #-}

module Dscp.Witness.Web.Types
    ( BlocksOrMempool (..)
    , PaginatedList (..)
    , BlockInfo (..)
    , WithBlockInfo (..)
    , Detailed (..)
    , BlockList (..)
    , AccountInfo (..)
    , TxInfo
    , TxList
    , PrivateBlockList
    , TxTypeFilter (..)
    , ATGChange (..)
    , ATGSubjectChange (..)
    , PrivateBlockInfoPart (..)
    , HashIs (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:),
                   (.:?), (.=))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (Options (..), deriveJSON)
import Fmt (build, genericF, (+|), (|+))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Aeson
import Dscp.Util.Servant (ForResponseLog (..), buildForResponse)

-- | Distinguishes stuff on whether does it take mempool in consideration.
data BlocksOrMempool a = BlocksOrMempool
    { bmConfirmed :: a
      -- ^ Only looking at blocks
    , bmTotal     :: a
      -- ^ From blocks + mempool
    } deriving (Eq, Show, Functor, Generic)

-- | Paginated list of something.
-- Parameter @d@ is required to tell name of entities for JSON encoding,
-- needed not to subvert backward-compatibility.
data PaginatedList (d :: Symbol) a = PaginatedList
    { plItems  :: [a]
      -- ^ Requested amount of entities.
    , plNextId :: Maybe (Id a)
      -- ^ Reference to next item.
    } deriving (Generic)

-- | Wrapper to get JSON instances suitable for the explorer.
newtype Detailed a = Detailed { unDetailed :: a }
    deriving (Eq, Show, Generic)

data BlockInfo = BlockInfo
    { biHeaderHash       :: HeaderHash
    , biNextHash         :: Maybe HeaderHash
    , biMerkleRootHash   :: Text
    , biHeader           :: Header
    , biIsGenesis        :: Bool
    , biSince            :: Word64  -- µs since UNIX epoch start
    , biSize             :: Int64  -- bytes
    , biTransactionCount :: Int
    , biTotalOutput      :: Coin
    , biTotalFees        :: Coin
    , biTransactions     :: Maybe [TxInfo]
    } deriving (Eq, Show, Generic)

data WithBlockInfo a = WithBlockInfo
    { wbiBlockInfo :: Maybe BlockInfo
    , wbiItem      :: a
    } deriving (Eq, Show, Generic)

type TxInfo = WithBlockInfo GTx

data BlockList = BlockList
    { blBlocks     :: [BlockInfo]
    , blTotalCount :: Word64
    } deriving (Eq, Show, Generic)

-- | All what user may wish to know about an account.
data AccountInfo = AccountInfo
    { aiBalances         :: BlocksOrMempool Coin
    , aiCurrentNonce     :: Nonce
    , aiTransactionCount :: Integer
    , aiTransactions     :: Maybe [TxInfo]
    } deriving (Eq, Show, Generic)

type TxList = PaginatedList "transactions" TxInfo

data ATGChange
    = ATGAdded
    | ATGRemoved
    deriving (Eq, Show, Generic)

data ATGSubjectChange = ATGSubjectChange
    { ascSubjectId :: Id Subject
    , ascStatus    :: ATGChange
    } deriving (Eq, Show, Generic)

data PrivateBlockInfoPart = PrivateBlockInfoPart
    { piHash            :: PrivateHeaderHash
    , piMerkleRoot      :: Hash Raw
    , piTransactionsNum :: Word32
    , piAtgDelta        :: [ATGSubjectChange]
    } deriving (Eq, Show, Generic)

type PrivateBlockInfo = WithBlockInfo PrivateBlockInfoPart
type PrivateBlockList = PaginatedList "publications" PrivateBlockInfo

data HashIs
    = HashIsUnknown
    | HashIsBlock
    | HashIsAddress
    | HashIsMoneyTx
    | HashIsPublicationTx
    deriving (Eq, Show, Generic)

data TxTypeFilter
    = AllTxTypes
    | MoneyTxType
    | PubTxType
    deriving (Eq, Show, Generic)

---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------

deriving instance (Eq (Id a), Eq a) => Eq (PaginatedList d a)
deriving instance (Show (Id a), Show a) => Show (PaginatedList d a)

instance HasId PrivateBlockInfoPart where
    type Id PrivateBlockInfoPart = PrivateHeaderHash
    getId = piHash

instance HasId a => HasId (WithBlockInfo a) where
    type Id (WithBlockInfo a) = Id a
    getId WithBlockInfo{..} = getId wbiItem

instance Buildable (ForResponseLog BlockInfo) where
    build (ForResponseLog BlockInfo{..}) =
        "{ headerHash = " +| biHeaderHash |+
        ", header = " +| biHeader |+
        " }"

instance Buildable (ForResponseLog BlockList) where
    build (ForResponseLog BlockList{..}) = "" +| length blBlocks |+ " blocks"

instance Buildable a => Buildable (BlocksOrMempool a) where
    build BlocksOrMempool{..} =
        "{ confirmed = " +| bmConfirmed |+
        ", total = " +| bmTotal |+
        " }"

instance KnownSymbol d => Buildable (PaginatedList d a) where
    build PaginatedList{..} =
        length plItems |+ " " +| symbolVal (Proxy @d) |+ ""

instance Buildable (ForResponseLog AccountInfo) where
    build (ForResponseLog AccountInfo{..}) =
        "{ balances = " +| aiBalances |+
        ", current nonce = " +| aiCurrentNonce |+
        " }"

instance (HasId a, Buildable (Id a)) =>
         Buildable (ForResponseLog $ WithBlockInfo a) where
    build (ForResponseLog WithBlockInfo{..}) =
        "{ item = " +| getId wbiItem |+
        ", headerHash = " +| biHeaderHash <$> wbiBlockInfo |+
        " }"

instance KnownSymbol d => Buildable (ForResponseLog (PaginatedList d a)) where
    build (ForResponseLog PaginatedList{..}) =
        "" +| length plItems |+ " " +| symbolValT @d |+ ""

instance Buildable (ForResponseLog HashIs) where
    build (ForResponseLog hashIs) = genericF hashIs

instance Buildable (ForResponseLog TxId) where
    build = buildForResponse

instance Buildable TxTypeFilter where
    build = \case
        AllTxTypes -> "all transaction types"
        MoneyTxType -> "only money transactions"
        PubTxType -> "only publication transactions"

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''BlocksOrMempool
deriveJSON defaultOptions ''BlockList
deriveJSON defaultOptions ''ATGSubjectChange
deriveJSON defaultOptions ''PrivateBlockInfoPart
deriveJSON defaultOptions{ omitNothingFields = True } ''BlockInfo
deriveJSON defaultOptions{ omitNothingFields = True } ''AccountInfo

instance ToJSON (Detailed a) => ToJSON (WithBlockInfo a) where
    toJSON WithBlockInfo{..} = mergeObjects
        (object $ maybe [] (\block -> ["block" .= block]) wbiBlockInfo)
        (toJSON $ Detailed wbiItem)

instance FromJSON (Detailed a) => FromJSON (WithBlockInfo a) where
    parseJSON v = flip (withObject "with block info") v $ \o -> do
        block <- o .:? "block"
        Detailed item <- parseJSON v
        return $ WithBlockInfo block item

instance ToJSON (Detailed GTx) where
    toJSON (Detailed gtx) = object $
        case gtx of
            GMoneyTx tx ->
                [ "txId" .= toTxId tx
                , "txType" .= ("money" :: Text)
                , "money" .= tx
                , case sumCoins . map txOutValue . txOuts $ tx of
                    Right c  -> "txOutValue" .= c
                    Left err -> "txOutValue" .= err
                ]
            GPublicationTx pTx ->
                [ "txId" .= toPtxId pTx
                , "txType" .= ("publication" :: Text)
                , "publication" .= pTx
                ]

instance FromJSON (Detailed GTx) where
    parseJSON = withObject "tx info" $ \o -> do
        txType :: Text <- o .: "txType"
        Detailed <$> case txType of
            "money"       -> GMoneyTx <$> o .: "money"
            "publication" -> GPublicationTx <$> o .: "publication"
            other         -> fail $ "invalid transaction type: " ++ toString other

deriving instance ToJSON (Detailed PrivateBlockInfoPart)
deriving instance FromJSON (Detailed PrivateBlockInfoPart)

instance (ToJSON (Id a), ToJSON a, KnownSymbol d) =>
         ToJSON (PaginatedList d a) where
    toJSON PaginatedList{..} =
        object [symbolValT @d .= plItems, "nextId" .= plNextId]

instance (FromJSON (Id a), FromJSON a, KnownSymbol d) =>
         FromJSON (PaginatedList d a) where
    parseJSON = withObject "tx list" $ \o -> do
        plItems <- o .: symbolValT @d
        plNextId <- o .: "nextId"
        return PaginatedList{..}

instance ToJSON HashIs where
    toJSON = String . \case
        HashIsUnknown -> "unknown"
        HashIsBlock -> "block"
        HashIsAddress -> "address"
        HashIsMoneyTx -> "money-transaction"
        HashIsPublicationTx -> "publication-transaction"

instance FromJSON HashIs where
    parseJSON = withText "hash type" $ \case
        "unknown" -> pure HashIsUnknown
        "block" -> pure HashIsBlock
        "address" -> pure HashIsAddress
        "money-transaction" -> pure HashIsMoneyTx
        "publication-transaction" -> pure HashIsPublicationTx
        _ -> fail "Invalid hash type"

instance ToJSON ATGChange where
    toJSON = String . \case
        ATGAdded -> "added"
        ATGRemoved -> "removed"

instance FromJSON ATGChange where
    parseJSON = withText "ATG change" $ \case
        "added" -> pure ATGAdded
        "removed" -> pure ATGRemoved
        _ -> fail "Invalid ATGChange"

---------------------------------------------------------------------------
-- HttpApiData instances
---------------------------------------------------------------------------

instance ToHttpApiData TxTypeFilter where
    toUrlPiece = \case
        AllTxTypes -> "all"
        MoneyTxType -> "money"
        PubTxType -> "publication"

instance FromHttpApiData TxTypeFilter where
    parseUrlPiece = \case
        "all" -> pure AllTxTypes
        "money" -> pure MoneyTxType
        "publication" -> pure PubTxType
        _ -> Left "Invalid transaction type filter"
