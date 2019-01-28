{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData        #-}

module Dscp.Witness.Web.Types
    ( BlocksOrMempool (..)
    , PaginatedList (..)
    , BlockInfo (..)
    , WithBlockInfo (..)
    , Detailed (..)
    , BlockList (..)
    , AccountInfo (..)
    , TxInfo
    , GTxInfo
    , TxList
    , PublicationList
    , HashIs (..)
    , FairCVAndCheckResult (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:),
                   (.:?), (.=))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (Options (..), deriveJSON)
import Fmt (build, genericF, (+|), (|+))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.Util (ForResponseLog (..), buildForResponse)

import Dscp.Core
import Dscp.Util
import Dscp.Util.Aeson

data FairCVAndCheckResult = FairCVAndCheckResult
    { fcacrFairCV      :: FairCV
    , fcacrCheckResult :: FairCVCheckResult
    } deriving (Eq, Show, Generic)

-- | Distinguishes stuff on whether does it take mempool in consideration.
data BlocksOrMempool a = BlocksOrMempool
    { bmConfirmed :: a
      -- ^ Only looking at blocks
    , bmTotal     :: a
      -- ^ From blocks + mempool
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Paginated list of something.
-- Parameter @d@ is required to tell name of entities for JSON encoding,
-- helps not to subvert backward-compatibility.
-- TODO: does anyone care at all?
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
    , biTransactions     :: Maybe [GTxInfo]
    } deriving (Eq, Show, Generic)

data WithBlockInfo a = WithBlockInfo
    { wbiBlockInfo :: Maybe BlockInfo
    , wbiItem      :: a
    } deriving (Eq, Show, Generic)

type TxInfo = WithBlockInfo Tx
type GTxInfo = WithBlockInfo GTx

data BlockList = BlockList
    { blBlocks     :: [BlockInfo]
    , blTotalCount :: Word64
    } deriving (Eq, Show, Generic)

-- | All what user may wish to know about an account.
data AccountInfo = AccountInfo
    { aiBalances         :: BlocksOrMempool Coin
    , aiCurrentNonce     :: Nonce
    , aiTransactionCount :: Integer
    , aiTransactions     :: Maybe [GTxInfo]  -- TODO: remove this field?
    } deriving (Eq, Show, Generic)

type TxList = PaginatedList "transactions" TxInfo

type PublicationInfo = WithBlockInfo PublicationTx
type PublicationList = PaginatedList "publications" PublicationInfo

data HashIs
    = HashIsUnknown
    | HashIsBlock
    | HashIsAddress
    | HashIsMoneyTx
    | HashIsPublicationTx
    deriving (Eq, Show, Generic)

---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------

deriving instance (Eq (Id a), Eq a) => Eq (PaginatedList d a)
deriving instance (Show (Id a), Show a) => Show (PaginatedList d a)

instance Buildable ByteString where
    build _ = "<binary data, omitted>"

instance HasId a => HasId (WithBlockInfo a) where
    type Id (WithBlockInfo a) = Id a
    getId WithBlockInfo{..} = getId wbiItem

instance Buildable (ForResponseLog BlockInfo) where
    build (ForResponseLog BlockInfo{..}) =
        "{ headerHash = " +| biHeaderHash |+
        ", header = " +| biHeader |+
        " }"

instance Buildable FairCVAndCheckResult where
    build (FairCVAndCheckResult res fcv) =
        "{ fairCV = "  +| res |+
        ", checkResult = " +| fcv |+
        " }"

instance Buildable (ForResponseLog FairCVAndCheckResult) where
    build (ForResponseLog rfc) = build rfc

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

instance Buildable (ForResponseLog PublicationTxId) where
    build = buildForResponse

instance Buildable (ForResponseLog FairCVCheckResult) where
    build (ForResponseLog res) = build res

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''BlocksOrMempool
deriveJSON defaultOptions ''BlockList
deriveJSON defaultOptions ''FairCVAndCheckResult
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

instance ToJSON (Detailed Tx) where
    toJSON (Detailed tx) = mergeObjects extraFields (toJSON tx)
      where
        extraFields = object
            [ "txId" .= toTxId tx
            , case sumCoins . map txOutValue . txOuts $ tx of
                  Right c  -> "outValue" .= c
                  Left err -> "outValue" .= err
            ]

instance FromJSON (Detailed Tx) where
    parseJSON = fmap Detailed . parseJSON

instance ToJSON (Detailed PublicationTx) where
    toJSON (Detailed pTx) = mergeObjects extraFields (toJSON pTx)
        where
          extraFields = object [ "txId" .= toPtxId pTx ]

instance FromJSON (Detailed PublicationTx) where
    parseJSON = fmap Detailed . parseJSON

instance ToJSON (Detailed GTx) where
    toJSON (Detailed gtx) = mergeObjects (object [ "txType" .= txType]) txObj
      where
        (txType, txObj) = case gtx of
            GMoneyTx tx        -> ("money" :: Text, toJSON $ Detailed tx)
            GPublicationTx pTx -> ("publication" :: Text, toJSON $ Detailed pTx)

instance FromJSON (Detailed GTx) where
    parseJSON = withObject "detailed gtx" $ \o -> do
        txType :: Text <- o .: "txType"
        Detailed <$> case txType of
            "money"       -> GMoneyTx . unDetailed <$> parseJSON (Object o)
            "publication" -> GPublicationTx . unDetailed <$> parseJSON (Object o)
            other         -> fail $ "invalid transaction type: " ++ toString other

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
