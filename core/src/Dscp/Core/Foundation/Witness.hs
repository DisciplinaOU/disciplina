-- | Common types for main witness node.
module Dscp.Core.Foundation.Witness
    (
    -- * Common
      Coin (..)
    , unsafeAddCoin
    , StakeholderId (..)
    , coinToInteger
    , coinFromInteger
    , unsafeMkCoin
    , sumCoins
    , SlotId (..)

    -- * Transaction
    , TxInAcc (..)
    , TxOut (..)
    , Tx (..)
    , TxId
    , toTxId
    , TxWitness (..)
    , TxWitnessed (..)
    , Publication (..)
    , PublicationTxWitness (..)
    , PublicationTxWitnessed (..)
    , PublicationTx (..)
    , PublicationTxId
    , PublicationsOf (..)
    , LastPublication (..)
    , PublicationHead (..)
    , PublicationNext (..)
    , toPtxId
    , GTx (..)
    , GTxId (..)
    , toGTxId
    , GTxWitnessed (..)
    , unGTxWitnessed

    -- * Block
    , Difficulty (..)
    , HeaderHash
    , BlockToSign (..)
    , Header (..)
    , Block (..)
    , BlockBody (..)
    , HasHeaderHash (..)
    ) where

import Codec.Serialise (Serialise)
import Data.Coerce (coerce)

import Fmt (blockListF, build, indentF, listF, nameF, (+|), (+||), (|+), (||+))

import Dscp.Core.Foundation.Address
import Dscp.Core.Foundation.Educator (PrivateHeaderHash)
import Dscp.Crypto

----------------------------------------------------------------------------
-- General
----------------------------------------------------------------------------


newtype StakeholderId = StakeholderId
    { unStakeholderId :: PublicKey
    } deriving (Eq, Show, Generic)

-- This is all naive for now, should be moved to the separate module later

-- | Coin amount.
newtype Coin = Coin { unCoin :: Word64 }
    deriving (Eq, Ord, Show, Generic, Hashable, Bounded)

-- | Add coins.
unsafeAddCoin :: Coin -> Coin -> Coin
unsafeAddCoin a b = case coinFromInteger $ coinToInteger a + coinToInteger b of
    Left e  -> error $ "unsafeCoin failed: " <> show (a,b) <> " " <> e
    Right x -> x

-- | Safely convert coin to integer.
coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unCoin

-- | Restore coin from integer.
coinFromInteger :: Integer -> Either Text Coin
coinFromInteger i
    | i < 0
        = Left "Negative coin amount"
    | i > fromIntegral (unCoin maxBound)
        = Left "Coin amount is too high"
    | otherwise
        = Right (Coin $ fromIntegral i)

-- | Same as 'coinFromInteger', but errors if Left happens.
unsafeMkCoin :: Integral i => i -> Coin
unsafeMkCoin = Coin . fromIntegral -- also do checks

instance Buildable Coin where
    build (Coin c) = c ||+ " coin(s)"

sumCoins :: Coin -> Coin -> Coin
sumCoins = coerce $ (+) @Word64

----------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------

-- We have several different types of "transactions". Money
-- transactions, delegation transactions (in the future). Money
-- transactions are the most popular, so we'll call them just
-- "transactions".

----------------------------------------------------------------------------
-- Money transactions
----------------------------------------------------------------------------

-- | Tx input account. Can be used for other tx types too.
data TxInAcc = TxInAcc
    { tiaAddr  :: Address
    , tiaNonce :: Integer
    } deriving (Eq, Ord, Generic, Show)

instance Buildable TxInAcc where
    build TxInAcc{..} = "TxInnAcc {" +| tiaAddr |+ " nonce " +|| tiaNonce ||+ "}"

-- | Money transaction output.
data TxOut = TxOut
    { txOutAddr  :: Address
    , txOutValue :: Coin
    } deriving (Eq, Ord, Generic, Show)

instance Buildable TxOut where
    build TxOut{..} = "<" +| txOutAddr |+ ", " +| txOutValue |+ ">"

-- | Transaction. Accounting-style money transfer.
data Tx = Tx
    { txInAcc   :: TxInAcc
    , txInValue :: Coin
    , txOuts    :: [TxOut]
    } deriving (Eq, Ord, Generic, Show)

instance Buildable Tx where
    build Tx{..} =
        "Tx { from: " +| txInAcc |+ "; inValue: " +| txInValue |+ "; outs:" +| listF txOuts |+ " }"

type TxId = Hash Tx

-- | Compute tx id.
toTxId :: (Serialise Tx) => Tx -> TxId
toTxId = hash

-- | Transaction witness. We sign a pair of transaction hash and private
-- key. The second element is there to authenticate proposed changes
-- (@kirill.andreev). Public key hash should be equal to the input address.
-- Also, public key should be the same which used to validate signature.
data TxWitness = TxWitness
    { txwSig :: Signature (TxId, PublicKey, ())
    , txwPk  :: PublicKey
    } deriving (Eq, Show, Generic)

instance Buildable TxWitness where
    build TxWitness {..} =
        "TxWitness { " +| txwSig |+ ", pk: " +| txwPk |+ " }"

-- | Transaction coupled with witness.
data TxWitnessed = TxWitnessed
    { twTx      :: Tx
    , twWitness :: TxWitness
    } deriving (Eq, Show, Generic)

instance Buildable TxWitnessed where
    build TxWitnessed {..} =
        "TxWitnessed { " +| twTx |+ ", " +| twWitness |+  " }"

----------------------------------------------------------------------------
-- Publication transactions
----------------------------------------------------------------------------

-- | Transaction for private block publications.
data PublicationTx = PublicationTx
    { ptAuthor    :: Address
    , ptPrevBlock :: Maybe PrivateHeaderHash
    , ptBlock     :: PrivateHeaderHash
    } deriving (Eq, Ord, Generic, Show)

instance Buildable PublicationTx where
    build PublicationTx { ptAuthor, ptPrevBlock, ptBlock } =
        "Tx { from: " +| ptAuthor |+ "; prev block: " +| ptPrevBlock |+ "; block:" +| ptBlock |+ " }"

type PublicationTxId = Hash PublicationTx

-- | Compute tx id.
toPtxId :: (Serialise PublicationTx) => PublicationTx -> PublicationTxId
toPtxId = hash

-- | Incoming publication message; transaction payload.
data Publication = Publication
    { pPrivateBlockHash  :: PrivateHeaderHash
    , pPreviousBlockHash :: Maybe PrivateHeaderHash
    }
    deriving (Eq, Ord, Show, Generic)

data PublicationTxWitness = PublicationTxWitness
    { pwSig :: Signature (PublicationTxId, PublicKey, Publication)
    , pwPk  :: PublicKey
    } deriving (Eq, Show, Generic)

instance Buildable PublicationTxWitness where
    build PublicationTxWitness {..} =
        "PublicationTxWitness { " +| pwSig |+ ", pk: " +| pwPk |+ " }"

-- | Transaction coupled with witness.
data PublicationTxWitnessed = PublicationTxWitnessed
    { ptwTx      :: PublicationTx
    , ptwWitness :: PublicationTxWitness
    } deriving (Eq, Show, Generic)

instance Buildable PublicationTxWitnessed where
    build PublicationTxWitnessed {..} =
        "PublicationTxWitnessed { " +| ptwTx |+ ", " +| ptwWitness |+  " }"

-- | Actual structure in the storage.
-- |
-- | For each educator, we'll have a `PublicationOf id ~> LastPublication blockHash`
-- | (which is effectively `(educator, lastBlockHash)`)
-- | to determine the lst publication.
newtype PublicationsOf
    = PublicationsOf Address
    deriving (Eq, Ord, Show, Generic)

instance Buildable PublicationsOf where
    build (PublicationsOf addr) =
        "PublicationsOf { " +| addr |+  " }"

-- | Points to the `PublicationHead addr` in the database.
data LastPublication
    = LastPublication PrivateHeaderHash
    deriving (Eq, Ord, Show, Generic)

-- | Once 'LastPublication' is known, you can walk the chain of
-- | `PublicationHead bh ~> PublicationNext bh`,
-- | (which is `(blockHash, Maybe blockHash)`)
-- | where phead contains block hash and pnext has prev block hash.
newtype PublicationHead
    = PublicationHead PrivateHeaderHash
    deriving (Eq, Ord, Show, Generic)

instance Buildable PublicationHead where
    build (PublicationHead blk) =
        "PublicationHead { " +| blk |+  " }"

data PublicationNext
    = PublicationNext (Maybe PrivateHeaderHash)
    deriving (Eq, Ord, Show, Generic)

----------------------------------------------------------------------------
-- Transactions (united)
----------------------------------------------------------------------------

-- | Generalised version of transaction, other types to appear
-- here.
data GTx
    = GMoneyTx Tx
    | GPublicationTx PublicationTx
    deriving (Generic, Eq, Show)

instance Buildable GTx where
    build (GMoneyTx       tw) = "GMoneyTx: "       +| tw |+ ""
    build (GPublicationTx pw) = "GPublciationTx: " +| pw |+ ""

-- | Unified tx id, which is actually a hash of underlying Tx or PublicationTx.
-- NB! It's different from Hash GTx.
newtype GTxId = GTxId (Hash GTx)
    deriving (Eq, Ord, Show, Generic, Buildable)

-- | Compute tx id.
toGTxId :: (Serialise Tx, Serialise PublicationTx) => GTx -> GTxId
toGTxId (GMoneyTx tx) = GTxId . unsafeCastHash . toTxId $ tx
toGTxId (GPublicationTx tx) = GTxId . unsafeCastHash . toPtxId $ tx

data GTxWitnessed
    = GMoneyTxWitnessed TxWitnessed
    | GPublicationTxWitnessed PublicationTxWitnessed
    deriving (Generic, Eq, Show)

instance Buildable GTxWitnessed where
    build (GMoneyTxWitnessed       tw) = "GMoneyTxWitnessed: " +| tw |+ ""
    build (GPublicationTxWitnessed pw) = "GPublicationTxWitnessed: " +| pw |+ ""

unGTxWitnessed :: GTxWitnessed -> GTx
unGTxWitnessed (GMoneyTxWitnessed tw) = GMoneyTx (twTx tw)
unGTxWitnessed (GPublicationTxWitnessed tw) = GPublicationTx (ptwTx tw)

----------------------------------------------------------------------------
-- Blocks and headers
----------------------------------------------------------------------------

-- | Slot id.
newtype SlotId = SlotId Word64
    deriving (Eq, Ord, Num, Show, Generic, Buildable)

-- | Chain difficulty.
newtype Difficulty = Difficulty { unDifficulty :: Word64 }
    deriving (Eq, Ord, Num, Show, Generic, Buildable)

-- | Blocks are indexed by their headers' hashes.
type HeaderHash = Hash Header

-- Part of the block we sign
data BlockToSign =
    BlockToSign Difficulty SlotId HeaderHash  BlockBody
    deriving (Eq, Show, Generic)

data Header = Header
    { hSignature  :: !(Signature BlockToSign)
    , hIssuer     :: !PublicKey
    , hDifficulty :: !Difficulty
    , hSlotId     :: !SlotId
    , hPrevHash   :: !HeaderHash
    } deriving (Eq, Show, Generic)

instance HasHash Header => Buildable Header where
    build h@Header{..} =
        "Header:\n" +|
        indentF 2 (blockListF [ nameF "sig" $ build hSignature
                              , nameF "issuer" $ build hIssuer
                              , nameF "slotId" $ build hSlotId
                              , nameF "difficulty" $ build hDifficulty
                              , nameF "prev" $ hashF hPrevHash
                              , nameF "headerHash" $ hashF (hash h)
                              ])

-- | Body of the block.
data BlockBody = BlockBody
    { bbTxs :: ![GTxWitnessed]
    } deriving (Eq, Show, Generic)

instance Buildable BlockBody where
    build (BlockBody txs) = listF txs

-- | Block.
data Block = Block
    { bHeader :: !Header
    , bBody   :: !BlockBody
    } deriving (Eq, Show, Generic)

instance HasHash Header => Buildable Block where
    build Block{..} =
        "Block { \nheader: " +| bHeader |+ ", body: " +| bBody |+ " }"

----------------------------------------------------------------------------
-- Lens and classes
----------------------------------------------------------------------------

-- | Class for things that have headerHash.
class HasHeaderHash d where
    headerHash :: d -> HeaderHash

instance HasHeaderHash HeaderHash where
    headerHash = identity

instance HasHash Header => HasHeaderHash Header where
    headerHash = hash

instance HasHash Header => HasHeaderHash Block where
    headerHash = headerHash . bHeader
