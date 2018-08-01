
module Dscp.Core.Foundation.Transactions.Types
    (
       -- * Transaction
         Coin (..)
       , StakeholderId (..)
       , coinToInteger
       , coinFromInteger
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
       , GTxWitnessed (..)

       -- * Block
       , HeaderHash
       , Difficulty (..)
       , BlockToSign (..)
       , Header (..)
       , Block (..)
       , BlockBody (..)
    )
    where

import Codec.Serialise (Serialise)

import Fmt (blockListF, build, indentF, listF, nameF, (+|), (+||), (|+), (||+))

import Dscp.Core.Foundation.Educator.Block (PrivateHeaderHash)
import Dscp.Core.Types
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

-- | Safely convert coin to integer.
coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unCoin

coinFromInteger :: Integer -> Either Text Coin
coinFromInteger i
    | i < 0
        = Left "Negative coin amount"
    | i > fromIntegral (unCoin maxBound)
        = Left "Coin amount is too high"
    | otherwise
        = Right (Coin $ fromIntegral i)

instance Buildable Coin where
    build (Coin c) = c ||+ " coin(s)"



----------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------

-- We have several different types of "transactions". Money
-- transactions, delegation transactions (in the future). Money
-- transactions are the most popular, so we'll call them just
-- "transactions".

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
toTxId :: (Serialise Tx) => Tx -> TxId
toTxId = hash

-- | Compute tx id.
toPtxId :: (Serialise PublicationTx) => PublicationTx -> PublicationTxId
toPtxId = hash

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
data TxWitnessed = TxWitnessed
    { twTx      :: Tx
    , twWitness :: TxWitness
    } deriving (Eq, Show, Generic)

instance Buildable TxWitnessed where
    build TxWitnessed {..} =
        "TxWitnessed { " +| twTx |+ ", " +| twWitness |+  " }"

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

-- | Generalised version of transaction, other types to appear
-- here.
data GTx
    = GMoneyTx Tx
    | GPublicationTx PublicationTx
    deriving (Generic, Eq, Show)

instance Buildable GTx where
    build (GMoneyTx       tw) = "GMoneyTx: "       +| tw |+ ""
    build (GPublicationTx pw) = "GPublciationTx: " +| pw |+ ""

data GTxWitnessed
    = GMoneyTxWitnessed TxWitnessed
    | GPublicationTxWitnessed PublicationTxWitnessed
    deriving (Generic, Eq, Show)

instance Buildable GTxWitnessed where
    build (GMoneyTxWitnessed       tw) = "GMoneyTxWitnessed: " +| tw |+ ""
    build (GPublicationTxWitnessed pw) = "GPublicationTxWitnessed: " +| pw |+ ""

----------------------------------------------------------------------------
-- Blocks/Transaction
----------------------------------------------------------------------------

newtype Difficulty = Difficulty Word64
    deriving (Eq,Ord,Num,Show,Generic,Buildable)

-- | Blocks are indexed by their headers' hashes.
type HeaderHash = Hash Header

-- Part of the block we sign
data BlockToSign =
    BlockToSign Difficulty HeaderHash BlockBody
    deriving (Eq, Show, Generic)

data Header = Header
    { hSignature  :: Signature BlockToSign
    , hIssuer     :: PublicKey
    , hDifficulty :: Difficulty
    , hPrevHash   :: HeaderHash
    } deriving (Eq, Show, Generic)

instance Buildable Header where
    build Header{..} =
        "Header: " +|
        indentF 2 (blockListF [ nameF "sig" $ build hSignature
                              , nameF "issuer" $ build hIssuer
                              , nameF "difficulty" $ build hDifficulty
                              , nameF "prev" $ hashF hPrevHash ])

-- | Body of the block.
data BlockBody = BlockBody
    { rbbTxs :: [GTxWitnessed]
    } deriving (Eq, Show, Generic)

instance Buildable BlockBody where
    build (BlockBody txs) = listF txs

-- | Block.
data Block = Block
    { rbHeader :: Header
    , rbBody   :: BlockBody
    } deriving (Eq, Show, Generic)

instance Buildable Block where
    build = build . (show :: Block -> Text)
