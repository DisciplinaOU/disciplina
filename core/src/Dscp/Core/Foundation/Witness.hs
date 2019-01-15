{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Common types for main witness node.
module Dscp.Core.Foundation.Witness
    (
    -- * Common
      SecretKeyData (..)
    , mkSecretKeyData
    , secretKeyDataFromPair
    , StakeholderId (..)
    , SlotId (..)

    -- * Transaction
    , Nonce (..)
    , TxInAcc (..)
    , tiaAddrL
    , tiaNonceL
    , TxOut (..)
    , txOutAddrL
    , txOutValueL
    , Tx (..)
    , txInAccL
    , txInValueL
    , txOutsL
    , TxId
    , toTxId
    , TxWitness (..)
    , txwSigL
    , txwPkL
    , TxWitnessed (..)
    , twTxL
    , twWitnessL
    , PublicationTxWitness (..)
    , PublicationTxWitnessed (..)
    , ptwTxL
    , ptwWitnessL
    , PublicationTx (..)
    , ptAuthorL
    , ptFeesAmountL
    , ptHeaderL
    , PublicationTxId
    , toPtxId
    , BlockMetaTx (..)
    , GTx (..)
    , _GMoneyTx
    , _GPublicationTx
    , GTxId (..)
    , toGTxId
    , GTxWitnessed (..)
    , _GMoneyTxWitnessed
    , _GPublicationTxWitnessed
    , unGTxWitnessed
    , toGTxwId
    , GTxInBlock (..)
    , WithBlock (..)

    -- * Block
    , Difficulty (..)
    , HeaderHash
    , BlockToSign (..)
    , Header (..)
    , hIssuerL
    , hSignatureL
    , hSlotIdL
    , hDifficultyL
    , hPrevHashL
    , Block (..)
    , bHeaderL
    , bBodyL
    , BlockBody (..)
    , bbTxsL
    , HasHeaderHash (..)
    ) where

import Codec.Serialise (Serialise)
import Control.Lens (makeLensesWith)
import Control.Lens (makePrisms)
import Data.Data (Data)
import System.Random (Random)

import Fmt (blockListF, build, indentF, listF, nameF, whenF, (+|), (+||), (|+), (||+))

import Dscp.Core.Foundation.Address
import Dscp.Core.Foundation.Coin
import Dscp.Core.Foundation.Educator
import Dscp.Crypto.Hash.Class (hashF)
import Dscp.Crypto.Impl
import Dscp.Util

----------------------------------------------------------------------------
-- General
----------------------------------------------------------------------------

-- | Carries all data related to a secret key, evaluating it lazily.
data SecretKeyData = SecretKeyData
    { skSecret  :: !SecretKey
    , skPublic  :: PublicKey
    , skAddress :: Address
    } deriving (Show)

instance Eq SecretKeyData where
    (==) = (==) `on` skSecret

instance Ord SecretKeyData where
    compare = compare `on` skSecret

-- | Make secret key data.
mkSecretKeyData :: SecretKey -> SecretKeyData
mkSecretKeyData sk =
    let skSecret = sk
        skPublic = toPublic sk
        skAddress = mkAddr skPublic
    in SecretKeyData{..}

-- | Make secret key data when public key is already known.
secretKeyDataFromPair :: (SecretKey, PublicKey) -> SecretKeyData
secretKeyDataFromPair (skSecret, skPublic) =
    let skAddress = mkAddr skPublic
    in SecretKeyData{..}

newtype StakeholderId = StakeholderId
    { unStakeholderId :: PublicKey
    } deriving (Eq, Show, Generic)

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

-- | Count of transactions originated _from_ given account, modulo @2^32@.
newtype Nonce = Nonce { unNonce :: Word32 }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Bounded, Data, Generic)

instance Buildable Nonce where
    build (Nonce n) = "#" +| n |+ ""

-- | Tx input account. Can be used for other tx types too.
data TxInAcc = TxInAcc
    { tiaAddr  :: !Address
    , tiaNonce :: !Nonce
    } deriving (Eq, Ord, Generic, Show)

instance Buildable TxInAcc where
    build TxInAcc{..} = "TxInnAcc {" +| tiaAddr |+ " nonce " +|| tiaNonce ||+ "}"

-- | Money transaction output.
data TxOut = TxOut
    { txOutAddr  :: !Address
    , txOutValue :: !Coin
    } deriving (Eq, Ord, Generic, Show)

instance Buildable TxOut where
    build TxOut{..} = "<" +| txOutAddr |+ ", " +| txOutValue |+ ">"

-- | Transaction. Accounting-style money transfer.
data Tx = Tx
    { txInAcc   :: !TxInAcc
    , txInValue :: !Coin
    , txOuts    :: ![TxOut]
    } deriving (Eq, Ord, Generic, Show)

instance Buildable Tx where
    build Tx{..} =
        "Tx { from: " +| txInAcc |+ "; inValue: " +| txInValue |+ "; outs:" +| listF txOuts |+ " }"

type TxId = Hash Tx

-- | Compute tx id.
toTxId :: (Serialise Tx) => Tx -> TxId
toTxId = hash

instance Serialise Tx => HasId Tx where
    type Id Tx = TxId
    getId = toTxId

-- | Transaction witness. We sign a pair of transaction hash and private
-- key. The third element is there to authenticate proposed changes
-- (@kirill.andreev). Public key hash should be equal to the input address.
-- Also, public key should be the same which used to validate signature.
data TxWitness = TxWitness
    { txwSig :: !(Signature (TxId, PublicKey))  -- TODO: do we need 'PublicKey' here?
    , txwPk  :: !PublicKey
    } deriving (Eq, Ord, Show, Generic)

instance Buildable TxWitness where
    build TxWitness {..} =
        "TxWitness { " +| txwSig |+ ", pk: " +| txwPk |+ " }"

-- | Transaction coupled with witness.
data TxWitnessed = TxWitnessed
    { twTx      :: !Tx
    , twWitness :: !TxWitness
    } deriving (Eq, Ord, Show, Generic)

instance Buildable TxWitnessed where
    build TxWitnessed {..} =
        "TxWitnessed { " +| twTx |+ ", " +| twWitness |+  " }"

----------------------------------------------------------------------------
-- Publication transactions
----------------------------------------------------------------------------

-- | Transaction for private block publications.
data PublicationTx = PublicationTx
    { ptAuthor     :: Address
      -- ^ Publication author.
    , ptFeesAmount :: Coin
      -- ^ Fees author spends.
    , ptHeader     :: PrivateBlockHeader
      -- ^ Private block header to publish.
    } deriving (Eq, Ord, Generic, Show)

instance Buildable PublicationTx where
    build PublicationTx { ptAuthor, ptFeesAmount, ptHeader } =
        "PublicationTx { author: " +| ptAuthor |+
        "; fees: " +| ptFeesAmount |+
        "; header:" +| ptHeader |+ " }"

type PublicationTxId = Hash PublicationTx

-- | Compute tx id.
toPtxId :: (Serialise PublicationTx) => PublicationTx -> PublicationTxId
toPtxId = hash

instance Serialise PublicationTx => HasId PublicationTx where
    type Id PublicationTx = PublicationTxId
    getId = toPtxId

-- I find this third element hack to be terrible tbh. @volhovm
--
-- | Publication witness. As with 'TxWitness', the third element is
-- needed for a better compatibility with snowdrop.
data PublicationTxWitness = PublicationTxWitness
    { pwSig :: Signature (PublicationTxId, PublicKey, PrivateBlockHeader)
    , pwPk  :: PublicKey
    } deriving (Eq, Ord, Show, Generic)

instance Buildable PublicationTxWitness where
    build PublicationTxWitness {..} =
        "PublicationTxWitness { " +| pwSig |+ ", pk: " +| pwPk |+ " }"

-- | Transaction coupled with witness.
data PublicationTxWitnessed = PublicationTxWitnessed
    { ptwTx      :: PublicationTx
    , ptwWitness :: PublicationTxWitness
    } deriving (Eq, Ord, Show, Generic)

instance Buildable PublicationTxWitnessed where
    build PublicationTxWitnessed {..} =
        "PublicationTxWitnessed { " +| ptwTx |+ ", " +| ptwWitness |+  " }"

----------------------------------------------------------------------------
-- Block meta
----------------------------------------------------------------------------

-- | Transaction which changes block-level data.
-- It works like separate transaction type because this is the only way to
-- validate, e.g. difficulty change.
--
-- Note, that this is just a helper in computation, it is neither stored nor submitted
-- over network. Nevertheless it is treated like transaction, being passed through
-- expansion and validation.
newtype BlockMetaTx = BlockMetaTx
    { bmtBlock :: Block
    } deriving (Eq, Ord, Show, Generic)

instance Buildable Header => Buildable BlockMetaTx where
    build (BlockMetaTx{..}) = "BlockMetaTx { " +| bHeader bmtBlock |+ " }"

----------------------------------------------------------------------------
-- Transactions (united)
----------------------------------------------------------------------------

-- | Generalised version of transaction, other types to appear
-- here.
data GTx
    = GMoneyTx Tx
    | GPublicationTx PublicationTx
    deriving (Generic, Eq, Show)

instance Buildable Header => Buildable GTx where
    build (GMoneyTx       tw) = "GMoneyTx: "       +| tw |+ ""
    build (GPublicationTx pw) = "GPublciationTx: " +| pw |+ ""

-- | Unified tx id, which is actually a hash of underlying Tx or PublicationTx.
-- NB! It's different from Hash GTx.
newtype GTxId = GTxId (Hash GTx)
    deriving (Eq, Ord, Show, Generic, Buildable)

-- | Compute tx id.
toGTxId :: (Serialise Tx, Serialise PublicationTx) => GTx -> GTxId
toGTxId (GMoneyTx tx)       = GTxId . unsafeCastHash . toTxId $ tx
toGTxId (GPublicationTx tx) = GTxId . unsafeCastHash . toPtxId $ tx

instance (Serialise Tx, Serialise PublicationTx) => HasId GTx where
    type Id GTx = GTxId
    getId = toGTxId

data GTxWitnessed
    = GMoneyTxWitnessed TxWitnessed
    | GPublicationTxWitnessed PublicationTxWitnessed
    deriving (Generic, Eq, Ord, Show)

instance Buildable GTxWitnessed where
    build (GMoneyTxWitnessed       tw) = "GMoneyTxWitnessed: " +| tw |+ ""
    build (GPublicationTxWitnessed pw) = "GPublicationTxWitnessed: " +| pw |+ ""

unGTxWitnessed :: GTxWitnessed -> GTx
unGTxWitnessed (GMoneyTxWitnessed tw)       = GMoneyTx (twTx tw)
unGTxWitnessed (GPublicationTxWitnessed tw) = GPublicationTx (ptwTx tw)

toGTxwId
    :: (Serialise Tx, Serialise PublicationTx)
    => GTxWitnessed -> GTxId
toGTxwId = toGTxId . unGTxWitnessed

-- | Transaction with reference to block it is published in
data GTxInBlock = GTxInBlock
    { tbBlock :: Maybe Block
    , tbTx    :: GTxWitnessed
    }
    deriving (Eq, Show, Generic)

-- | Something with reference to block which contains it.
data WithBlock a = WithBlock
    { wbBlock :: Maybe Block
    , wbItem  :: a
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

----------------------------------------------------------------------------
-- Blocks and headers
----------------------------------------------------------------------------

-- | Slot id.
newtype SlotId = SlotId Word64
    deriving (Eq, Ord, Num, Enum, Show, Generic, Buildable, Random)

-- | Chain difficulty.
newtype Difficulty = Difficulty { unDifficulty :: Word64 }
    deriving (Eq, Ord, Num, Show, Generic, Buildable)

-- | Blocks are indexed by their headers' hashes.
type HeaderHash = Hash Header

-- Part of the block we sign
data BlockToSign =
    BlockToSign Difficulty SlotId HeaderHash (Hash BlockBody)
    deriving (Eq, Show, Generic)

data Header = Header
    { hSignature  :: !(Signature BlockToSign)
    , hIssuer     :: !PublicKey
    , hDifficulty :: !Difficulty
    , hSlotId     :: !SlotId
    , hPrevHash   :: !HeaderHash
    } deriving (Eq, Ord, Show, Generic)

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
    } deriving (Eq, Ord, Show, Generic)

instance Buildable BlockBody where
    build (BlockBody txs) =
        listF displayedTxs +|
        whenF (n > displayed) (" + "+|left|+" more transactions.")
      where
        n = length txs
        displayed = 10
        displayedTxs = take displayed txs
        left = n - displayed

-- | Block.
data Block = Block
    { bHeader :: !Header
    , bBody   :: !BlockBody
    } deriving (Eq, Ord, Show, Generic)

instance HasHash Header => Buildable Block where
    build Block{..} =
        "Block { \nheader: " +| bHeader |+ ", body: " +| bBody |+ " }"

----------------------------------------------------------------------------
-- Lens and classes
----------------------------------------------------------------------------


makeLensesWith postfixLFields ''TxInAcc
makeLensesWith postfixLFields ''TxOut
makeLensesWith postfixLFields ''Tx
makeLensesWith postfixLFields ''TxWitness
makeLensesWith postfixLFields ''TxWitnessed
makeLensesWith postfixLFields ''PublicationTx
makeLensesWith postfixLFields ''PublicationTxWitnessed
makePrisms ''GTx
makePrisms ''GTxWitnessed

makeLensesWith postfixLFields ''Block
makeLensesWith postfixLFields ''Header
makeLensesWith postfixLFields ''BlockBody

-- | Class for things that have headerHash.
class HasHeaderHash d where
    headerHash :: d -> HeaderHash

instance HasHeaderHash HeaderHash where
    headerHash = identity

instance HasHash Header => HasHeaderHash Header where
    headerHash = hash

instance HasHash Header => HasHeaderHash Block where
    headerHash = headerHash . bHeader
