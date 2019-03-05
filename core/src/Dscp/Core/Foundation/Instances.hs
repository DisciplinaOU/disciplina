{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Instances for foundation types.

module Dscp.Core.Foundation.Instances where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (Decoder, decodeListLenOf, decodeWord)
import Codec.Serialise.Encoding (Encoding, encodeListLen, encodeWord)
import Data.HList.CommonMain
import Data.Time.Calendar (Day (..))

import Dscp.Core.Foundation.Address
import Dscp.Core.Foundation.Coin
import Dscp.Core.Foundation.Educator
import Dscp.Core.Foundation.Witness
import Dscp.Crypto.Impl
import Dscp.Crypto.MerkleTree
import Dscp.Crypto.Serialise ()
import Dscp.Util

----------------------------------------------------------------------------
-- Helpers for Product types
----------------------------------------------------------------------------

-- | Decoding 'ApplyAB' function
data HDecode = HDecode

instance (dec ~ Decoder s x, Serialise x) => ApplyAB HDecode () dec where
    applyAB _ _ = decode

-- | Class for product types that automatically converts to/from Serialise
class ProductList p where
    -- | Type-level list of the product type fields
    type Attrs p :: [*]

    -- | Get the length as an 'Integral'
    prodListLength :: Integral i => Proxy p -> i
    default prodListLength
        :: (KnownNat (HNat2Nat (HLength (Attrs p))), Integral i)
        => Proxy p -> i
    prodListLength _ = hNat2Integral (Proxy :: Proxy (HLength (Attrs p)))

    -- | Heterogeneous list of the product type fields
    toProductList :: p -> HList (Attrs p)

    -- | Transforms a product type in an ordered list of it's field's 'Encoding'
    prodListToEncoding :: p -> [Encoding]
    default prodListToEncoding
        :: HFoldr (Mapcar (Fun Serialise Encoding)) [Encoding] (Attrs p) [Encoding]
        => p -> [Encoding]
    prodListToEncoding = hMapOut toSerial . toProductList
      where
        toSerial :: Fun Serialise Encoding
        toSerial = Fun encode

    -- | Make a product type from the heterogeneous list of its fields
    fromProductList :: HList (Attrs p) -> p

    -- | 'Decoder' for the product type (NOTE: not including the list length)
    prodListDecoder :: Decoder s p
    default prodListDecoder
        :: (HSequence (Decoder s) _d (Attrs p), HReplicateF (HLength (Attrs p)) HDecode () _d)
        => Decoder s p
    prodListDecoder = fromProductList <$> (hSequence $ hReplicateF Proxy HDecode ())

instance {-# OVERLAPPABLE #-} ( ProductList p ) => Serialise p where
    encode = mconcat . prependLength . prodListToEncoding
      where prependLength = (encodeListLen (prodListLength (Proxy :: Proxy p)) :)
    decode = do
        decodeListLenOf $ prodListLength (Proxy :: Proxy p)
        prodListDecoder

----------------------------------------------------------------------------
-- Educator
----------------------------------------------------------------------------

-- | Block
instance ProductList PrivateBlockHeader where
    type Attrs PrivateBlockHeader = '[PrivateHeaderHash, MerkleSignature PrivateTx, ATGDelta]
    toProductList (PrivateBlockHeader {..}) = hBuild _pbhPrevBlock _pbhBodyProof _pbhAtgDelta
    fromProductList = hUncurry PrivateBlockHeader

instance Serialise PrivateBlockBody where
    encode = encode . _pbbTxs
    decode = PrivateBlockBody <$> decode

instance ProductList PrivateBlock where
    type Attrs PrivateBlock = '[PrivateBlockHeader, PrivateBlockBody]
    toProductList (PrivateBlock {..}) = hBuild _pbHeader _pbBody
    fromProductList = hUncurry PrivateBlock

instance HasId PrivateBlock where
    type Id PrivateBlock = Hash PrivateBlockHeader

    getId = hash . _pbHeader

-- | Transactions
instance ProductList PrivateTx where
    type Attrs PrivateTx = '[SignedSubmission, Grade, Timestamp]
    toProductList (PrivateTx {..}) = hBuild _ptSignedSubmission _ptGrade _ptTime
    fromProductList = hUncurry PrivateTx

instance ProductList PrivateTxWitness where
    type Attrs PrivateTxWitness = '[PublicKey, PrivateTxSig]
    toProductList (PkWitness {..}) = hBuild _ptwKey _ptwSig
    fromProductList = hUncurry PkWitness

instance ProductList PrivateTxAux where
    type Attrs PrivateTxAux = '[PrivateTx, PrivateTxWitness]
    toProductList (PrivateTxAux {..}) = hBuild _ptaTx _ptaWitness
    fromProductList = hUncurry PrivateTxAux

instance HasId PrivateTx where
    type Id PrivateTx = Hash PrivateTx
    getId = hash

instance Serialise Course where
    encode = encode . getCourseId
    decode = Course <$> decode

instance Serialise Subject where
    encode = encode . getSubjectId
    decode = Subject <$> decode

instance Serialise Grade where
    encode = encode . getGrade
    decode = nothingToFail "Invalid Grade Value" . mkGrade =<< decode

instance ProductList ATGNode where
    type Attrs ATGNode = '[Id Subject, [ATGEdge]]
    toProductList (ATGNode {..}) = hBuild _atgnSubjectId _atgnChildren
    fromProductList = hUncurry ATGNode

instance ProductList ATGEdge where
    type Attrs ATGEdge = '[Float, ATGNode]
    toProductList (ATGEdge {..}) = hBuild _atgeWeight _atgeChild
    fromProductList = hUncurry ATGEdge

instance Serialise ATGSubjectChange where
    encode a = encodeListLen 1
            <> encodeWord (case a of
                ATGAdded   -> 0
                ATGRemoved -> 1
                )
    decode = do
        decodeListLenOf 1
        decodeWord >>= \case
            0 -> return ATGAdded
            1 -> return ATGRemoved
            _ -> fail "Unexpected tag"

instance Serialise ATGDelta where
    encode = encode . getATGDelta
    decode = ATGDelta <$> decode

instance Serialise ATG where
    encode = encode . getATGRoots
    decode = ATG <$> decode

instance ProductList Assignment where
    type Attrs Assignment = '[Id Course, Hash Raw, AssignmentType, ItemDesc]
    toProductList (Assignment {..}) = hBuild _aCourseId _aContentsHash _aType _aDesc
    fromProductList = hUncurry Assignment

instance Serialise AssignmentType where
    encode a = encodeListLen 1
            <> encodeWord (case a of
                Regular     -> 0
                CourseFinal -> 1
                )
    decode = do
        decodeListLenOf 1
        decodeWord >>= \case
            0 -> return Regular
            1 -> return CourseFinal
            _ -> fail "Unexpected tag"

instance ProductList SubmissionWitness where
    type Attrs SubmissionWitness = '[PublicKey, SubmissionSig]
    toProductList (SubmissionWitness {..}) = hBuild _swKey _swSig
    fromProductList = hUncurry SubmissionWitness

instance ProductList SignedSubmission where
    type Attrs SignedSubmission = '[Submission, SubmissionWitness]
    toProductList (SignedSubmission {..}) = hBuild _ssSubmission _ssWitness
    fromProductList = hUncurry SignedSubmission

instance Serialise (DocumentType a) where
    encode a = encodeListLen 1
            <> encodeWord (case a of
                Online  -> 0
                Offline -> 1
                )
    decode = do
        decodeListLenOf 1
        decodeWord >>= \case
            0 -> return Online
            1 -> return Offline
            _ -> fail "Unexpected tag"

instance Serialise Day where
    encode = encode . toModifiedJulianDay
    decode = ModifiedJulianDay <$> decode

instance Serialise EducationForm where
    encode a = encodeListLen 1
            <> encodeWord (case a of
                Fulltime -> 0
                Parttime -> 1
                Fullpart -> 2
                )
    decode = do
        decodeListLenOf 1
        decodeWord >>= \case
            0 -> return Fulltime
            1 -> return Parttime
            2 -> return Fullpart
            _ -> fail "Unexpected tag"

instance ProductList CertificateMeta where
    type Attrs CertificateMeta =
           '[ ItemDesc
            , Day
            , Word16
            , Word16
            , EducationForm
            , Natural
            , Day
            , ItemDesc
            , ItemDesc
            , Maybe ItemDesc
            ]
    toProductList (CertificateMeta {..}) = hBuild
        cmStudentName
        cmStudentBirthDate
        cmStartYear
        cmEndYear
        cmEducationForm
        cmNumber
        cmIssueDate
        cmTitle
        cmMajor
        cmSpecialization
    fromProductList = hUncurry CertificateMeta

instance HasId CertificateMeta where
    type Id CertificateMeta = Hash CertificateMeta
    getId = hash

instance HasId CertificateFullInfo where
    type Id CertificateFullInfo = Id CertificateMeta
    getId = getId . cfiMeta

instance ProductList Submission where
    type Attrs Submission = '[Id Student, Hash Raw, Hash Assignment]
    toProductList (Submission {..}) = hBuild _sStudentId _sContentsHash _sAssignmentHash
    fromProductList = hUncurry Submission

instance Serialise ItemDesc where
    encode t = encode $ unItemDesc t
    decode = leftToFail . toItemDesc =<< decode

instance Serialise Timestamp where
    encode t = encode $ unTimestamp t
    decode = toTimestamp <$> decode

----------------------------------------------------------------------------
-- Witness
----------------------------------------------------------------------------

-- | TODO: CBOR uncompromisingly seralises numbers in variable-length manner,
-- but we actually don't want fees to increase over time as corresponding
-- account is used.
instance Serialise Nonce where
    encode = encode . unNonce
    decode = Nonce <$> decode

instance Serialise Coin where
    encode = encode . unCoin
    decode = Coin <$> decode

instance ProductList TxInAcc where
    type Attrs TxInAcc = '[Address, Nonce]
    toProductList (TxInAcc {..}) = hBuild tiaAddr tiaNonce
    fromProductList = hUncurry TxInAcc

instance ProductList TxOut where
    type Attrs TxOut = '[Address, Coin]
    toProductList (TxOut {..}) = hBuild txOutAddr txOutValue
    fromProductList = hUncurry TxOut

instance ProductList Tx where
    type Attrs Tx = '[TxInAcc, Coin, [TxOut]]
    toProductList (Tx {..}) = hBuild txInAcc txInValue txOuts
    fromProductList = hUncurry Tx

instance ProductList TxWitness where
    type Attrs TxWitness = '[Signature (TxId, PublicKey, ()), PublicKey]
    toProductList (TxWitness {..}) = hBuild txwSig txwPk
    fromProductList = hUncurry TxWitness

instance ProductList TxWitnessed where
    type Attrs TxWitnessed = '[Tx, TxWitness]
    toProductList (TxWitnessed {..}) = hBuild twTx twWitness
    fromProductList = hUncurry TxWitnessed

instance ProductList PublicationTx where
    type Attrs PublicationTx = '[Address, Coin, PrivateBlockHeader]
    toProductList (PublicationTx {..}) = hBuild ptAuthor ptFeesAmount ptHeader
    fromProductList = hUncurry PublicationTx

instance ProductList PublicationTxWitness where
    type Attrs PublicationTxWitness =
            '[Signature (PublicationTxId, PublicKey, PrivateBlockHeader), PublicKey]
    toProductList (PublicationTxWitness {..}) = hBuild pwSig pwPk
    fromProductList = hUncurry PublicationTxWitness

instance ProductList PublicationTxWitnessed where
    type Attrs PublicationTxWitnessed = '[PublicationTx, PublicationTxWitness]
    toProductList (PublicationTxWitnessed {..}) = hBuild ptwTx ptwWitness
    fromProductList = hUncurry PublicationTxWitnessed

instance Serialise GTx where
    encode = \case
        GMoneyTx tx -> mconcat
            [ encodeListLen 2
            , encodeWord 0
            , encode tx
            ]
        GPublicationTx publicationTx -> mconcat
            [ encodeListLen 2
            , encodeWord 1
            , encode publicationTx
            ]
    decode = do
        decodeListLenOf 2
        decodeWord >>= \case
            0 -> GMoneyTx       <$> decode
            1 -> GPublicationTx <$> decode
            _ -> fail "Unexpected tag"

instance Serialise GTxId where
    encode (GTxId hGTx) = encode hGTx
    decode = GTxId <$> decode

instance Serialise GTxWitnessed where
    encode = \case
        GMoneyTxWitnessed txWitnessed -> mconcat
            [ encodeListLen 2
            , encodeWord 0
            , encode txWitnessed
            ]
        GPublicationTxWitnessed publicationTxWitnessed -> mconcat
            [ encodeListLen 2
            , encodeWord 1
            , encode publicationTxWitnessed
            ]
    decode = do
        decodeListLenOf 2
        decodeWord >>= \case
            0 -> GMoneyTxWitnessed       <$> decode
            1 -> GPublicationTxWitnessed <$> decode
            _ -> fail "Unexpected tag"

instance ProductList BlockToSign where
    type Attrs BlockToSign = '[Difficulty, SlotId, HeaderHash, Hash BlockBody]
    toProductList (BlockToSign difficulty slotId hHeader hBlockBody) =
        hBuild difficulty slotId hHeader hBlockBody
    fromProductList = hUncurry BlockToSign

instance Serialise SlotId where
    encode (SlotId w) = encode w
    decode = SlotId <$> decode

instance Serialise Difficulty where
    encode = encode . unDifficulty
    decode = Difficulty <$> decode

instance ProductList Header where
    type Attrs Header = '[Signature BlockToSign, PublicKey, Difficulty, SlotId, HeaderHash]
    toProductList (Header {..}) = hBuild hSignature hIssuer hDifficulty hSlotId hPrevHash
    fromProductList = hUncurry Header

instance ProductList Block where
    type Attrs Block = '[Header, BlockBody]
    toProductList (Block {..}) = hBuild bHeader bBody
    fromProductList = hUncurry Block

instance Serialise BlockBody where
    encode = encode . bbTxs
    decode = BlockBody <$> decode
