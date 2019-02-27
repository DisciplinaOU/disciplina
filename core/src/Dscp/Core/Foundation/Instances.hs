-- | Instances for foundaion types.

module Dscp.Core.Foundation.Instances where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Data.Time.Calendar (Day (..))

import Dscp.Core.Foundation.Coin
import Dscp.Core.Foundation.Educator
import Dscp.Core.Foundation.Witness
import Dscp.Crypto.Impl
import Dscp.Crypto.Serialise ()
import Dscp.Util

----------------------------------------------------------------------------
-- Educator
----------------------------------------------------------------------------

-- | Block
instance Serialise PrivateBlockHeader where
    encode (PrivateBlockHeader {..}) = mconcat
        [ encodeListLen 3
        , encode _pbhPrevBlock
        , encode _pbhBodyProof
        , encode _pbhAtgDelta
        ]

    decode = do
        decodeListLenOf 3
        _pbhPrevBlock <- decode
        _pbhBodyProof <- decode
        _pbhAtgDelta  <- decode
        return $ PrivateBlockHeader {..}

instance Serialise PrivateBlockBody where
    encode = encode . _pbbTxs
    decode = PrivateBlockBody <$> decode

instance Serialise PrivateBlock where
    encode (PrivateBlock {..}) = mconcat
        [ encodeListLen 2
        , encode _pbHeader
        , encode _pbBody
        ]
    decode = do
        decodeListLenOf 2
        _pbHeader <- decode
        _pbBody   <- decode
        return $ PrivateBlock {..}

instance HasId PrivateBlock where
    type Id PrivateBlock = Hash PrivateBlockHeader

    getId = hash . _pbHeader

-- | Transactions
instance Serialise PrivateTx where
    encode (PrivateTx {..}) = mconcat
        [ encodeListLen 3
        , encode _ptSignedSubmission
        , encode _ptGrade
        , encode _ptTime
        ]
    decode = do
        decodeListLenOf 3
        _ptSignedSubmission <- decode
        _ptGrade            <- decode
        _ptTime             <- decode
        return $ PrivateTx {..}

instance Serialise PrivateTxWitness where
    encode (PkWitness {..}) = mconcat
        [ encodeListLen 2
        , encode _ptwKey
        , encode _ptwSig
        ]
    decode = do
        decodeListLenOf 2
        _ptwKey <- decode
        _ptwSig <- decode
        return $ PkWitness {..}

instance Serialise PrivateTxAux where
    encode (PrivateTxAux {..}) = mconcat
        [ encodeListLen 2
        , encode _ptaTx
        , encode _ptaWitness
        ]

    decode = do
        decodeListLenOf 2
        _ptaTx      <- decode
        _ptaWitness <- decode
        return $ PrivateTxAux {..}

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

instance Serialise ATGNode where
    encode (ATGNode {..}) = mconcat
        [ encodeListLen 2
        , encode _atgnSubjectId
        , encode _atgnChildren
        ]

    decode = do
        decodeListLenOf 2
        _atgnSubjectId <- decode
        _atgnChildren  <- decode
        return $ ATGNode {..}

instance Serialise ATGEdge where
    encode (ATGEdge {..}) = mconcat
        [ encodeListLen 2
        , encode _atgeWeight
        , encode _atgeChild
        ]

    decode = do
        decodeListLenOf 2
        _atgeWeight <- decode
        _atgeChild  <- decode
        return $ ATGEdge {..}

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

instance Serialise Assignment where
    encode (Assignment {..}) = mconcat
        [ encodeListLen 4
        , encode _aCourseId
        , encode _aContentsHash
        , encode _aType
        , encode _aDesc
        ]

    decode = do
        decodeListLenOf 4
        _aCourseId     <- decode
        _aContentsHash <- decode
        _aType         <- decode
        _aDesc         <- decode
        return $ Assignment {..}

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

instance Serialise SubmissionWitness where
    encode (SubmissionWitness {..}) = mconcat
        [ encodeListLen 2
        , encode _swKey
        , encode _swSig
        ]

    decode = do
        decodeListLenOf 2
        _swKey <- decode
        _swSig <- decode
        return $ SubmissionWitness {..}

instance Serialise SignedSubmission where
    encode (SignedSubmission {..}) = mconcat
        [ encodeListLen 2
        , encode _ssSubmission
        , encode _ssWitness
        ]

    decode = do
        decodeListLenOf 2
        _ssSubmission <- decode
        _ssWitness    <- decode
        return $ SignedSubmission {..}

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

instance Serialise CertificateMeta where
    encode (CertificateMeta {..}) = mconcat
        [ encodeListLen 10
        , encode cmStudentName
        , encode cmStudentBirthDate
        , encode cmStartYear
        , encode cmEndYear
        , encode cmEducationForm
        , encode cmNumber
        , encode cmIssueDate
        , encode cmTitle
        , encode cmMajor
        , encode cmSpecialization
        ]

    decode = do
        decodeListLenOf 10
        cmStudentName      <- decode
        cmStudentBirthDate <- decode
        cmStartYear        <- decode
        cmEndYear          <- decode
        cmEducationForm    <- decode
        cmNumber           <- decode
        cmIssueDate        <- decode
        cmTitle            <- decode
        cmMajor            <- decode
        cmSpecialization   <- decode
        return $ CertificateMeta {..}

instance HasId CertificateMeta where
    type Id CertificateMeta = Hash CertificateMeta
    getId = hash

instance HasId CertificateFullInfo where
    type Id CertificateFullInfo = Id CertificateMeta
    getId = getId . cfiMeta

instance Serialise Submission where
    encode (Submission {..}) = mconcat
        [ encodeListLen 3
        , encode _sStudentId
        , encode _sContentsHash
        , encode _sAssignmentHash
        ]
    decode = do
        decodeListLenOf 3
        _sStudentId      <- decode
        _sContentsHash   <- decode
        _sAssignmentHash <- decode
        return $ Submission {..}

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

instance Serialise TxInAcc where
    encode (TxInAcc {..}) = mconcat
        [ encodeListLen 2
        , encode tiaAddr
        , encode tiaNonce
        ]
    decode = do
        decodeListLenOf 2
        tiaAddr  <- decode
        tiaNonce <- decode
        return $ TxInAcc {..}

instance Serialise TxOut where
    encode (TxOut {..}) = mconcat
        [ encodeListLen 2
        , encode txOutAddr
        , encode txOutValue
        ]
    decode = do
        decodeListLenOf 2
        txOutAddr  <- decode
        txOutValue <- decode
        return $ TxOut {..}

instance Serialise Tx where
    encode (Tx {..}) = mconcat
        [ encodeListLen 3
        , encode txInAcc
        , encode txInValue
        , encode txOuts
        ]
    decode = do
        decodeListLenOf 3
        txInAcc   <- decode
        txInValue <- decode
        txOuts    <- decode
        return $ Tx {..}

instance Serialise TxWitness where
    encode (TxWitness {..}) = mconcat
        [ encodeListLen 2
        , encode txwSig
        , encode txwPk
        ]
    decode = do
        decodeListLenOf 2
        txwSig <- decode
        txwPk  <- decode
        return $ TxWitness {..}

instance Serialise TxWitnessed where
    encode (TxWitnessed {..}) = mconcat
        [ encodeListLen 2
        , encode twTx
        , encode twWitness
        ]
    decode = do
        decodeListLenOf 2
        twTx      <- decode
        twWitness <- decode
        return $ TxWitnessed {..}

instance Serialise PublicationTx where
    encode (PublicationTx {..}) = mconcat
        [ encodeListLen 3
        , encode ptAuthor
        , encode ptFeesAmount
        , encode ptHeader
        ]
    decode = do
        decodeListLenOf 3
        ptAuthor     <- decode
        ptFeesAmount <- decode
        ptHeader     <- decode
        return $ PublicationTx {..}

instance Serialise PublicationTxWitness where
    encode (PublicationTxWitness {..}) = mconcat
        [ encodeListLen 2
        , encode pwSig
        , encode pwPk
        ]
    decode = do
        decodeListLenOf 2
        pwSig <- decode
        pwPk  <- decode
        return $ PublicationTxWitness {..}

instance Serialise PublicationTxWitnessed where
    encode (PublicationTxWitnessed {..}) = mconcat
        [ encodeListLen 2
        , encode ptwTx
        , encode ptwWitness
        ]
    decode = do
        decodeListLenOf 2
        ptwTx      <- decode
        ptwWitness <- decode
        return $ PublicationTxWitnessed {..}

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

instance Serialise BlockToSign where
    encode (BlockToSign difficulty slotId hHeader hBlockBody) = mconcat
        [ encodeListLen 4
        , encode difficulty
        , encode slotId
        , encode hHeader
        , encode hBlockBody
        ]
    decode = do
        decodeListLenOf 4
        difficulty <- decode
        slotId     <- decode
        hHeader    <- decode
        hBlockBody <- decode
        return $ BlockToSign difficulty slotId hHeader hBlockBody

instance Serialise SlotId where
    encode (SlotId w) = encode w
    decode = SlotId <$> decode

instance Serialise Difficulty where
    encode = encode . unDifficulty
    decode = Difficulty <$> decode

instance Serialise Header where
    encode (Header {..}) = mconcat
        [ encodeListLen 5
        , encode hSignature
        , encode hIssuer
        , encode hDifficulty
        , encode hSlotId
        , encode hPrevHash
        ]
    decode = do
        decodeListLenOf 5
        hSignature  <- decode
        hIssuer     <- decode
        hDifficulty <- decode
        hSlotId     <- decode
        hPrevHash   <- decode
        return $ Header {..}

instance Serialise Block where
    encode (Block {..}) = mconcat
        [ encodeListLen 2
        , encode bHeader
        , encode bBody
        ]
    decode = do
        decodeListLenOf 2
        bHeader <- decode
        bBody   <- decode
        return $ Block {..}

instance Serialise BlockBody where
    encode = encode . bbTxs
    decode = BlockBody <$> decode
