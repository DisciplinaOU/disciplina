-- | JSON insances for core types

module Dscp.Core.Aeson () where

import Universum
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
                   ToJSONKey (..), Value (..), object, withObject, withScientific, withText, (.:),
                   (.=))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON, deriveJSON)
import Data.Aeson.Types (toJSONKeyText)
import Data.Scientific (scientific, toBoundedInteger)
import Data.Time.Clock (UTCTime)
import Data.Typeable (gcast)

import Dscp.Core.Config
import Dscp.Core.FairCV
import Dscp.Core.Fees
import Dscp.Core.Foundation
import Dscp.Core.Genesis
import Dscp.Core.Governance
import Dscp.Core.PubChain
import Dscp.Crypto
import Dscp.Util (Base (Base16, Base64), leftToFail, nothingToFail)
import Dscp.Util.Aeson (dscpAesonOptions, parseJSONSerialise, toJSONSerialise)

---------------------------------------------------------------------------
-- Manual instances
---------------------------------------------------------------------------

-- TODO [DSCP-416]: Move
instance ToJSON ItemDesc where
    toJSON = toJSON . unItemDesc
instance FromJSON ItemDesc where
    parseJSON v = leftToFail . toItemDesc =<< parseJSON @Text v

deriving instance ToJSON Timestamp
instance FromJSON Timestamp where
    parseJSON v = toTimestamp <$> parseJSON @UTCTime v

instance ToJSON Address where
    toJSON = String . toText
instance FromJSON Address where
    parseJSON = withText "Address" $ leftToFail . addrFromText


instance ToJSON Grade where
    toJSON = Number . fromIntegral . getGrade
instance FromJSON Grade where
    parseJSON = withScientific "Grade" $
        maybe (fail "value is outside [0, 100] range") pure .
        mkGrade . round

instance ToJSON SubmissionWitness where
    toJSON = toJSONSerialise Base16
instance FromJSON SubmissionWitness where
    parseJSON = parseJSONSerialise Base16

instance ToJSON ATGSubjectChange where
    toJSON = String . \case
        ATGAdded -> "added"
        ATGRemoved -> "removed"

instance FromJSON ATGSubjectChange where
    parseJSON = withText "ATG subject change" $ \case
        "added" -> pure ATGAdded
        "removed" -> pure ATGRemoved
        _ -> fail "Invalid ATG subject change"

instance FromJSON Governance where
  parseJSON = withObject "governance" $ \o -> do
    (governanceType :: Text) <- o .: "type"
    case governanceType of
        "committeeOpen" -> do
            commSecret <- o .: "secret"
            commN <- o .: "n"
            return $ GovCommittee (CommitteeOpen {..})
        "committeeClosed" -> do
            commParticipants <- o .: "participants"
            return $ GovCommittee (CommitteeClosed {..})
        _ -> fail "Governance type is invalid"

instance ToJSON Coin where
    toJSON coin = Number $ scientific (coinToInteger coin) (-6)

instance FromJSON Coin where
    parseJSON = withScientific "Coin" $
        nothingToFail "Coin is in invalid format" .
        fmap Coin . toBoundedInteger . (*1000000)

instance (ToJSON a, Serialise (EmptyMerkleProof a)) =>
         ToJSON (MerkleProof a) where
    toJSON proof = object
        [ "proof" .= toJSONSerialise Base64 emptyProof
        , "txs" .= txs
        ]
      where
        (emptyProof, txs) = separateProofAndData proof

instance (FromJSON a, Serialise (EmptyMerkleProof a)) =>
         FromJSON (MerkleProof a) where
    parseJSON = withObject "TaggedProof" $ \o -> do
        emptyProof <- parseJSONSerialise Base64 =<< o .: "proof"
        txs <- o .: "txs"
        nothingToFail "Merkle proof and data do not match" $
            mergeProofAndData emptyProof txs

instance ToJSON (MerkleProof a) => ToJSON (MerkleProofReady a) where
    toJSON mpr = object
        [ "root" .= mprRoot mpr
        , "tree" .= mprProof mpr
        ]

instance (FromJSON (MerkleProof a), HasHash a) => FromJSON (MerkleProofReady a) where
    parseJSON = withObject "TaggedProofReady" $ \o -> do
        root  <- o .: "root"
        proof <- o .: "tree"

        let reconstructed = readyProof proof
        when (mprRoot reconstructed /= root) $
            fail "Proof and its root do not match"
        return reconstructed

instance ToJSON a => ToJSON (TxIdAnnotated a) where
    toJSON (TxIdAnnotated txId val) = object
        [ "txId" .= txId
        , "val" .= val
        ]

instance FromJSON a => FromJSON (TxIdAnnotated a) where
    parseJSON = withObject "TxIdAnnotated" $ \o ->
        TxIdAnnotated <$> (o .: "txId") <*> (o .: "val")

instance Serialise (MerkleProof PrivateTx) =>
         ToJSON FairCV where
    toJSON FairCV {..} = object
        [ "desc" .= fcDesc
        , "cv" .= fcCV
        ]

instance Serialise (MerkleProof PrivateTx) =>
         FromJSON FairCV where
    parseJSON = withObject "FairCV" $ \o -> do
        desc <- o .: "desc"
        cv <- o .: "cv"
        pure $ FairCV desc cv

instance ToJSON (MerkleProofReady PrivateTx) =>
         ToJSON FairCVReady where
    toJSON FairCV {..} = object
        [ "desc" .= fcDesc
        , "cv" .= fcCV
        ]

instance FromJSON (MerkleProofReady PrivateTx) =>
         FromJSON FairCVReady where
    parseJSON = withObject "FairCV" $ \o -> do
        desc <- o .: "desc"
        cv <- o .: "cv"
        pure $ FairCV desc cv

---------------------------------------------------------------------------
-- Standalone derivations for newtypes
---------------------------------------------------------------------------

deriving instance ToJSON Nonce
deriving instance FromJSON Nonce

deriving instance ToJSON GTxId
deriving instance FromJSON GTxId

deriving instance ToJSON Entity
deriving instance FromJSON Entity

deriving instance ToJSON Course
deriving instance FromJSON Course

deriving instance ToJSON Subject
deriving instance FromJSON Subject

deriving instance ToJSON SlotDuration
deriving instance FromJSON SlotDuration

deriving instance ToJSON SlotId
deriving instance FromJSON SlotId

deriving instance ToJSON Difficulty
deriving instance FromJSON Difficulty

deriving instance ToJSONKey Subject
deriving instance FromJSONKey Subject

deriving instance ToJSON ATGDelta
deriving instance FromJSON ATGDelta

instance ToJSONKey Address where
    toJSONKey = toJSONKeyText addrToText
instance FromJSONKey Address where
    fromJSONKey = FromJSONKeyTextParser $ leftToFail . addrFromText

instance ToJSONKey PubAddress where
    toJSONKey = toJSONKeyText toText
instance FromJSONKey PubAddress where
    fromJSONKey = FromJSONKeyTextParser $ leftToFail . pubAddrFromText

deriving instance FromJSON GenAddressMap
deriving instance ToJSON GenAddressMap
instance FromJSON CommitteeSecret where
    parseJSON = withText "CommitteeSecret" $ leftToFail . mkCommitteeSecret . encodeUtf8

deriving instance ToJSON GenesisDistribution
deriving instance FromJSON GenesisDistribution

---------------------------------------------------------------------------
-- TH derivations for data
---------------------------------------------------------------------------

deriveJSON defaultOptions ''Assignment
deriveJSON defaultOptions ''Submission
deriveJSON defaultOptions ''SignedSubmission
deriveJSON dscpAesonOptions ''DocumentType  -- TODO: apply everywhere
deriveJSON dscpAesonOptions ''AssignmentType
deriveJSON defaultOptions ''CertificateIssuerInfo
deriveJSON defaultOptions ''CertificateMeta
deriveJSON defaultOptions ''PrivateTx
deriveJSON defaultOptions ''PrivateBlockHeader
deriveJSON defaultOptions ''Header
deriveJSON defaultOptions ''TxInAcc
deriveJSON defaultOptions ''TxOut
deriveJSON defaultOptions ''Tx
deriveJSON defaultOptions ''TxWitness
deriveJSON defaultOptions ''TxWitnessed
deriveJSON defaultOptions ''GTx
deriveJSON defaultOptions ''GTxWitnessed
deriveJSON defaultOptions ''PublicationTxWitness
deriveJSON defaultOptions ''PublicationTxWitnessed
deriveJSON defaultOptions ''PublicationTx
deriveJSON defaultOptions ''FeeCoefficients
deriveJSON dscpAesonOptions ''EducationForm
deriveJSON dscpAesonOptions ''GradingScale

deriveFromJSON defaultOptions ''Committee
deriveJSON defaultOptions ''GenesisDistributionElem

instance FromJSON GenesisInfo where
    parseJSON = error "FromJSON GenesisInfo should never be called"

instance Typeable tx => FromJSON (FeePolicy tx) where
    parseJSON = asum . sequence
        [ withObject "linear fee policy" $ \o -> do
            "linear" :: Text <- o .: "type"
            coeffs           <- o .: "coeffs"
            return $ LinearFeePolicy coeffs
        , withObject "unknown fee policy" $ \o -> do
            "unknown" :: Text <- o .: "type"
            nothingToFail unallowedPolicy $ gcast UnknownFeePolicy
        ]
      where
        unallowedPolicy = "This fees policy is not applicable"

instance ToJSON FairCVCheckResult where
    toJSON FairCVCheckResult{..} = object
        [ "isValid" .= fairCVFullyValid
        , "checkResponse" .= fairCVCheckResults
        ]
instance FromJSON FairCVCheckResult where
    parseJSON = withObject "FairCVCheckResult" $ \o -> do
        fairCVFullyValid <- o .: "isValid"
        fairCVCheckResults <- o .: "checkResponse"
        return FairCVCheckResult{..}

deriveJSON defaultOptions ''FairCVAndCheckResult
