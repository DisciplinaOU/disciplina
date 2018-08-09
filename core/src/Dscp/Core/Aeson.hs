-- | JSON insances for core types

module Dscp.Core.Aeson () where

import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey, Value (..),
                   withScientific, withText, withObject, (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON, deriveJSON)

import Dscp.Core.Config
import Dscp.Core.Foundation
import Dscp.Core.Genesis
import Dscp.Core.Governance
import Dscp.Crypto.Aeson ()
import Dscp.Util (Base (Base16), leftToFail)
import Dscp.Util.Aeson (parseJSONSerialise, toJSONSerialise)

---------------------------------------------------------------------------
-- Manual instances
---------------------------------------------------------------------------

-- | TODO: make a generic instance generation for these enum-like instances
instance ToJSON AssignmentType where
    toJSON Regular     = String "regular"
    toJSON CourseFinal = String "courseFinal"
instance FromJSON AssignmentType where
    parseJSON = withText "AssignmentType" $ \case
        "regular"     -> pure Regular
        "courseFinal" -> pure CourseFinal
        other -> fail $ "invalid constructor: " ++ toString other

instance ToJSON DocumentType where
    toJSON Offline = String "offline"
    toJSON Online  = String "online"
instance FromJSON DocumentType where
    parseJSON = withText "DocumentType" $ \case
        "online" -> pure Online
        "offline" -> pure Offline
        other -> fail $ "invalid constructor: " ++ toString other

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
        "governanceOpen" -> return GovOpen
        _ -> fail "Governance type is invalid"

---------------------------------------------------------------------------
-- Standalone derivations for newtypes
---------------------------------------------------------------------------

deriving instance ToJSON Coin
deriving instance FromJSON Coin

deriving instance ToJSON GTxId
deriving instance FromJSON GTxId

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

instance FromJSONKey Address
instance ToJSONKey Address

deriving instance FromJSON GenAddressMap
deriving instance ToJSON GenAddressMap
instance FromJSON CommitteeSecret where
    parseJSON = withText "CommitteeSecret" $ pure . CommitteeSecret . encodeUtf8

deriving instance ToJSON GenesisDistribution
deriving instance FromJSON GenesisDistribution

---------------------------------------------------------------------------
-- TH derivations for data
---------------------------------------------------------------------------

deriveJSON defaultOptions ''Assignment
deriveJSON defaultOptions ''Submission
deriveJSON defaultOptions ''SignedSubmission
deriveJSON defaultOptions ''Header
deriveJSON defaultOptions ''TxInAcc
deriveJSON defaultOptions ''TxOut
deriveJSON defaultOptions ''Tx
deriveJSON defaultOptions ''TxWitness
deriveJSON defaultOptions ''TxWitnessed
deriveJSON defaultOptions ''GTx
deriveJSON defaultOptions ''GTxWitnessed
deriveJSON defaultOptions ''Publication
deriveJSON defaultOptions ''PublicationTxWitness
deriveJSON defaultOptions ''PublicationTxWitnessed
deriveJSON defaultOptions ''PublicationTx
deriveJSON defaultOptions ''PublicationsOf
deriveJSON defaultOptions ''LastPublication
deriveJSON defaultOptions ''PublicationHead
deriveJSON defaultOptions ''PublicationNext

deriveFromJSON defaultOptions ''Committee
deriveJSON defaultOptions ''GenesisDistributionElem
deriveFromJSON defaultOptions ''GenesisConfig

instance FromJSON GenesisInfo where
    parseJSON = error "FromJSON GenesisInfo should never be called"
