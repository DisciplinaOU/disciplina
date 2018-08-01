-- | JSON insances for core types

module Dscp.Core.Aeson () where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withScientific, withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)

import Dscp.Core.Address (addrFromText)
import Dscp.Core.Foundation.Transactions.Types
import Dscp.Core.Serialise ()
import Dscp.Core.Types
import Dscp.Crypto.Aeson ()
import Dscp.Util (Base (Base64), leftToFail)
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
    toJSON = toJSONSerialise Base64
instance FromJSON SubmissionWitness where
    parseJSON = parseJSONSerialise Base64

---------------------------------------------------------------------------
-- Standalone derivations for newtypes
---------------------------------------------------------------------------

deriving instance ToJSON Course
deriving instance FromJSON Course

deriving instance ToJSON Subject
deriving instance FromJSON Subject

deriving instance ToJSON Coin
deriving instance FromJSON Coin

---------------------------------------------------------------------------
-- TH derivations for data
---------------------------------------------------------------------------

deriveJSON defaultOptions ''Assignment
deriveJSON defaultOptions ''Submission
deriveJSON defaultOptions ''SignedSubmission
deriveJSON defaultOptions ''TxInAcc
deriveJSON defaultOptions ''TxOut
deriveJSON defaultOptions ''Tx
deriveJSON defaultOptions ''TxWitness
deriveJSON defaultOptions ''TxWitnessed
deriveJSON defaultOptions ''GTxWitnessed
deriveJSON defaultOptions ''Publication
deriveJSON defaultOptions ''PublicationTxWitness
deriveJSON defaultOptions ''PublicationTxWitnessed
deriveJSON defaultOptions ''PublicationTx
deriveJSON defaultOptions ''PublicationsOf
deriveJSON defaultOptions ''LastPublication
deriveJSON defaultOptions ''PublicationHead
deriveJSON defaultOptions ''PublicationNext
