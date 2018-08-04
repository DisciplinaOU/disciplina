{-# LANGUAGE StrictData #-}

-- | Common datatypes for educator and student HTTP APIs

module Dscp.Educator.Web.Types
       (
         -- * Flags
         IsFinal (..)
       , _IsFinal
       , HasProof (..)

         -- * Responses
       , GradeInfo (..)
       , BlkProofInfo (..)

         -- * Conversions
       , assignmentTypeRaw
       ) where

import Control.Lens (Iso', from, iso, makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Servant (FromHttpApiData (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.Util.Aeson (AsHex)

-- | Whether assignment is final in course.
newtype IsFinal = IsFinal { unIsFinal :: Bool }
    deriving (Eq, Show)

makePrisms ''IsFinal

-- | Whether transaction has been published into public chain.
newtype HasProof = HasProof { unHasProof :: Bool }
    deriving (Eq, Show)

data GradeInfo = GradeInfo
    { giSubmissionHash :: (Hash Submission)
    , giGrade          :: Grade
    , giTimestamp      :: UTCTime
    , giHasProof       :: Bool
    } deriving (Show, Eq, Generic)

data BlkProofInfo = BlkProofInfo
    { bpiMtreeSerialized :: (AsHex ByteString)
    , bpiTxs             :: [PrivateTx]
    } deriving (Show, Eq, Generic)

---------------------------------------------------------------------------
-- Simple conversions
---------------------------------------------------------------------------

assignmentTypeRaw :: Iso' AssignmentType IsFinal
assignmentTypeRaw = iso forth back . from _IsFinal
  where
    back = \case
        False -> Regular
        True  -> CourseFinal
    forth = \case
        Regular     -> False
        CourseFinal -> True

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''PrivateTx

deriveJSON defaultOptions ''IsFinal
deriveJSON defaultOptions ''HasProof
deriveJSON defaultOptions ''GradeInfo
deriveJSON defaultOptions ''BlkProofInfo

---------------------------------------------------------------------------
-- FromHttpApiData instances
---------------------------------------------------------------------------

deriving instance FromHttpApiData IsFinal
