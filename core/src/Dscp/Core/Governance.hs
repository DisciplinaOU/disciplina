module Dscp.Core.Governance
    ( Governance (..)
    , CommitteeSecret
    , mkCommitteeSecret
    , unCommitteeSecret
    , Committee (..)
    , mkClosedCommittee
    , committeeDerive
    , committeeAddrs
    , openCommitteeSecrets
    , committeeOwnsSlot
    ) where

import Data.ByteArray (convert)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List

import Dscp.Core.Foundation
import Dscp.Crypto

-- | Governance type.
data Governance
    = GovCommittee Committee -- ^ Fixed set of leaders.
    | GovOpen                -- ^ Open governance
    deriving (Eq, Show, Generic)

----------------------------------------------------------------------------
-- Committee
----------------------------------------------------------------------------

-- | Committee secret.
newtype CommitteeSecret = CommitteeSecret
    { unCommitteeSecret :: ByteString
    } deriving (Eq, Ord, Show, Generic)

mkCommitteeSecret :: ByteString -> Either Text CommitteeSecret
mkCommitteeSecret sec
    | null sec = Left "CommitteeSecret is empty"
    | otherwise = Right $ CommitteeSecret sec

-- | Committee is a fixed order list of addresses allowed to issue
-- block. It can be either closed or open (for testing). Participants'
-- addresses are derived from the secret, which is open in case of
-- open committee.
data Committee
    = CommitteeClosed { commParticipants :: [Address]
                        -- ^ Participants of the committee.
                      }
    | CommitteeOpen { commSecret :: CommitteeSecret
                      -- ^ Secret everyone can derive participants'
                      -- secret keys from.
                    , commN      :: Integer
                      -- ^ Number of participants.
                    }
    deriving (Eq, Ord, Show, Generic)

-- | Create committee or fail.
mkClosedCommittee :: CommitteeSecret -> Integer -> Committee
mkClosedCommittee sec n = CommitteeClosed addresses
  where
    addresses = map (mkAddr . toPublic) sks
    sks = map (committeeDerive sec) [0 .. n - 1]

-- | Derive a secret key from committee secret and participant index.
committeeDerive :: CommitteeSecret -> Integer -> SecretKey
committeeDerive (CommitteeSecret s) idx =
    withSeed (convert $ hash $ BS.pack (show idx) `BS.append` s) genSecretKey

-- | Get committee addresses.
committeeAddrs :: Committee -> [Address]
committeeAddrs (CommitteeClosed addrs) = addrs
committeeAddrs (CommitteeOpen s n)     = commParticipants $ mkClosedCommittee s n

-- | Get open committee secrets.
openCommitteeSecrets :: Committee -> [SecretKey]
openCommitteeSecrets (CommitteeOpen sec n) = map (committeeDerive sec) [0 .. n - 1]
openCommitteeSecrets _                     = error "openCommitteeSecrets: committee is not open"

-- | Determine committee member which is allowed to issue block in the
-- current slot.
committeeOwnsSlot :: Committee -> Address -> SlotId -> Bool
committeeOwnsSlot com addr (SlotId slotId) =
    case List.elemIndex addr addresses of
        Nothing -> False
        Just i  -> fromIntegral slotId `mod` length addresses == i
  where
    addresses = committeeAddrs com
