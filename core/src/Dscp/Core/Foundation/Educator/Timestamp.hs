
module Dscp.Core.Foundation.Educator.Timestamp where

import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)

-- | Timestamp with up to microseconds precision, as it is stored by Postgres.
-- Helps to avoid confusion like when a time value gets rounded on itself.
--
-- Since Postgres truncates not only timestamps it stores, but also the ones we compare on
-- in @SELECT@ query, this type should appear in web API as well.
-- TODO [DSCP-416]: move to SQL utils moved to core.
newtype Timestamp = TimestampUnsafe { unTimestamp :: UTCTime }
    deriving (Show, Eq, Ord, Buildable)

-- | Rounds time to microseconds.
toTimestamp :: UTCTime -> Timestamp
toTimestamp utc = TimestampUnsafe
    utc{ utctDayTime =
            picosecondsToDiffTime . roundPcsToMcs . diffTimeToPicoseconds $
            utctDayTime utc
       }
  where
    _1e6 :: Num a => a
    _1e6 = 1000 * 1000

    roundPcsToMcs :: Integer -> Integer
    roundPcsToMcs = (* _1e6) . round . (/ _1e6) . fromIntegral @_ @Double

toTimestampUnsafe :: UTCTime -> Timestamp
toTimestampUnsafe = TimestampUnsafe

