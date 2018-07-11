module Dscp.Snowdrop.Configuration where

import Fmt (build, (+|))

import Snowdrop.Model.Block (Block, BlockRef (..), Blund, TipKey, TipValue)
import Snowdrop.Model.State.Accounting.Account (Account, Author (..))
import Snowdrop.Model.State.Core (SValue)
import Snowdrop.Util

import Dscp.Core (Address, HeaderHash)
import qualified Dscp.Core.Types as T
import Dscp.Crypto (hashF)

----------------------------------------------------------------------------
-- Snowdrop block-related types
----------------------------------------------------------------------------

type SHeader = T.Header
type SPayload = T.BlockBody
type SUndo = ChangeSet Ids Values
type SBlock = Block SHeader SPayload
type SBlund = Blund SHeader SPayload SUndo

----------------------------------------------------------------------------
-- Identities/prefixes
----------------------------------------------------------------------------

tipPrefix :: Prefix
tipPrefix = Prefix 1

blockPrefix :: Prefix
blockPrefix = Prefix 2

accountPrefix :: Prefix
accountPrefix = Prefix 3

-- | Sum-type for all ids used within the application.
data Ids
    = TipKeyIds TipKey
    | BlockRefIds (BlockRef HeaderHash)
    | AccountInIds (Author Address)
    deriving (Eq, Ord, Show, Generic)

instance Buildable Ids where
    build = ("Key" <>) . \case
        TipKeyIds t -> build t
        BlockRefIds (BlockRef r) -> "block ref " +| hashF r
        AccountInIds a -> build a

instance IdSumPrefixed Ids where
    idSumPrefix (TipKeyIds _)    = tipPrefix
    idSumPrefix (BlockRefIds _)  = blockPrefix
    idSumPrefix (AccountInIds _) = accountPrefix

type instance SValue TipKey = TipValue HeaderHash
type instance SValue (BlockRef HeaderHash) = SBlund
type instance SValue (Author Address) = Account

----------------------------------------------------------------------------
-- Values
----------------------------------------------------------------------------

data Values
    = TipValueVal (TipValue HeaderHash)
    | BlundVal SBlund
    | AccountOutVal Account
    deriving (Eq, Show, Generic)

----------------------------------------------------------------------------
-- HasReview
----------------------------------------------------------------------------

deriveView withInjProj ''Ids
deriveIdView withInjProj ''Ids

deriveView withInjProj ''Values
deriveIdView withInjProj ''Values
