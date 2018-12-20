module Dscp.Snowdrop.Expanders.Common
     ( Expansion
     , ChangeEntry
     , (==>)
     , toChangeMap
     ) where

import qualified Data.Map.Strict as Map

import Snowdrop.Core (ERoComp, ValueOp (..))
import Snowdrop.Util (HasReview (..))

import Dscp.Snowdrop.Configuration

-- | Expander monad.
type Expansion ctx = ERoComp Exceptions Ids Values ctx
-- | Single ChangeSet entry.
type ChangeEntry = (Ids, ValueOp Values)

(==>)
    :: forall id value k v
    .  HasReview k id
    => HasReview v value
    => id
    -> value
    -> (k, v)
k ==> v = (inj k, inj v)

-- | Folds changes list into map checking that no entries overlap.
toChangeMap :: [ChangeEntry] -> Map Ids (ValueOp Values)
toChangeMap changes =
    let keyz = map fst changes
    in if length keyz == length (ordNub keyz)
       then Map.fromList changes
       else error "Changeset produced by tx has duplicates"
