
module Dscp.Snowdrop.Contracts.Util
       ( module Dscp.Snowdrop.Contracts.Util
       , module Lenses
       , module Errors
       , module Config
       , module SD.Core
       , module SD.Util
       , module Core
       , module Types
       , module Storage.Types
       , module Auth
       , module Asserts
       , module Account
       , module Serialise
       , module Util
       ) where

import Control.Lens as Lenses (makeLenses)
import Codec.Serialise as Serialise (Serialise)
import Data.Default (def)
import qualified Data.Map as Map

import Snowdrop.Core as SD.Core (
    PreValidator (..), StateTx (..), StateTxType (..), Validator, ValueOp (..),
    changeSet, mkValidator, queryOne, SValue, ERoComp, StatePException, HasKeyValue,
    SeqExpanders(..), Expander (..), DiffChangeSet (..), ChangeSet (..), mappendChangeSet)
import Snowdrop.Util as SD.Util

import Dscp.Core                      as Core hiding (PublicationTxWitness)
import Dscp.Snowdrop.Account          as Account
import Dscp.Snowdrop.Assertions       as Asserts
import Dscp.Snowdrop.Configuration    as Config
import Dscp.Snowdrop.Contracts.Errors as Errors
import Dscp.Snowdrop.Types            as Types (Account, AccountId)
import Dscp.Snowdrop.Storage.Types    as Storage.Types (PublicationHead)
import Dscp.Snowdrop.Authentication   as Auth
import Dscp.Snowdrop.Util             as Util

-- validateCreated = mkValidator ty [checkCreation]
--   where
--     ty = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId

type PreValidate ctx = PreValidator  Exceptions Ids Proofs Values ctx
type SDActionM   ctx = ERoComp       Exceptions Ids        Values ctx
type Expand      ctx = SeqExpanders  Exceptions Ids Proofs Values ctx
type Delta           = DiffChangeSet            Ids        Values

delta :: [(Ids, ValueOp Values)] -> Delta
delta = DiffChangeSet . ChangeSet . Map.fromList

merge :: [SDActionM ctx Delta] -> SDActionM ctx Delta
merge dcsMs = do
    dcss <- sequence dcsMs
    let csOrErr = fmap DiffChangeSet $ foldM mappendChangeSet def $ map unDiffCS dcss
    either throwLocalError pure csOrErr
