
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
       ) where

import Control.Lens as Lenses (makeLenses)
import Codec.Serialise as Serialise (Serialise)

import Snowdrop.Core as SD.Core (
    PreValidator (..), StateTx (..), StateTxType (..), Validator, ValueOp (..),
    changeSet, mkValidator, queryOne, SValue, ERoComp, StatePException, HasKeyValue)
import Snowdrop.Util as SD.Util

import Dscp.Core                      as Core hiding (PublicationTxWitness)
import Dscp.Snowdrop.Account          as Account
import Dscp.Snowdrop.Assertions       as Asserts
import Dscp.Snowdrop.Configuration    as Config
import Dscp.Snowdrop.Contracts.Errors as Errors
import Dscp.Snowdrop.Types            as Types (Account, AccountId)
import Dscp.Snowdrop.Storage.Types    as Storage.Types (PublicationHead)
import Dscp.Snowdrop.Authentication   as Auth

-- validateCreated = mkValidator ty [checkCreation]
--   where
--     ty = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId

type PreValidate ctx = PreValidator Exceptions Ids Proofs Values ctx
type SDActionM   ctx = ERoComp      Exceptions Ids        Values ctx
