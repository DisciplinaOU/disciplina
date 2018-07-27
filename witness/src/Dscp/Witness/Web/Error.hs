{-# LANGUAGE StrictData #-}

module Dscp.Witness.Web.Error
    ( WitnessWebError (..)
    , witnessToServantErr
    ) where

import qualified Data.Text.Buildable as B
import Fmt ((+|), (|+))
import Servant (ServantErr (..), err403, err404, err500)
import qualified Text.Show

data WitnessWebError
    = EntityAbsent Text
      -- ^ Requested entity does not exist
    | RequestError Text
      -- ^ Client requested invalid operation
    | InternalError Text
      -- ^ Unexpected error happened

instance Show WitnessWebError where
    show = toString . pretty

instance Buildable WitnessWebError where
    build = \case
        EntityAbsent msg -> B.build msg
        RequestError msg -> B.build msg
        InternalError msg -> "Internal error: " +| msg |+ ""

instance Exception WitnessWebError

-- Specular function: 'servantToWitnessError'.
witnessToServantErr :: WitnessWebError -> ServantErr
witnessToServantErr = \case
    EntityAbsent msg -> err404{ errBody = encodeUtf8 msg }
    RequestError msg -> err403{ errBody = encodeUtf8 msg }
    InternalError msg -> err500{ errBody = encodeUtf8 msg }
