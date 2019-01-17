{-# LANGUAGE StrictData #-}

module Dscp.Witness.Keys
       ( WitnessKeyParams
       , WitnessKeyParamsRec
       , WitnessKeyParamsRecP
       ) where

import Loot.Config ((::+), (::-), Config, PartialConfig)

import Dscp.Resource.Keys

-- | Witness key parameters.
-- Note, there is a reason this contains the whole tree and not just its content
-- see 'ConfigMaybe' for an explanation.
type WitnessKeyParams =
   '[ "params" ::+
       '[ "basic" ::- BaseKeyParams
          -- Basic key management with a keyfile
        , "committee" ::- CommitteeParams
          -- Generate a key from committee params
        ]
    ]

type WitnessKeyParamsRec = Config WitnessKeyParams
type WitnessKeyParamsRecP = PartialConfig WitnessKeyParams
