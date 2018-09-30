{-# LANGUAGE StrictData #-}

module Dscp.Witness.Keys
       ( WitnessKeyParams (..)
       ) where

import Data.Aeson (FromJSON (..), Value (..), withObject, (.:))

import Dscp.Resource.Keys

-- | Witness key parameters.
data WitnessKeyParams
    = Basic BaseKeyParams       -- ^ Basic key management with a keyfile
    | Committee CommitteeParams -- ^ Generate a key from committee params
    deriving (Show)

-- | JSON instance (for configuration specs)
instance FromJSON WitnessKeyParams where
    parseJSON = withObject "WitnessKeyParams" $ \o -> do
        typ :: Text <- o .: "type"
        case typ of
            "basic"     -> Basic <$> parseJSON (Object o)
            "committee" -> Committee <$> parseJSON (Object o)
            _           -> fail $ "Unrecognized key params type: " ++ toString typ
