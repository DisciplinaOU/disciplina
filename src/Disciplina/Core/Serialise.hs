module Disciplina.Core.Serialise () where

import Codec.Serialise (Serialise (..))

import Disciplina.Core.Types (ATG (..), ATGDelta (..), ATGEdge (..), ATGNode (..), Address (..),
                              Grade (..))

-- TODO: move to well-specified serialisation instead of generic one.
instance Serialise Address
instance Serialise Grade
instance Serialise ATGNode
instance Serialise ATGEdge

deriving instance Serialise ATGDelta
deriving instance Serialise ATG
