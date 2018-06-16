module Dscp.Core.Serialise () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Types (ATG (..), ATGDelta (..), ATGEdge (..), ATGNode (..), Address (..),
                              CourseId (..), Grade (..), SubjectId (..))

-- TODO: move to well-specified serialisation instead of generic one.
deriving instance Serialise CourseId
deriving instance Serialise SubjectId

instance Serialise Address
instance Serialise Grade
instance Serialise ATGNode
instance Serialise ATGEdge

deriving instance Serialise ATGDelta
deriving instance Serialise ATG
