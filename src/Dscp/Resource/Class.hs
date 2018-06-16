-- | Class for resource allocation.

module Dscp.Resource.Class
       ( AllocResource(..)
       ) where

import Control.Monad.Component (ComponentM)

-- | Resources safe allocation.
class AllocResource param resource | param -> resource, resource -> param where
    -- | Construct a resource using given parameters. Automatic cleanup.
    -- Use 'buildComponent' to construct function of this type.
    allocResource :: param -> ComponentM resource
