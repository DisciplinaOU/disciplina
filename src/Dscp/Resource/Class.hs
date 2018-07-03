-- | Class for resource allocation.

module Dscp.Resource.Class
       ( AllocResource(..)
       , tryRunComponentM
       ) where

import Control.Monad.Component (ComponentM, ComponentError, runComponentM)
import qualified Data.Text.Prettyprint.Doc as Doc (pretty)

-- | Resources safe allocation.
class AllocResource param resource | param -> resource, resource -> param where
    -- | Construct a resource using given parameters. Automatic cleanup.
    -- Use 'buildComponent' to construct function of this type.
    allocResource :: param -> ComponentM resource

-- | Runs a component, handling and printing possible synchronous exceptions.
tryRunComponentM
    :: Text
    -> ComponentM a
    -> (a -> IO b)
    -> IO (Either ComponentError b)
tryRunComponentM appName component main = do
    res <- try $ runComponentM appName component main
    whenLeft res $ print . Doc.pretty
    return res
