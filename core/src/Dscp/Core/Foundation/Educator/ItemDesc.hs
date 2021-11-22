
module Dscp.Core.Foundation.Educator.ItemDesc where

import Control.Exception as E
import qualified Data.Text as T

import Dscp.Util (leftToPanic)

-- | Description of some object.
-- Cannot contain @\0@ symbol (Postgres truncates such strings for 'TEXT' type).
-- TODO [DSCP-416]: move to SQL utils moved to core.
newtype ItemDesc = ItemDescUnsafe { unItemDesc :: Text }
    deriving (Show, Eq, Ord, Buildable, Semigroup, Monoid)

isValidItemDesc :: Text -> Bool
isValidItemDesc = isNothing . T.find (== '\0')

toItemDesc :: Text -> Either Text ItemDesc
toItemDesc t
    | isValidItemDesc t = Right (ItemDescUnsafe t)
    | otherwise = Left "Text contains \0 characters."

toItemDescUnsafe :: Text -> ItemDesc
toItemDescUnsafe t = E.assert (isValidItemDesc t) (ItemDescUnsafe t)

instance IsString ItemDesc where
    fromString = leftToPanic . toItemDesc . fromString

