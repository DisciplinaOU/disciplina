
-- | Utilities

module Dscp.Util
       ( anyMapM
       , wrapRethrow
       , wrapRethrowIO

         -- * Formatting
       , toBase64
       , fromBase64
       , toHex
       , fromHex
         -- * Re-exports
       , module Snowdrop.Util
       ) where

import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding (Base (..), convertFromBase, convertToBase)
import Snowdrop.Util

import Dscp.Crypto.ByteArray (FromByteArray (..))

deriving instance Container (b a) => Container (OldestFirst b a)
deriving instance Container (b a) => Container (NewestFirst b a)

anyMapM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyMapM _ [] = return False
anyMapM f (a:as) = f a >>= \case
    True -> return True
    False -> anyMapM f as

-- | Converts a possible error, used for wrapping exceptions using given
-- constructor into ADT-sum of exceptions.
wrapRethrow
    :: (Exception e1, Exception e2, MonadCatch m)
    => (e1 -> e2) -> m a -> m a
wrapRethrow wrap action = catch action (throwM . wrap)

-- | Handy for wrapping exceptions provided by libraries.
wrapRethrowIO
    :: (Exception e1, Exception e2, MonadCatch m, MonadIO m)
    => (e1 -> e2) -> IO a -> m a
wrapRethrowIO wrap action = wrapRethrow wrap (liftIO action)

-----------------------------------------------------------
-- Bytestrings formatting
-----------------------------------------------------------

toBase :: ByteArrayAccess ba => Base -> ba -> Text
toBase base = decodeUtf8 @Text @ByteString . convertToBase base

fromBase :: forall ba. FromByteArray ba => Base -> Text -> Either String ba
fromBase base =
    convertFromBase base . encodeUtf8 @Text @ByteString >=>
    fromByteArray @ba @ByteString

toBase64, toHex :: ByteArrayAccess ba => ba -> Text
toBase64 = toBase Base64
toHex    = toBase Base16

fromBase64, fromHex :: FromByteArray ba => Text -> Either String ba
fromBase64 = fromBase Base64
fromHex    = fromBase Base16
