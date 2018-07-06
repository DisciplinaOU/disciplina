
-- | Utilities

module Dscp.Util
       ( anyMapM
       , wrapRethrow
       , wrapRethrowIO
       , leftToThrow
       , leftToFail
       , leftToPanic
       , leftToFailWith
       , leftToPanicWith

         -- * Formatting
       , Base (..)
       , toBase
       , fromBase
       , toBase64
       , fromBase64
       , toHex
       , fromHex

         -- * Error handling
       , assert
       , assertJust

         -- * Ids for databases
       , HasId (..)
       , idOf

         -- * Re-exports
       , module Snowdrop.Util
       ) where

import Control.Lens (Getter, to)

import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding (Base (..), convertFromBase, convertToBase)
import Snowdrop.Util hiding (getId)

import Dscp.Crypto.ByteArray (FromByteArray (..))

import qualified UnliftIO as UIO

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

leftToThrow
    :: (MonadThrow m, Exception e2)
    => (e1 -> e2) -> Either e1 a -> m a
leftToThrow wrapErr = either (throwM . wrapErr) pure

leftToFail
    :: (MonadFail m, ToString s) => Either s a -> m a
leftToFail = either (fail . toString) pure

leftToPanic
    :: ToText s => Either s a -> a
leftToPanic = either (error . toText) identity

prefixed :: Semigroup a => a -> a -> a
prefixed text prefix = prefix <> text

leftToFailWith
    :: (MonadFail m, ToString s) => String -> Either s a -> m a
leftToFailWith prefix =
    either (fail . prefixed (prefix <> ": ") . toString) pure

leftToPanicWith
    :: ToText s => Text -> Either s a -> a
leftToPanicWith prefix =
    either error identity . first (prefixed (prefix <> ": ") . toText)

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

-----------------------------------------------------------
-- Do-or-throw error handlers
-----------------------------------------------------------

assert :: (MonadIO m, Exception e) => m Bool -> e -> m ()
assert action message = do
    yes <- action

    unless yes $ do
        UIO.throwIO message

assertJust :: (MonadIO m, Exception e) => m (Maybe a) -> e -> m a
assertJust action message = do
    mb <- action

    whenNothing mb $ do
        UIO.throwIO message

-----------------------------------------------------------
-- Helper to establish notion of SQLite/db ID
-----------------------------------------------------------

class HasId s where
    type Id s :: *
    type Id s = s

    getId :: s -> Id s

    default getId :: (Id s ~ s) => s -> Id s
    getId = id

idOf :: HasId s => Getter s (Id s)
idOf = to getId