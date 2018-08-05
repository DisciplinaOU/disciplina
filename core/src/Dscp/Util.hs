-- | Utilities.

module Dscp.Util
       ( anyMapM
       , listToMaybeWarn
       , allUniqueOrd

         -- * Exceptions processing
       , wrapRethrow
       , wrapRethrowIO

         -- * Error handling
       , assert
       , assertJust

         -- * Maybe conversions
       , nothingToThrow
       , nothingToFail
       , nothingToPanic

         -- * Either conversions
       , leftToThrow
       , leftToFail
       , leftToPanic
       , leftToFailWith
       , leftToPanicWith
       , mappendLefts

         -- * Formatting
       , Base (..)
       , toBase
       , fromBase
       , toBase64
       , fromBase64
       , toHex
       , fromHex

         -- * Ids for databases
       , HasId (..)
       , idOf

         -- * Re-exports
       , module Snowdrop.Util
       ) where

import Codec.Serialise (Serialise)
import Control.Lens (Getter, to)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding (Base (..), convertFromBase, convertToBase)
import Fmt ((+|), (|+))
import Loot.Log (MonadLogging, logWarning)
import Snowdrop.Util hiding (HasHash, getId)

import Dscp.Crypto.ByteArray (FromByteArray (..))

import qualified UnliftIO as UIO

deriving instance Container (b a) => Container (OldestFirst b a)
deriving instance Container (b a) => Container (NewestFirst b a)

deriving instance Show (b a) => Show (OldestFirst b a)
deriving instance Show (b a) => Show (NewestFirst b a)

deriving instance Serialise (b a) => Serialise (OldestFirst b a)
deriving instance Serialise (b a) => Serialise (NewestFirst b a)

anyMapM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyMapM _ [] = return False
anyMapM f (a:as) = f a >>= \case
    True -> return True
    False -> anyMapM f as

prefixed :: Semigroup a => a -> a -> a
prefixed text prefix = prefix <> text

listToMaybeWarn :: (Monad m, MonadLogging m) => Text -> [a] -> m (Maybe a)
listToMaybeWarn msg = \case
    [] -> pure Nothing
    [x] -> pure (Just x)
    (x:_) -> do
        logWarning $ "listToMaybeWarn: to many entries ("+|msg|+")"
        return (Just x)

allUniqueOrd :: Ord a => [a] -> Bool
allUniqueOrd = all (null . drop 1) . group . sort

-----------------------------------------------------------
-- Exceptions processing
-----------------------------------------------------------

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
-- Do-or-throw error handlers
-----------------------------------------------------------

assert :: (MonadThrow m, Exception e) => m Bool -> e -> m ()
assert action message = unlessM action $ throwM message

assertJust :: (MonadThrow m, Exception e) => m (Maybe a) -> e -> m a
assertJust action message = whenNothingM action $ throwM message

-----------------------------------------------------------
-- Maybe conversions
-----------------------------------------------------------

nothingToThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
nothingToThrow e = maybe (throwM e) pure

nothingToFail :: (MonadFail m, ToString t) => t -> Maybe a -> m a
nothingToFail e = maybe (fail $ toString e) pure

nothingToPanic :: Text -> Maybe a -> a
nothingToPanic e = fromMaybe (error e)

-----------------------------------------------------------
-- Either conversions
-----------------------------------------------------------

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

leftToFailWith
    :: (MonadFail m, ToString s) => String -> Either s a -> m a
leftToFailWith prefix =
    either (fail . prefixed (prefix <> ": ") . toString) pure

leftToPanicWith
    :: ToText s => Text -> Either s a -> a
leftToPanicWith prefix =
    either error identity . first (prefixed (prefix <> ": ") . toText)

mappendLefts :: Monoid m => Either m () -> Either m () -> Either m ()
mappendLefts (Left a) (Left b) = Left (mappend a b)
mappendLefts (Right _) x       = x
mappendLefts x (Right _)       = x

-----------------------------------------------------------
-- ByteArray-based types formatting/parsing
-----------------------------------------------------------

toBase :: ByteArrayAccess ba => Base -> ba -> Text
toBase base = decodeUtf8 @Text @ByteString . convertToBase base

fromBase :: forall ba. FromByteArray ba => Base -> Text -> Either String ba
fromBase base =
    convertFromBase base . encodeUtf8 @Text @ByteString >=>
    fromByteArray @ba @ByteString

-- These are compatible with fmt.
toBase64, toHex :: ByteArrayAccess ba => ba -> Text
toBase64 = toBase Base64
toHex    = toBase Base16

fromBase64, fromHex :: FromByteArray ba => Text -> Either String ba
fromBase64 = fromBase Base64
fromHex    = fromBase Base16

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
