-- | Utilities.

module Dscp.Util
       ( anyMapM
       , listToMaybeWarn
       , allUniqueOrd
       , reportTime
       , Size (..)
       , sizeSerialised

         -- * Exceptions processing
       , wrapRethrow
       , wrapRethrowIO
       , onAnException

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

         -- * Catch errors and report to logs
       , dieGracefully
       , foreverAlive

         -- * Re-exports
       , module Snowdrop.Util
       ) where

import Codec.Serialise (Serialise, serialise)
import Control.Lens (Getter, to)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding (Base (..), convertFromBase, convertToBase)
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt ((+|), (+||), (|+), (||+))
import GHC.TypeLits (KnownSymbol)
import Mon (recordTimer)
import Mon.Network (Endpoint)
import Mon.Types (Name)
import Time (KnownRat, Time, UnitName, threadDelay)

import Loot.Log (MonadLogging, logError, logWarning)

import Snowdrop.Util (NewestFirst (..), OldestFirst (..))

import Dscp.Crypto.ByteArray (FromByteArray (..))

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
        logWarning $ "listToMaybeWarn: too many entries ("+|msg|+")"
        return (Just x)

allUniqueOrd :: Ord a => [a] -> Bool
allUniqueOrd = all (null . drop 1) . group . sort

reportTime :: MonadIO m => Name -> Maybe Endpoint -> m a -> m a
reportTime name mEndpoint m = case mEndpoint of
    Nothing -> m
    Just (endpoint) -> do
        t <- liftIO $ getPOSIXTime
        a <- m
        t' <- liftIO $ getPOSIXTime
        let diff :: Double
            diff = fromRational . toRational $ t' - t
        -- mon accepts only Int as metric value and expects amount of milliseconds in recordTimer
        liftIO $ recordTimer endpoint name 1 [] (round $ diff * 1000)
        return a

-- | Size of serialised item.
-- First phantom type stands for a typeclass corresponding to serialisation
-- method, while the second one is type of item being serialised.
newtype Size a = Size { unSize :: Word64 }
    deriving (Eq, Ord, Show)

-- | Count size of serialised item.
sizeSerialised :: Serialise a => a -> Size a
sizeSerialised = Size . fromIntegral . BSL.length . serialise

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

-- | Similar to 'onException', but provides exception itself.
onAnException :: (MonadCatch m, Exception e) => m a -> (e -> m ()) -> m a
onAnException action onExc = action `catch` \e -> onExc e >> throwM e

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

-----------------------------------------------------------
-- Wrapper, that prints an error happened
-----------------------------------------------------------

dieGracefully :: (MonadLogging m, MonadCatch m) => Text -> m () -> m ()
dieGracefully desc action =
    action `catchAny` \e -> do
        logError $ fromString $ "Exception in " <> toString desc <> ": " <> show e

foreverAlive
    :: (MonadIO m, MonadCatch m, MonadLogging m,
        KnownRat unit, KnownSymbol(UnitName unit))
    => Text -> Time unit -> m () -> m a
foreverAlive name recovery action =
    forever action `catchAny` \e -> do
        printNecrolog e
        threadDelay recovery
        foreverAlive name recovery action
  where
    printNecrolog e =
        logError $ name |+ " died (" +|| e ||+ "); \
                   \ressurecting in " +|| recovery ||+ ""
