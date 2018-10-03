-- | Utilities.

module Dscp.Util
       ( anyMapM
       , listToMaybeWarn
       , allUniqueOrd
       , Size (..)
       , sizeSerialised
       , Seed (..)
       , execUnmasked

         -- * Exceptions processing
       , wrapRethrow
       , wrapRethrowIO
       , onAnException

         -- * Error handling
       , assert
       , assertJust

         -- * Maybe conversions
       , nothingToThrow
       , nothingToError
       , nothingToFail
       , nothingToPanic

         -- * Either conversions
       , leftToThrow
       , leftToFail
       , leftToPanic
       , leftToFailWith
       , leftToPanicWith
       , eitherToMaybe
       , mappendLefts

         -- * Formatting
       , Base (..)
       , toBase
       , fromBase
       , toBase64
       , fromBase64
       , toHex
       , fromHex

         -- * Lenses
       , _headNE
       , _tailNE
       , seeOnly
       , postfixLFields

         -- * String-functions adopted
       , symbolValT

         -- * Ids for databases
       , HasId (..)
       , idOf

         -- * Catch errors and report to logs
       , dieGracefully

         -- * Re-exports
       , module Snowdrop.Util
       ) where

import Codec.Serialise (Serialise, serialise)
import Control.Lens (Getter, LensRules, lens, lensField, lensRules, mappingNamer, to)
import Control.Monad.Except (MonadError (..))
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding (Base (..), convertFromBase, convertToBase)
import qualified Data.ByteString.Lazy as BSL
import Fmt ((+|), (|+))
import GHC.TypeLits (KnownSymbol, symbolVal)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UIO

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

-- | Size of serialised item.
newtype Size a = Size { unSize :: Word64 }
    deriving (Eq, Ord, Show)

-- | Count size of serialised item.
sizeSerialised :: Serialise a => a -> Size a
sizeSerialised = Size . fromIntegral . BSL.length . serialise

-- | Seed for deterministic random generator.
newtype Seed a = Seed { unSeed :: a }
    deriving (Eq, Ord, Show, Enum, Num, IsString)

-- | Executes the action unmasked.
-- Spawns a thread under hood.
execUnmasked :: MonadUnliftIO m => m a -> m a
execUnmasked action =
    UIO.asyncWithUnmask (\doUnmask -> doUnmask action) >>= UIO.wait

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

nothingToError :: MonadError e m => e -> Maybe a -> m a
nothingToError e = maybe (throwError e) pure

nothingToFail :: MonadFail m => Text -> Maybe a -> m a
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
    :: forall s m a.
       (MonadFail m, ToString s)
    => Either s a -> m a
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

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (\_ -> Nothing) Just

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
-- Lens fun
-----------------------------------------------------------

_headNE :: Lens' (NonEmpty a) a
_headNE f (x :| l) = ( :| l) <$> f x

_tailNE :: Lens' (NonEmpty a) [a]
_tailNE f (x :| l) = (x :| ) <$> f l

-- | Lens that always returns given value as getter, and does nothing as setter.
-- NOTE: this function violates lens rules, and it's only useful till the moment
-- we switch to `HasGetters` for our monadic constraints.
seeOnly :: b -> Lens' a b
seeOnly b = lens (const b) const

-- | For datatype with "myNyan" field it will create "myNyanL" lens.
postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])

-----------------------------------------------------------
-- Adopted functions which work with strings
-----------------------------------------------------------

symbolValT :: forall s. KnownSymbol s => Text
symbolValT = toText $ symbolVal (Proxy @s)

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

instance HasId ()

-----------------------------------------------------------
-- Wrapper, that prints an error happened
-----------------------------------------------------------

dieGracefully :: (MonadLogging m, MonadCatch m) => Text -> m () -> m ()
dieGracefully desc action =
    action `catchAny` \e -> do
        logError $ fromString $ "Exception in " <> toString desc <> ": " <> show e
