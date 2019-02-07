-- | Utilities.

module Dscp.Util
       ( anyMapM
       , listToMaybeWarn
       , listToMaybeWarnM
       , allUniqueOrd
       , Size (..)
       , sizeSerialised
       , Seed (..)
       , execUnmasked
       , fromIntegralChecked
       , groupToAssoc

         -- * Exceptions processing
       , wrapRethrow
       , wrapRethrowIO

         -- * Error handling
       , assert
       , assertJust
       , assert_

         -- * Maybe conversions
       , nothingToThrow
       , nothingToError
       , nothingToFail
       , nothingToPanic
       , (<?:>)

         -- * Either conversions
       , leftToThrow
       , leftToFail
       , leftToPanic
       , leftToFailWith
       , leftToPanicWith
       , eitherToMaybe
       , mappendLefts

         -- * Unsafe conversions
       , oneOrError
       , maybeOneOrError

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
       , (&:)

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
import qualified Control.Exception as E
import Control.Lens (Getter, LensRules, lens, lensField, lensRules, mappingNamer, to)
import Control.Monad.Except (MonadError (..))
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding (Base (..), convertFromBase, convertToBase)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Data.Typeable (typeRep)
import qualified GHC.Exts as Exts
import GHC.TypeLits (KnownSymbol, symbolVal)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UIO

import Loot.Log (ModifyLogName (..), MonadLogging, NameSelector (CallstackName), logError,
                 logWarning)

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

listToMaybeWarn
    :: (Monad m, MonadLogging m, ModifyLogName m)
    => [a] -> m (Maybe a)
listToMaybeWarn = \case
    [] -> pure Nothing
    [x] -> pure (Just x)
    (x:_) -> do
        modifyLogNameSel (const CallstackName) $
            logWarning $ "listToMaybeWarn: too many entries"
        return (Just x)

listToMaybeWarnM
    :: (Monad m, MonadLogging m, ModifyLogName m)
    => m [a] -> m (Maybe a)
listToMaybeWarnM action = action >>= listToMaybeWarn

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

-- | Like 'fromIntegral', but ensures there is no overflow.
fromIntegralChecked
    :: forall b a.
       (HasCallStack, Each [Integral, Num] [a, b])
    => a -> b
fromIntegralChecked x =
    let r = fromIntegral x
    in if fromIntegral r == x
          then r
          else error "Integral overflow"

groupToAssoc :: Ord k => [(k, v)] -> [(k, NonEmpty v)]
groupToAssoc =
    M.toList . fmap Exts.fromList . foldl' push M.empty
  where
    push acc a = M.insertWith (++) (fst a) [snd a] acc

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

-- | Ensure given condition holds.
-- You have to force evaluation of result in order for check to take effect.
assert_ :: HasCallStack => (a -> Bool) -> a -> ()
assert_ predicate value = E.assert (predicate value) ()

-----------------------------------------------------------
-- Maybe conversions
-----------------------------------------------------------

nothingToThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
nothingToThrow e = maybe (throwM e) pure

nothingToError :: MonadError e m => e -> Maybe a -> m a
nothingToError e = maybe (throwError e) pure

nothingToFail :: MonadFail m => Text -> Maybe a -> m a
nothingToFail e = maybe (fail $ toString e) pure

nothingToPanic :: HasCallStack => Text -> Maybe a -> a
nothingToPanic e = fromMaybe (error e)

-- | Like '?:' from Universum, for monads.
infixr 2 <?:>
(<?:>) :: Functor f => f (Maybe a) -> a -> f a
a <?:> b = fromMaybe b <$> a

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
-- Unsafe conversions
-----------------------------------------------------------

-- | Expects the list to contain only one entry, panics otherwise.
oneOrError :: forall a. (Typeable a, HasCallStack) => [a] -> a
oneOrError = \case
    []  -> error $ "No items " <> tyName
    [x] -> x
    l   -> error $ "Too many items: " <> show (length l) <> " " <> tyName
  where
    tyRep = typeRep (Proxy @a)
    tyName = "(" <> show tyRep <> ")"

-- | Expects the list to contain zero or one entry, panics otherwise.
maybeOneOrError :: forall a. (Typeable a, HasCallStack) => [a] -> Maybe a
maybeOneOrError = \case
    []  -> Nothing
    [x] -> Just x
    l   -> error $ "Too many items: " <> show (length l) <> " " <> tyName
  where
    tyRep = typeRep (Proxy @a)
    tyName = "(" <> show tyRep <> ")"

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

-- | Version of 'Control.Lens.&~' with modified priority, suitable for defining
-- configs in do-notaion.
infixl 9 &:
(&:) :: a -> State a () -> a
(&:) = flip execState

-----------------------------------------------------------
-- Adopted functions which work with strings
-----------------------------------------------------------

symbolValT :: forall s. KnownSymbol s => Text
symbolValT = toText $ symbolVal (Proxy @s)

-----------------------------------------------------------
-- Helper to establish notion of SQL/db ID
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

-----------------------------------------------------------
-- Instances
-----------------------------------------------------------

instance (n1 ~ n2) => IsLabel n1 (Proxy n2) where
    fromLabel = Proxy
