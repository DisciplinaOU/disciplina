{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Dscp.Util.Migration
    ( EditionType (..)
    , PrevEditionRef (..)
    , Migratable (..)

    , ensureUpToDate
    , verifyVersionsGrowth
    ) where

import qualified Data.SemVer as V
import Fmt (Buildable (..), (+|), (|+))
import Loot.Log (ModifyLogName, MonadLogging, Name, logDebug, modifyLogName)
import qualified Text.Show

import Dscp.Util

-- | Edition type.
data EditionType a
    = BaseEdition
      -- ^ The very first edition.
    | EditionExtending a
      -- ^ Extends a previous edition.

-- | Reference to the previous edition.
data PrevEditionRef (et :: EditionType *) where
    NullEditionRef :: PrevEditionRef 'BaseEdition
    PrevEditionRef :: Migratable e => Proxy e -> PrevEditionRef ('EditionExtending e)

-- | Generalized custom migrations support.
class Migratable (edition :: *) where
    -- | Object which is subject to migrations.
    type EditionObject edition = obj | obj -> edition
    type EditionObject edition = edition

    -- | Type of this edition.
    type MigrationEditionType edition :: EditionType *

    -- | Proof of the fact that previous version is migratable itself.
    -- Well, it also simplifies migration internals.
    previousEditionRef :: PrevEditionRef (MigrationEditionType edition)

    -- | Run-time value of object version.
    editionVersion :: V.Version
    editionVersion = case previousEditionRef @edition of
        NullEditionRef ->
            V.initial
        PrevEditionRef (_ :: Proxy pe) ->
            V.incrementMajor $ editionVersion @pe

    -- | Constraints on migration monad.
    type MigrationActionConstr edition (m :: * -> *) :: Constraint
    type MigrationActionConstr edition m = ()

    -- | Perform migration from previous version to the current one.
    -- You *have to* make sure that if a migration step throws exception then state
    -- is rollbacked (if possible).
    migrate
        :: (Monad m, MigrationActionConstr edition m)
        => PreviousEditionObject (MigrationEditionType edition)
        -> m (EditionObject edition)

    -- | Read value from the database (or whatever else), assuming
    -- that the database version matches this edition.
    readEdition
        :: (Monad m, MigrationActionConstr edition m)
        => m (EditionObject edition)


type family PreviousEditionObject (et :: EditionType *) where
    PreviousEditionObject 'BaseEdition = ()
    PreviousEditionObject ('EditionExtending e) = EditionObject e

type AllMigrarionActionsConstrs edition m =
    ( MigrationActionConstr edition m
    , AllMigrationActionsConstrs' (MigrationEditionType edition) m
    )

type family AllMigrationActionsConstrs' (et :: EditionType *) m :: Constraint where
    AllMigrationActionsConstrs' 'BaseEdition _ = ()
    AllMigrationActionsConstrs' ('EditionExtending e) m = AllMigrarionActionsConstrs e m

-- | Some inconsistency in versions.
data VersionException
    = -- | Version in storage in too high and is not mentioned in our code.
      StoredVersionIsTooHigh
      { veStored :: V.Version
      , veActual :: V.Version
      }
      -- | Version in storage is lower than the very first version.
    | StoredVersionIsTooLow
      { veStored :: V.Version
      , veBase   :: V.Version
      }
      -- | Version in storage somehow occured between two neighbor versions
      -- in code.
    | StoredVersionLostMidway
      { veStored :: V.Version
      , veLower  :: V.Version
      , veHigher :: V.Version
      }

instance Buildable VersionException where
    build = \case
        StoredVersionIsTooHigh{..} ->
            "Version " <> V.toBuilder veStored <> " is higher than \
            \any known version. The latest one is " <> V.toBuilder veActual
        StoredVersionIsTooLow{..} ->
            "Version in storage " <> V.toBuilder veStored <> " \
            \is even smaller than base version " <> V.toBuilder veBase
        StoredVersionLostMidway{..} ->
            "Version in storage " <> V.toBuilder veStored <> " is \
            \between two actual neighbor versions " <>
            V.toBuilder veLower <> " and " <> V.toBuilder veHigher

-- | Migration exception.
data MigrationException
    = VersionError VersionException
    | StoreReadFailed SomeException
    | MigrationFailed
      { mfFrom  :: V.Version
      , mfTo    :: V.Version
      , mfError :: SomeException
      }

instance Show MigrationException where
    show = toString . pretty

instance Buildable MigrationException where
    build = \case
        VersionError err -> build err
        StoreReadFailed (SomeException err) -> show err
        MigrationFailed{..} ->
            "Migration " +| V.toBuilder mfFrom |+ " -> " +| V.toBuilder mfTo |+
            "failed: " +| case mfError of SomeException e -> show @String e |+ ""

instance Exception MigrationException

-- | Perform required migrations from a given version to the actual one.
ensureUpToDate
    :: forall edition m.
       ( Migratable edition
       , MonadCatch m, MonadLogging m, ModifyLogName m
       , AllMigrarionActionsConstrs edition m
       )
    => Name -> V.Version -> m (EditionObject edition)
ensureUpToDate migrationName storedVersion =
    modifyLogName (<> "migrate" <> migrationName) $
        ensureUpToDateRec Nothing storedVersion <* logDebug "Migration complete"

ensureUpToDateRec
    :: forall edition m.
       ( Migratable edition
       , AllMigrarionActionsConstrs edition m
       , MonadCatch m, MonadLogging m
       )
    => Maybe V.Version -> V.Version -> m (EditionObject edition)
ensureUpToDateRec lastTopDownVersion storedVersion
    | curVersion < storedVersion =
        case lastTopDownVersion of
            Nothing ->
                throwM $ VersionError StoredVersionIsTooHigh
                { veStored = storedVersion, veActual = curVersion }
            Just higherVersion ->
                throwM $ VersionError StoredVersionLostMidway
                { veStored = storedVersion, veLower = curVersion
                , veHigher = higherVersion }

    | curVersion == storedVersion = do
        case lastTopDownVersion of
            Nothing -> logDebug "Storage is up to date"
            Just _ -> logDebug $ "Storage version is " +| V.toBuilder curVersion
                              |+ ", preparing migration"
        readEdition & wrapRethrow StoreReadFailed

    | otherwise = case previousEditionRef @edition of
        NullEditionRef ->
            throwM $ VersionError StoredVersionIsTooLow
            { veStored = storedVersion, veBase = curVersion }

        PrevEditionRef (_ :: Proxy pe) -> do
            prev <- ensureUpToDateRec (Just curVersion) storedVersion

            logDebug $ "Migrating to version " +| V.toBuilder curVersion |+ ""
            cur <- migrate prev
                 & wrapRethrow (MigrationFailed (editionVersion @pe) curVersion)
            logDebug $ "Finished migration to version " +| V.toBuilder curVersion |+ ""
            return cur
  where
    curVersion = editionVersion @edition

-- | Check that versions grow sanely over editions.
verifyVersionsGrowth
    :: forall edition.
       Migratable edition
    => Either Text ()
verifyVersionsGrowth =
    case previousEditionRef @edition of
        NullEditionRef -> pass
        PrevEditionRef (_ :: Proxy prevEdition) ->
            let ver = editionVersion @edition
                pver = editionVersion @prevEdition
            in if editionVersion @prevEdition >= editionVersion @edition
                then Left $ "Non-monotonic versions growth: \
                             \version " <> V.toText ver <> " goes after \
                             \version " <> V.toText pver
                else verifyVersionsGrowth @prevEdition


data V0 = V0 Int

instance Migratable V0 where
    type MigrationEditionType V0 = 'BaseEdition

    previousEditionRef = NullEditionRef

    type MigrationActionConstr V0 m = MonadReader Int m

    migrate () = V0 <$> ask

    readEdition = return $ V0 10

data V1 = V1 Int Text

instance Migratable V1 where
    type MigrationEditionType V1 = 'EditionExtending V0

    previousEditionRef = PrevEditionRef Proxy

    type MigrationActionConstr V1 m = MonadState Double m

    migrate (V0 i) = return (V1 i "")

    readEdition = return $ V1 10 "kek"
