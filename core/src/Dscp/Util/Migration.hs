{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Dscp.Util.Migration
    ( -- * Migration datatypes
      EditionType (..)
    , PrevEditionRef (..)
    , Migratable (..)

      -- * Performing migration
    , FullMigration (..)
    , fullMigrationDesc
    , prepareMigration
    , verifyVersionsGrowth
    ) where

import qualified Data.SemVer as V
import qualified Data.Sequence as Seq
import Fmt (Buildable (..), Builder, (+|), (|+))
import qualified Text.Show

import Dscp.Util

----------------------------------------------------------------------------
-- Basis
----------------------------------------------------------------------------

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
    type MigrationActionConstr edition m = Monad m

    -- | Perform migration from previous version to the current one.
    migrate
        :: (Monad m, MigrationActionConstr edition m)
        => PreviousEditionObject (MigrationEditionType edition)
        -> m (EditionObject edition)

-- | Get previous version of migratable object.
type family PreviousEditionObject (et :: EditionType *) where
    PreviousEditionObject 'BaseEdition = ()
    PreviousEditionObject ('EditionExtending e) = EditionObject e

-- | Summary constraint required to run any migration to the given edition.
type AllMigrarionActionsConstrs edition m =
    ( MigrationActionConstr edition m
    , AllMigrationActionsConstrs' (MigrationEditionType edition) m
    )

type family AllMigrationActionsConstrs' (et :: EditionType *) m :: Constraint where
    AllMigrationActionsConstrs' 'BaseEdition _ = ()
    AllMigrationActionsConstrs' ('EditionExtending e) m = AllMigrarionActionsConstrs e m

----------------------------------------------------------------------------
-- Gathering migration plan
----------------------------------------------------------------------------

-- TODO: split into modules

-- | Some inconsistency in versions.
data MigrationVersioningException
    = -- | Version in storage in too high and is not mentioned in our code.
      StoredVersionIsTooHigh
      { veStored :: V.Version
      , veActual :: V.Version
      }
    | -- | Version in storage is lower than the very first version.
      StoredVersionIsTooLow
      { veStored :: V.Version
      , veBase   :: V.Version
      }
    | -- | Version in storage somehow occured between two neighbor versions
      -- in code.
      StoredVersionLostMidway
      { veStored :: V.Version
      , veLower  :: V.Version
      , veHigher :: V.Version
      }

instance Show MigrationVersioningException where
    show = toString . pretty

instance Buildable MigrationVersioningException where
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

instance Exception MigrationVersioningException

-- | One of migration steps has raised an error.
data MigrationException = MigrationException
    { mfFrom  :: V.Version
    , mfTo    :: V.Version
    , mfError :: SomeException
    }

instance Show MigrationException where
    show = toString . pretty

instance Buildable MigrationException where
    build MigrationException{..} =
        "Migration " +| V.toBuilder mfFrom |+ " -> " +| V.toBuilder mfTo |+
        "failed: " +| case mfError of SomeException e -> show @String e |+ ""

instance Exception MigrationException

-- | Requires the following list of constraints to hold for each edition
-- starting from the given one down.
type family ForEachEditionUpTo (constrs :: [* -> Constraint]) edition :: Constraint where
    ForEachEditionUpTo constrs edition =
        ( Each constrs '[EditionObject edition]
        , ForEachEditionUpTo' constrs (MigrationEditionType edition)
        )

type family ForEachEditionUpTo' (constrs :: [* -> Constraint]) (et :: EditionType *)
              :: Constraint where
    ForEachEditionUpTo' constrs 'BaseEdition = ()
    ForEachEditionUpTo' constrs ('EditionExtending e) = ForEachEditionUpTo constrs e

-- | Information about migration between two adjacent versions.
data MigrationStepMeta = MigrationStepMeta
    { fmsVersionFrom :: V.Version
    , fmsVersionTo   :: V.Version
    }

-- | Migration from some version to the @edition@ version.
--
-- The object you migrate from will satisfy every constraint in @constrs@ list
-- (for that, of course, object of each edition in the chain has to satisfy those).
--
-- Monad @m@ is the one where 'migrate' is executed and should satisfy
-- 'MigrationActionConstr' constraint for every edition in the chain.
data FullMigration constrs m edition =
    forall curObj. Each constrs '[curObj] => FullMigration
    { runFullMigration   :: curObj -> m (EditionObject edition)
    , fullMigrationSteps :: Seq.Seq MigrationStepMeta
    }

-- | Migration which does nothing.
fmLeaveAsIs
    :: (Applicative m, Each constrs '[EditionObject edition])
    => FullMigration constrs m edition
fmLeaveAsIs = FullMigration pure mempty

-- | Make a full description of prepared migration.
fullMigrationDesc :: FullMigration constrs m edition -> Builder
fullMigrationDesc fm = case toList (fullMigrationSteps fm) of
    [] -> "Data in storage is up to date"
    (step : steps) ->
        let versions = fmsVersionFrom step : map fmsVersionTo (step : steps)
            versionsT = mconcat $ intersperse " -> " $ map show versions
        in "Going to perform a migration: " <> versionsT

-- | Perform required migrations from a given version to the actual one.
prepareMigration
    :: forall edition n constrs m.
       ( Migratable edition
       , ForEachEditionUpTo constrs edition
       , MonadThrow m
       , MonadCatch n, AllMigrarionActionsConstrs edition n
       )
    => V.Version -> m (FullMigration constrs n edition)
prepareMigration storedVersion =
    prepareMigrationRec Nothing storedVersion

prepareMigrationRec
    :: forall edition n constrs m.
       ( Migratable edition
       , ForEachEditionUpTo constrs edition
       , MonadThrow m
       , MonadCatch n, AllMigrarionActionsConstrs edition n
       )
    => Maybe V.Version -> V.Version -> m (FullMigration constrs n edition)
prepareMigrationRec lastTopDownVersion storedVersion
    | curVersion < storedVersion =
        case lastTopDownVersion of
            Nothing ->
                throwM $ StoredVersionIsTooHigh
                { veStored = storedVersion, veActual = curVersion }
            Just higherVersion ->
                throwM $ StoredVersionLostMidway
                { veStored = storedVersion, veLower = curVersion
                , veHigher = higherVersion }

    | curVersion == storedVersion =
        return fmLeaveAsIs

    | otherwise = case previousEditionRef @edition of
        NullEditionRef ->
            throwM $ StoredVersionIsTooLow
            { veStored = storedVersion, veBase = curVersion }

        PrevEditionRef (_ :: Proxy pe) -> do
            FullMigration{..} <- prepareMigrationRec @pe @n @constrs
                                 (Just curVersion) storedVersion
            return FullMigration
                { runFullMigration = \base -> do
                    prev <- runFullMigration base
                    migrate prev &
                        wrapRethrow (MigrationException (editionVersion @pe) curVersion)
                , fullMigrationSteps = fullMigrationSteps Seq.|> MigrationStepMeta
                        { fmsVersionFrom = editionVersion @pe
                        , fmsVersionTo = curVersion
                        }
                }
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
    deriving (Show)

instance Migratable V0 where
    type MigrationEditionType V0 = 'BaseEdition

    previousEditionRef = NullEditionRef

    type MigrationActionConstr V0 m = MonadReader Int m

    migrate () = V0 <$> ask

data V1 = V1 Int Text

instance Migratable V1 where
    type MigrationEditionType V1 = 'EditionExtending V0

    previousEditionRef = PrevEditionRef Proxy

    type MigrationActionConstr V1 m = MonadState Double m

    migrate (V0 i) = return (V1 i "")
