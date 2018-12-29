-- | Helpers for comprehensive relations testing.
module Test.Dscp.Educator.Web.Relations
    ( DepRelationTestActions (..)
    , RelationTestActions (..)
    , buildRelationTestScenarios
    ) where

import Test.Hspec.Core.Spec (SpecWith)
import Test.QuickCheck.Property (failed, reason)

import Dscp.DB.SQL
import Dscp.Educator.DB
import Dscp.Util.Test

import Test.Dscp.DB.SQL.Mode
import Test.Dscp.Educator.Mode

data DepRelationTestActions lid rid m = DepRelationTestActions
    { drtaName    :: String
      -- ^ Relation name
    , drtaPrepare :: lid -> rid -> m ()
      -- ^ Create dependant relation, assuming that dependencies of the main relation
      -- are set up.
    , drtaDomain  :: Traversal' DomainErrorItem ()
      -- ^ Error domain of this relation.
    , drtaDelete  :: Maybe (lid -> rid -> m ())
      -- ^ Delete the relation.
    }

-- | Describes components related to testing a relation between two entities.
data RelationTestActions lid rid m = RelationTestActions
    { rtsLeftName        :: String
      -- ^ Name of the left dependency as it mentioned in test tree.
    , rtsPrepareLeftDep  :: PropertyM m lid
      -- ^ Create an entity for the left part of the relation.
    , rtsLeftDepDomain   :: Traversal' DomainErrorItem ()
      -- ^ Error domain of the left dependency.

    , rtsRightName       :: String
      -- ^ Name of the right dependency as it mentioned in test tree.
    , rtsPrepareRightDep :: PropertyM m rid
      -- ^ Create an entity for the right part of the relation.
    , rtsRightDepDomain  :: Traversal' DomainErrorItem ()
      -- ^ Error domain of the right dependency.

    , rtsDepRelation     :: Maybe $ DepRelationTestActions lid rid m
      -- ^ When our relation depends on another relation, specify actions for it.

    , rtsCreate          :: lid -> rid -> m ()
      -- ^ Create relation itself.
    , rtsDomain          :: Traversal' DomainErrorItem ()
      -- ^ Error domain of the relation itself.
    , rtsExists          :: lid -> rid -> m Bool
      -- ^ Check whether relation exists.

    , rtsDeleteLeftDep   :: Maybe (lid -> m ())
      -- ^ Delete the left dependency.
    , rtsDeleteRightDep  :: Maybe (rid -> m () )
      -- ^ Delete the right dependency.
    , rtsDelete          :: Maybe (lid -> rid -> m ())
      -- ^ Delete the relation.
    }

-- | Build test spec for given relation.
--
-- This will test properties like "dependencies should exist before relation is created".
-- If deleting action is provided, then properties involving deletion will be added as well;
-- here we assume that dependencies cannot be deleted while a relation which links them
-- exists (like SQL's RESTRICT).
buildRelationTestScenarios
    :: forall lid rid t m.
       ( Each [Show, Arbitrary, Typeable] [lid, rid]
       , m ~ DBT t TestEducatorM
       )
    => RelationTestActions lid rid m -> SpecWith PostgresTestServer
buildRelationTestScenarios RelationTestActions{..} = do
    it ("Relation cannot be created without dependencies") $
        sqlPropertyM $ do
            (lid, rid) <- pick arbitrary
            let someDepDomain = rtsLeftDepDomain <> rtsRightDepDomain
            lift . throwsPrism (_AbsentError . someDepDomain) $
                rtsCreate lid rid

    it ("Relation cannot be created without " <> rtsLeftName <> " dependency") $
        sqlPropertyM $ do
            rid <- pick arbitrary
            lid <- rtsPrepareLeftDep
            lift . throwsPrism (_AbsentError . rtsRightDepDomain) $
                rtsCreate lid rid

    it ("Relation cannot be created without " <> rtsRightName <> " dependency") $
        sqlPropertyM $ do
            lid <- pick arbitrary
            rid <- rtsPrepareRightDep
            lift . throwsPrism (_AbsentError . rtsLeftDepDomain) $
                rtsCreate lid rid

    whenJust rtsDepRelation $ \DepRelationTestActions{..} ->
        it ("Relation cannot be created without " <> drtaName <> " dependency") $
            sqlPropertyM $ do
                lid <- rtsPrepareLeftDep
                rid <- rtsPrepareRightDep
                lift . throwsPrism (_AbsentError . drtaDomain) $
                    rtsCreate lid rid

    let createAll :: PropertyM m (lid, rid)
        createAll = do
            lid <- rtsPrepareLeftDep
            rid <- rtsPrepareRightDep
            lift $ whenJust rtsDepRelation $
                \depRel -> drtaPrepare depRel lid rid
            lift $ rtsCreate lid rid
            return (lid, rid)

    it ("Relation can be normally created") $
        sqlPropertyM $ do
            (lid, rid) <- createAll
            exists <- lift $ rtsExists lid rid
            unless exists $
                stop failed{ reason = "Relation wasn't created" }

    it ("Cannot create same relation twice") $
        sqlPropertyM $ do
            (lid, rid) <- createAll
            lift . throwsPrism (_AlreadyPresentError . rtsDomain) $
                rtsCreate lid rid

    whenJust rtsDeleteLeftDep $ \doDeleteLeftDep -> do
        it ("Cannot delete " <> rtsLeftName <> " dependency while relation exists") $
            sqlPropertyM $ do
                (lid, _) <- createAll
                lift . throwsPrism (_AlreadyPresentError . rtsLeftDepDomain) $
                    doDeleteLeftDep lid

    whenJust rtsDeleteRightDep $ \doDeleteRightDep -> do
        it ("Cannot delete " <> rtsRightName <> " dependency while relation exists") $
            sqlPropertyM $ do
                (_, rid) <- createAll
                lift . throwsPrism (_AlreadyPresentError . rtsRightDepDomain) $
                    doDeleteRightDep rid

    whenJust rtsDepRelation $ \DepRelationTestActions{..} ->
        whenJust drtaDelete $ \doDeleteDepRelation -> do
            it ("Cannot delete " <> drtaName <> " while main relation exists") $
                sqlPropertyM $ do
                    (lid, rid) <- createAll
                    lift . throwsPrism (_AlreadyPresentError . drtaDomain) $
                        doDeleteDepRelation lid rid

    whenJust rtsDelete $ \doDelete -> do
        it ("Deletion of non-existing relation fails properly") $
            sqlPropertyM $ do
                (lid, rid) <- pick arbitrary
                lift . throwsPrism (_AbsentError . rtsDomain) $
                    doDelete lid rid

        it ("Can delete relation properly") $
            sqlPropertyM $ do
                (lid, rid) <- createAll
                lift $ doDelete lid rid

                exists <- lift $ rtsExists lid rid
                when exists $
                    stop failed{ reason = "Relation wasn't deleted" }
