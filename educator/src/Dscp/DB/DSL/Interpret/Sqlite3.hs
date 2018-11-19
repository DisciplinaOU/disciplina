{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.DB.DSL.Interpret.Sqlite3 () where

import qualified Data.Set as Set (Set, empty, singleton)
import Database.Beam.Query (all_, related_, select)

import qualified Dscp.Core.Foundation as Core
import Dscp.DB.DSL.Class
import Dscp.DB.SQLite

instance
    ( MonadIO n
    , MonadCatch n
    , m ~ DBT t w n
    )
    => MonadSearchTxObj m
  where
    runTxQuery (SELECTTx WHERE (TxIdEq pid)) =
        getTransaction pid

    -- TODO (kir): find where the 'Obj'ects live.
    runObjQuery (SELECTObj WHERE (ObjHashEq _hash)) =
        return Nothing

    runTxsQuery (SELECTTxs WHERE txFilter) =
        getPrivateTxsByFilter txFilter

getPrivateTxsByFilter
    :: MonadIO m
    => TxsFilterExpr -> DBT t w m [Core.PrivateTx]
getPrivateTxsByFilter filterExpr = do
    runSelectMap privateTxFromRow . select $ do
        privateTx <- all_ (esTransactions educatorSchema)
        submission <- related_ (esSubmissions educatorSchema) (trSubmissionHash privateTx)
        -- some of rows below may be extra
        assignment <- related_ (esAssignments educatorSchema) (srAssignmentHash submission)
        course <- related_ (esCourses educatorSchema) (arCourse assignment)
        subject <- all_ (esSubjects educatorSchema)
        guard_ (srCourse subject `references_` course)

        guard_ $ buildWhereStatement
            (srId subject, trGrade privateTx)
            filterExpr
        return (privateTx, submission)

buildWhereStatement
    :: _
    => (QGenExpr syntax ctx s Core.Subject, QGenExpr syntax ctx s Core.Grade)
    -> TxsFilterExpr
    -> QGenExpr syntax ctx s Bool
buildWhereStatement (subjExpr, gradeExpr) = go
  where
    go = \case
        TxHasSubjectId sid   -> subjExpr ==. val_ sid
        TxGrade :==    grade -> gradeExpr ==. val_ grade
        TxGrade :>=    grade -> gradeExpr >=. val_ grade

        left :&  right       -> go left &&. go right
        left :|| right       -> go left ||. go right

        TxHasDescendantOfSubjectId _sid ->
            error "buildWhereStatement: TxHasDescendantOfSubjectId: not supported yet"

data RequiredTables
    = Subject
    deriving (Eq, Ord)

_requiredTables :: TxsFilterExpr -> Set.Set RequiredTables
_requiredTables = \case
    TxHasSubjectId             _ -> Set.singleton Subject
    TxHasDescendantOfSubjectId _ -> Set.singleton Subject
    (:||) a b                    -> _requiredTables a <> _requiredTables b
    (:&)  a b                    -> _requiredTables a <> _requiredTables b
    _other                       -> Set.empty
