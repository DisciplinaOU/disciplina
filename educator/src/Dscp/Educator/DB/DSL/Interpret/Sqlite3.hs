{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.Educator.DB.DSL.Interpret.Sqlite3 () where

import Universum

import qualified Dscp.Core.Foundation as Core
import Dscp.DB.SQL
import Dscp.Educator.DB.DSL.Class
import Dscp.Educator.DB.Queries
-- import Dscp.Educator.DB.Schema

instance
    ( MonadIO n
    , MonadCatch n
    , m ~ DBT t n
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
    => TxsFilterExpr -> DBT t m [Core.PrivateTx]
getPrivateTxsByFilter _ = error "NOT NECESSARY, NOT IMPLEMENTED" -- do
    -- runSelectMap privateTxFromRow . select $ do
    --     privateTx <- all_ (esTransactions educatorSchema)
    --     submission <- related_ (esSubmissions educatorSchema) (trSubmission privateTx)
    --     -- some of rows below may be extra ones
    --     assignment <- related_ (esAssignments educatorSchema) (srAssignment submission)
    --     course <- related_ (esCourses educatorSchema) (arCourse assignment)
    --     subject <- all_ (esSubjects educatorSchema)
    --     guard_ (srCourse subject `references_` course)

    --     guard_ $ buildWhereStatement
    --         (srId subject, trGrade privateTx)
    --         filterExpr
    --     return (privateTx, submission)

-- buildWhereStatement
--     :: _
--     => (QGenExpr syntax ctx s Core.Subject, QGenExpr syntax ctx s Core.Grade)
--     -> TxsFilterExpr
--     -> QGenExpr syntax ctx s Bool
-- buildWhereStatement (subjExpr, gradeExpr) = go
--   where
--     go = \case
--         TxHasSubjectId sid   -> subjExpr ==. val_ sid
--         TxGrade :==    grade -> gradeExpr ==. val_ grade
--         TxGrade :>=    grade -> gradeExpr >=. val_ grade

--         left :&  right       -> go left &&. go right
--         left :|| right       -> go left ||. go right

--         TxHasDescendantOfSubjectId _sid ->
--             error "buildWhereStatement: TxHasDescendantOfSubjectId: not supported yet"
