{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.DB.DSL.Interpret.Sqlite3 () where

import Database.Beam.Query (all_, related_, select)

import qualified Dscp.Core.Foundation as Core
import Dscp.DB.DSL.Class
import Dscp.DB.SQLite

instance
    ( MonadQuery cmd be hdl m
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
    :: MonadQuery cmd be hdl m
    => TxsFilterExpr -> m [Core.PrivateTx]
getPrivateTxsByFilter filterExpr = do
    runSelectMap privateTxFromRow . select $ do
        privateTx <- all_ (esTransactions educatorSchema)
        submission <- related_ (esSubmissions educatorSchema) (trSubmission privateTx)
        -- some of rows below may be extra ones
        assignment <- related_ (esAssignments educatorSchema) (srAssignment submission)
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
