{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.DSL.Interpret.Sqlite3 () where

import qualified Data.Set as Set (Set, empty, member, singleton)

import Text.InterpolatedString.Perl6 (qc, qq)

import Dscp.Core.Foundation (PrivateTx (..))
import Dscp.DB.DSL.Class
import Dscp.DB.SQLite

instance
--    v-- GHC says it cand "find" Monad in superclasses (wat).
    ( Monad m
    , MonadSQLiteDB m
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
    :: MonadSQLiteDB m
    => TxsFilterExpr
    -> m [PrivateTx]
getPrivateTxsByFilter filterExpr = do
    let
      tables = requiredTables filterExpr

      -- The [qc||] cannot occur inside its {}-interpolator :(
      -- Expression [qc| {[qc||]} |] gives an error.
      additionalJoins :: Text
      additionalJoins =
        if Subject `Set.member` tables
        then [qc|

            left join Courses     on  Assignment.course_id       = Courses.id
            left join Subjects    on    Subjects.course_id       = Courses.id

             |]
        else [qc||]

    query
        [qc|
            select    Submissions.student_addr,
                      Submissions.contents_hash,

                      Assignments.course_id,
                      Assignments.contents_hash,
                      Assignments.type
                      Assignments.desc,

                      Submissions.signature,

                      Transactions.grade,
                      Transactions.time

            from      Transactions

            left join Submissions on             submission_hash = Submissions.hash
            left join Assignments on Submissions.assignment_hash = Assignments.hash
            {
                additionalJoins
            }
            where {buildWhereStatement filterExpr}
            ;
        |]
        ()

buildWhereStatement :: TxsFilterExpr -> Text
buildWhereStatement = go
  where
    go = \case
        TxHasSubjectId sid   -> [qq| Subjects.id = $sid   |]
        TxGrade :==    grade -> [qq| grade       = $grade |]
        TxGrade :>=    grade -> [qq| grade      >= $grade |]

        left :&  right       -> [qq| ({go left}) and ({go right}) |]
        left :|| right       -> [qq| ({go left}) or  ({go right}) |]

        TxHasDescendantOfSubjectId _sid ->
            error "buildWhereStatement: TxHasDescendantOfSubjectId: not supported yet"

data RequiredTables
    = Subject
    deriving (Eq, Ord)

requiredTables :: TxsFilterExpr -> Set.Set RequiredTables
requiredTables = \case
    TxHasSubjectId             _ -> Set.singleton Subject
    TxHasDescendantOfSubjectId _ -> Set.singleton Subject
    (:||) a b                    -> requiredTables a <> requiredTables b
    (:&)  a b                    -> requiredTables a <> requiredTables b
    _other                       -> Set.empty
