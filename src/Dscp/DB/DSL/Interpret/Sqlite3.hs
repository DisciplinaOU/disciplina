
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.DSL.Interpret.Sqlite3
    ( -- Only instances are exported.
    ) where

import Universum

import qualified Data.Set as Set (Set, empty, member, singleton)

import Database.SQLite.Simple hiding (query)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

import Text.InterpolatedString.Perl6 (q, qc, qq)

import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType (..), CourseId (..),
                        Grade (..), SignedSubmission (..), Submission (..), SubmissionSig,
                        SubmissionType (..), SubmissionWitness (..))
import Dscp.Crypto (Hash, PublicKey, Signature)
import Dscp.DB.DSL.Class
import Dscp.DB.SQLite
import Dscp.DB.SQLite.Class
import Dscp.Educator.Txs (PrivateTx (..), PrivateTxId)

-- TODO(kir): split into separate .Instances module.
instance FromField Address
instance FromField CourseId
instance FromField Grade
instance FromField SubmissionSig
instance ToField   PrivateTxId

instance
--    v-- GHC says it cand "find" Monad in superclasses (wat).
    ( Monad m
    , MonadSQLiteDB m
    )
    => MonadSearchTxObj m
  where
    runTxQuery (SELECTTx WHERE (TxIdEq pid)) =
        getPrivateTxFromId (error "get pk from keyring here") pid

    -- TODO (kir): find where the 'Obj'ects live.
    runObjQuery (SELECTObj WHERE (ObjHashEq hash)) =
        return Nothing

    runTxsQuery (SELECTTxs WHERE txFilter) =
        getPrivateTxsByFilter (error "get pk from keyring here") txFilter

getPrivateTxsByFilter
    :: MonadSQLiteDB m
    => PublicKey
    -> TxsFilterExpr
    -> m [PrivateTx]
getPrivateTxsByFilter pk filterExpr = do
    let
      tables = requiredTables filterExpr

      -- The [qc||] cannot occur inside its {}-interpolator :(
      -- Expression [qc| {[qc||]} |] gives an error.
      additionalJoins =
        if Subject `Set.member` tables
        then [qc|

            left join Courses     on  Assignment.course_id       = Courses.id
            left join Subjects    on    Subjects.course_id       = Courses.id

             |]
        else [qc||]

    map (packPrivateTxQuery pk) <$> query
        [qc|
            select    Submissions.student_addr,
                      Submissions.contents_hash,

                      Assignments.course_id,
                      Assignments.contents_hash,
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

buildWhereStatement = go
  where
    go = \case
        TxHasSubjectId sid   -> [qq| Subjects.id = $sid   |]
        TxGradeEq      grade -> [qq| grade       = $grade |]
        TxGrade :>=    grade -> [qq| grade      >= $grade |]

        left :&  right       -> [qq| ({go left}) and ({go right}) |]
        left :|| right       -> [qq| ({go left}) or  ({go right}) |]

        TxHasDescendantOfSubjectId _sid ->
            error "buildWhereStatement: TxHasDescendantOfSubjectId: not supported yet"

getPrivateTxFromId pk tid = do
    pack <- query
        [q|
            select    Submissions.student_addr,
                      Submissions.contents_hash,

                      Assignments.course_id,
                      Assignments.contents_hash,
                      Assignments.desc,

                      Submissions.signature,

                      Transactions.grade,
                      Transactions.time

            from      Transactions

            left join Submissions on             submission_hash = Submissions.hash
            left join Assignments on Submissions.assignment_hash = Assignments.hash

            where     Transactions.hash = ?;
        |]
        (Only tid)

    return $ case pack of
        [queryResult] -> Just (packPrivateTxQuery pk queryResult)
        _other        -> Nothing

packPrivateTxQuery pk
    ( student_addr
    , sub_type
    , course_id
    , assign_type
    , assign_desc
    , sub_sig
    , grade
    , time
    ) = PrivateTx
            (SignedSubmission
                (Submission
                    student_addr
                    (select sub_type Digital Offline)
                    (Assignment
                        course_id
                        (select assign_type Regular CourseFinal)
                        assign_desc))
                (SubmissionWitness
                    pk
                    sub_sig))
            grade
            time
  where
    select i left right =
        if i == (0 :: Integer)
        then left
        else right

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
