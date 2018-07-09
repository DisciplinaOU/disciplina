
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.DSL.Interpret.Sqlite3 () where

import qualified Data.Set as Set (Set, empty, member, singleton)
import Data.Time.Clock (UTCTime)

import Database.SQLite.Simple (Only (..))

import Text.InterpolatedString.Perl6 (q, qc, qq)

import Dscp.Core.Types (Assignment (..), AssignmentType (..), CourseId (..), Grade (..),
                        SignedSubmission (..), StudentId, Submission (..), SubmissionSig,
                        SubmissionWitness (..))
import Dscp.Crypto (PublicKey, fromByteArray)
import Dscp.DB.DSL.Class
import Dscp.DB.SQLite
import Dscp.Educator.Txs (PrivateTx (..), PrivateTxId)

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
    runObjQuery (SELECTObj WHERE (ObjHashEq _hash)) =
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
      additionalJoins :: Text
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
        TxGradeEq      grade -> [qq| grade       = $grade |]
        TxGrade :>=    grade -> [qq| grade      >= $grade |]

        left :&  right       -> [qq| ({go left}) and ({go right}) |]
        left :|| right       -> [qq| ({go left}) or  ({go right}) |]

        TxHasDescendantOfSubjectId _sid ->
            error "buildWhereStatement: TxHasDescendantOfSubjectId: not supported yet"

getPrivateTxFromId :: MonadSQLiteDB m => PublicKey -> PrivateTxId -> m (Maybe PrivateTx)
getPrivateTxFromId pk tid = do
    pack <- query
        [q|
            select    Submissions.student_addr,
                      Submissions.contents_hash,

                      Assignments.course_id,
                      Assignments.contents_hash,
                      Assignments.type,
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

packPrivateTxQuery
    :: PublicKey
    ->  ( StudentId
        , ByteString
        , CourseId
        , ByteString
        , Integer
        , Text
        , SubmissionSig
        , Grade
        , UTCTime
        )
    -> PrivateTx
packPrivateTxQuery pk
    ( student_addr
    , sub_contents_hash
    , course_id
    , assign_contents_hash
    , assign_type
    , assign_desc
    , sub_sig
    , grade
    , time
    ) = PrivateTx
            (SignedSubmission
                (Submission
                    student_addr
                    (mkHash sub_contents_hash)
                    (Assignment
                        course_id
                        (mkHash assign_contents_hash)
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
    mkHash = either (error . toText) identity . fromByteArray

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
