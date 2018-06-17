
module Disciplina.DB.DSL.Interpret.Sqlite3 where

import Universum

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans

import qualified Data.Set as Set (Set, singleton, member)
import Database.SQLite.Simple

import Disciplina.DB.DSL.Class

newtype Sqlite3T m a = Sqlite3T { getSqlite3T :: ReaderT Connection m a }
    deriving (MonadTrans)

runSqlite3T :: (MonadIO m, MonadCatch m) => Sqlite3T m a -> String -> m a
runSqlite3T connectionString action = do
    connection <- liftIO $ open connectionString
    res <- getSqlite3T action
        `runReaderT` connection
        `finally`    close connection
    return res

instance (MonadIO m, MonadCatch m) => MonadSearchTxObj (Sqlite3T m) where
    runTxQuery (SELECTTx WHERE (TxIdEq pid)) =
        Sqlite3T $ reader $ \connection ->
            liftIO $ withTransaction connection $ do
                Just <$> query connection transByPID (Only pid)

              `catch` \(e :: SomeException) -> do
                return Nothing

    runTxQuery (SELECTObj WHERE (ObjHashEq hash)) =
        return Nothing

    runTxsQuery (SELECTTxs WHERE txFilter) = collect txFilter
      where
        query
            =  "select Trasactions.* "
            ++ "where "
            ++ wherePart txFilter
            ++ "from Trasactions "
            ++ possibleJoins

        wherePart = \case
            TxHasSubjectId subId ->
                "Subjects.id == ?"

        possibleJoins
            | Subject `Set.member` tables
                =   "left join Submissions on "
                ++      "Submissions.hash = Transaction.submission_hash "

                ++  "left join Assignments on "
                ++      "Assignments.submission_hash = Submissions.hash "

                ++  "left join Courses on "
                ++      "Assignments.course_id = Courses.id "

                ++  "left join Subjects on "
                ++      "Courses.id = Subjects.course_id "

            | otherwise =
                    "from Transaction"

        tables = requiredTables txFilter

data RequiredTables
    = Subject

requiredTables :: TxsFilterExpr -> Set.Set RequiredTables
requiredTables = \case
    TxHasSubjectId             _ -> Set.singleton Subject
    TxHasDescendantOfSubjectId _ -> Set.singleton Subject
    (:||) a b                    -> requiredTables a <> requiredTables b
    (:&)  a b                    -> requiredTables a <> requiredTables b
    _other                       -> Set.empty
