-- | Scenarious for advanced test cases.

module Test.Dscp.Educator.Web.Scenarios
     ( prepareForAssignments
     , prepareForSubmissions
     , prepareAndCreateSubmission
     , prepareAndCreateSubmissions
     , prepareAndCreateTransactions
     ) where

import qualified Data.Foldable as F
import Data.List (nub)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite

-- | Puts in db all needed to put 'SignedSubmission's
-- later, tolerates repeated entities.
prepareForAssignments
    :: (MonadSQLiteDB m, MonadCatch m)
    => CoreTestEnv -> m ()
prepareForAssignments CoreTestEnv{..} = do
    let assignments = F.toList cteAssignments
        courses = map _aCourseId assignments
        owners = F.toList cteStudents
    mapM_ createStudent (ordNub owners)
    forM_ (ordNub courses) $ \course -> do
          void $ createCourse (simpleCourse course)
          forM_ (ordNub owners) $ \owner ->
              enrollStudentToCourse owner course

-- | Puts in db all needed to put 'SignedSubmission's
-- later, tolerates repeated entities.
prepareForSubmissions
    :: (MonadSQLiteDB m, MonadCatch m)
    => CoreTestEnv -> m ()
prepareForSubmissions env@CoreTestEnv{..} = do
    let assignments = F.toList cteAssignments
        owners = F.toList cteStudents
    prepareForAssignments env
    forM_ (ordNub assignments) $ \assignment -> do
          void $ createAssignment assignment
          forM_ (ordNub owners) $ \owner ->
              setStudentAssignment owner (hash assignment)

-- | Prepare all needed to put 'SignedSubmission's, and puts the first one.
prepareAndCreateSubmission
    :: (MonadSQLiteDB m, MonadCatch m)
    => CoreTestEnv -> m ()
prepareAndCreateSubmission env = do
    prepareForSubmissions env
    let sigSub = tiOne $ cteSignedSubmissions env
    void $ sqlTransaction $ submitAssignment sigSub

-- | Add all submissions from given test env to the database.
prepareAndCreateSubmissions
    :: (MonadSQLiteDB m, MonadCatch m)
    => CoreTestEnv -> m ()
prepareAndCreateSubmissions env@CoreTestEnv{..} = do
    prepareForSubmissions env
    let sigSubs = nub $ tiList cteSignedSubmissions
    forM_ sigSubs $ \sigSub ->
        sqlTransaction $ submitAssignment sigSub

-- | Add all transactions from given test env to the database.
-- Transactions will have no block assiged to them.
prepareAndCreateTransactions
    :: (MonadSQLiteDB m, MonadCatch m)
    => CoreTestEnv -> m ()
prepareAndCreateTransactions env@CoreTestEnv{..} = do
    prepareAndCreateSubmissions env
    let txs = nub $ tiList ctePrivateTxs
    forM_ txs createTransaction
