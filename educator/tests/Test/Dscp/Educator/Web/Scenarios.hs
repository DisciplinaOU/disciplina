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
import Dscp.Educator.DB

-- | Puts in db all needed to put 'SignedSubmission's
-- later, tolerates repeated entities.
prepareForAssignments
    :: (MonadIO m, MonadCatch m)
    => CoreTestEnv -> DBT 'WithinTx 'Writing m ()
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
    :: (MonadIO m, MonadCatch m)
    => CoreTestEnv -> DBT 'WithinTx 'Writing m ()
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
    :: (MonadIO m, MonadCatch m)
    => CoreTestEnv -> DBT 'WithinTx 'Writing m ()
prepareAndCreateSubmission env = do
    prepareForSubmissions env
    let sigSub = tiOne $ cteSignedSubmissions env
    void $ submitAssignment sigSub

-- | Add all submissions from given test env to the database.
prepareAndCreateSubmissions
    :: (MonadIO m, MonadCatch m)
    => CoreTestEnv -> DBT 'WithinTx 'Writing m ()
prepareAndCreateSubmissions env@CoreTestEnv{..} = do
    prepareForSubmissions env
    let sigSubs = nub $ tiList cteSignedSubmissions
    forM_ sigSubs submitAssignment

-- | Add all transactions from given test env to the database.
-- Transactions will have no block assiged to them.
prepareAndCreateTransactions
    :: (MonadIO m, MonadCatch m)
    => CoreTestEnv -> DBT 'WithinTx 'Writing m ()
prepareAndCreateTransactions env@CoreTestEnv{..} = do
    prepareAndCreateSubmissions env
    let txs = nub $ tiList ctePrivateTxs
    forM_ txs createTransaction
