-- | Scenarious for advanced test cases.

module Test.Dscp.Educator.Web.Scenarios
     ( prepareForAssignments
     , prepareForSubmissions
     , prepareAndCreateSubmission
     ) where

import qualified Data.Foldable as F

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
