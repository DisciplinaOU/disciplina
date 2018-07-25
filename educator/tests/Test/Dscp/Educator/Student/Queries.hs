module Test.Dscp.Educator.Student.Queries where

import Data.List (nub, (!!))
import Data.Time.Clock (UTCTime (..))

import Dscp.Core.Arbitrary (genStudentSignedSubmissions)
import qualified Dscp.Core.Grade as Core
import qualified Dscp.Core.Types as Core
import Dscp.Crypto (Hash, Raw, hash)
import Dscp.DB.SQLite (MonadSQLiteDB, WithinSQLTransaction, sqlTransaction, _AssignmentDoesNotExist,
                       _SubmissionDoesNotExist)
import qualified Dscp.DB.SQLite as CoreDB
import Dscp.Educator.Txs (PrivateTx (..))
import Dscp.Educator.Web.Student (Assignment (..), Course (..), Grade (..), IsEnrolled (..),
                                  IsFinal (..), MonadStudentAPIQuery, Student, Submission (..),
                                  aDocumentType, assignmentTypeRaw, liftAssignment, liftSubmission,
                                  _BadSubmissionSignature, _DeletingGradedSubmission,
                                  _EntityAlreadyPresent, _FakeSubmissionSignature, _IsFinal,
                                  _SubmissionAlreadyExists)
import qualified Dscp.Educator.Web.Student.Logic as Logic
import qualified Dscp.Educator.Web.Student.Queries as DB
import Dscp.Util (Id (..))
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Common (sqliteProperty)
import Test.Dscp.Educator.Student.Instances ()

allStudents :: [Student]
student1, student2 :: Student
allStudents@(student1 : student2 : _) = detGen 3432 $ vectorUnique 100

allCourses :: [Id Core.Course]
courseId1, courseId2 :: Id Core.Course
allCourses@(courseId1 : courseId2 : _) = detGen 2234 $ vectorUnique 100

allAssignments :: [Core.Assignment]
assignment1, assignment2 :: Core.Assignment
allAssignments@(assignment1 : assignment2 : _) = detGen 23423 $ do
    contentsHashes <- vectorUnique 100
    return $
        zip3 [1::Int ..]
             (cycle [Core.Regular, Core.CourseFinal])
             contentsHashes
        <&> \(i, assignType, chash) ->
            Core.Assignment
                { _aCourseId = courseId1
                , _aContentsHash = chash
                , _aType = assignType
                , _aDesc = "assignment " <> show i
                }

submission1 :: Core.Submission
submission1 : _ = detGen 23423 $ do
    contentsHashes <- vectorUnique 100
    return $
        zip3 allStudents
             contentsHashes
             allAssignments
        <&> \(_sStudentId, _sContentsHash, _sAssignment) -> Core.Submission{..}

createCourseSimple :: CoreDB.DBM m => Int -> m Core.Course
createCourseSimple i =
    CoreDB.createCourse
        (allCourses !! (i - 1))
        (Just $ "course " <> pretty i)
        []

getAllSubmissions
    :: (MonadStudentAPIQuery m)
    => Student -> m [Submission]
getAllSubmissions student =
    sqlTx $ DB.getSubmissions student Nothing Nothing Nothing

-- | For advanced queries. Puts SignedSubmissions in db, tolerates repeating
-- entities.
prepareForSubmissions
    :: (MonadSQLiteDB m, Container l, Element l ~ Core.SignedSubmission)
    => l -> m ()
prepareForSubmissions (toList -> sigSubmissions) = do
    let submissions = map Core._ssSubmission sigSubmissions
        assignments = map Core._sAssignment submissions
        courses = map Core._aCourseId assignments
        owners = map Core._sStudentId submissions
    mapM_ CoreDB.createStudent (ordNub owners)
    forM_ (ordNub courses) $ \course -> do
          void $ CoreDB.createCourse course Nothing []
          forM_ (ordNub owners) $ \owner ->
              CoreDB.enrollStudentToCourse owner course
    forM_ (ordNub assignments) $ \assignment -> do
          void $ CoreDB.createAssignment assignment
          forM_ (ordNub owners) $ \owner ->
              CoreDB.setStudentAssignment owner
                                          (getId assignment)

-- | For advanced queries. Puts SignedSubmissions in db, tolerates repeating
-- entities.
prepareAndCreateSubmissions
    :: (MonadSQLiteDB m, Container l, Element l ~ Core.SignedSubmission)
    => l -> m ()
prepareAndCreateSubmissions (toList -> sigSubmissions) = do
    prepareForSubmissions sigSubmissions
    mapM_ CoreDB.submitAssignment (nub sigSubmissions)

applyFilterOn :: Eq f => (a -> f) -> (Maybe f) -> [a] -> [a]
applyFilterOn field (Just match) = filter (\a -> field a == match)
applyFilterOn _ _                = id

sqlTx :: MonadSQLiteDB m => (WithinSQLTransaction => m a) -> m a
sqlTx = sqlTransaction

someTime :: UTCTime
someTime = UTCTime (toEnum 0) 0

{- The plan is to have following tests for each endpoint (where applicable):

1. Simple negative test;
2. Positive test, returned datatype filled properly in default case;
3. Student does not see entities of other students / can't affect them;
4. Corner cases (e.g. optional fields are filled properly);
5. Filtering.

-}
spec_StudentApiQueries :: Spec
spec_StudentApiQueries = describe "Basic database operations" $ do
  describe "Courses" $ do
    describe "getCourse" $ do
        it "Student is not enrolled initially" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1

                course <- DB.getCourse student1 courseId1
                return (not $ cIsEnrolled course)

        it "Student gets enrolled when she asks to" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1
                CoreDB.enrollStudentToCourse student1 courseId1

                course <- DB.getCourse student1 courseId1
                return (cIsEnrolled course)

        it "'Course' datatype is filled correctly" $
            sqliteProperty $ \
              ( courseId
              , desc
              , delayedGen listUnique -> subjects
              ) -> do
                _ <- CoreDB.createCourse courseId desc subjects
                course <- DB.getCourse student1 courseId
                return $ course === Course
                    { cId = courseId
                    , cDesc = fromMaybe "" desc
                    , cSubjects = subjects
                    , cIsEnrolled = False
                    }

        it "Student has vision proper for him" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createStudent student2
                _ <- createCourseSimple 1
                _ <- createCourseSimple 2
                CoreDB.enrollStudentToCourse student1 courseId1

                course <- DB.getCourse student2 courseId1
                return (not $ cIsEnrolled course)

    describe "getCourses" $ do
        it "Student is not enrolled initially" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1

                courses <- sqlTx $ DB.getCourses student1 Nothing
                return (all (not . cIsEnrolled) courses)

        it "Student gets enrolled when he asks to" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1
                CoreDB.enrollStudentToCourse student1 courseId1

                courses <- sqlTx $ DB.getCourses student1 Nothing
                let Just course1 = find (\c -> cId c == courseId1) courses
                return (cIsEnrolled course1)

        it "'Course' datatype is filled correctly" $
            sqliteProperty $ \
              ( courseId
              , desc
              , delayedGen listUnique -> subjects
              ) -> do
                _ <- CoreDB.createCourse courseId desc subjects
                courses <- sqlTx $ DB.getCourses student1 Nothing
                return $ courses === one Course
                    { cId = courseId
                    , cDesc = fromMaybe "" desc
                    , cSubjects = subjects
                    , cIsEnrolled = False
                    }

        it "Student has vision proper for him" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createStudent student2
                _ <- createCourseSimple 1
                _ <- createCourseSimple 2
                mapM_ (CoreDB.enrollStudentToCourse student1)
                      [courseId1]
                mapM_ (CoreDB.enrollStudentToCourse student2)
                      [courseId1, courseId2]

                courses <- sqlTx $ DB.getCourses student1 Nothing
                               <&> sortOn cDesc
                return (map cIsEnrolled courses === [True, False])

        it "Filtering on IsEnrolled works" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1
                _ <- createCourseSimple 2
                CoreDB.enrollStudentToCourse student1 courseId2

                [notEnrolled, enrolled] <-
                    forM [False, True] $ \isEnrolled ->
                        sqlTx $
                        DB.getCourses student1 (Just $ IsEnrolled isEnrolled)

                return $
                    map cDesc enrolled === ["course 2"]
                  .&&.
                    map cDesc notEnrolled === ["course 1"]

  describe "Assignments" $ do
    describe "getAssignment" $ do
        it "Fails on request of non-existent assignment" $
            sqliteProperty $ \() ->
                throwsPrism _AssignmentDoesNotExist $ do
                    _ <- CoreDB.createStudent student1
                    sqlTx $ DB.getAssignment student1 (getId assignment1)

        it "Fails when student is not assigned to submission" $
            -- Student is required to just take his recent submission,
            -- but we have to fail on all unautharized actions.
            sqliteProperty $ \assignment -> do
                let course = Core._aCourseId assignment
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment
                throwsPrism _AssignmentDoesNotExist $
                    sqlTx $ DB.getAssignment student1 (getId assignment1)

        it "Returns existing assignment properly" $
            sqliteProperty $ \assignment -> do
                let assignmentH = getId assignment
                let course = Core._aCourseId assignment
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment
                _ <- CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.setStudentAssignment student1 assignmentH

                assignment' <- sqlTx $ DB.getAssignment student1 assignmentH
                return $ assignment' === Assignment
                    { aHash = assignmentH
                    , aCourseId = Core._aCourseId assignment
                    , aContentsHash = Core._aContentsHash assignment
                    , aIsFinal = Core._aType assignment ^. assignmentTypeRaw . _IsFinal
                    , aDesc = Core._aDesc assignment
                    , aLastSubmission = Nothing
                    }

        it "Student sees only his assignment" $
            sqliteProperty $ \
              ( delayedGen (vectorUnique 2) -> [assignment, needlessAssignment]
              ) -> do
                let course = Core._aCourseId assignment
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment
                _ <- CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.setStudentAssignment student1 (getId assignment)
                throwsPrism _AssignmentDoesNotExist $
                    sqlTx $ DB.getAssignment student1 (getId needlessAssignment)

    describe "getAssignments" $ do
        let getAssignmentsSimple student =
                sqlTx $ DB.getAssignments student Nothing Nothing Nothing

        it "Student has no last submission initially" $
            sqliteProperty $ \() -> do
                let course = Core._aCourseId assignment1
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment1
                _ <- CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.setStudentAssignment student1 (getId assignment1)
                assignments <- getAssignmentsSimple student1
                return $ all (isNothing . aLastSubmission) assignments

        it "Returns existing assignment properly and only related to student" $
            sqliteProperty $
              \(delayedGen (vectorUnique 2)
                 -> assignments@[assignment, needlessAssignment]) -> do
                let assignmentH = getId assignment
                _ <- CoreDB.createStudent student1
                forM_ (ordNub $ map Core._aCourseId assignments) $ \course -> do
                    void $ CoreDB.createCourse course Nothing []
                    void $ CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.createAssignment assignment
                _ <- CoreDB.createAssignment needlessAssignment
                _ <- CoreDB.setStudentAssignment student1 assignmentH

                res <- getAssignmentsSimple student1
                return $ res === one Assignment
                    { aHash = getId assignment
                    , aCourseId = Core._aCourseId assignment
                    , aContentsHash = Core._aContentsHash assignment
                    , aIsFinal =
                        Core._aType assignment ^. assignmentTypeRaw . _IsFinal
                    , aDesc = Core._aDesc assignment
                    , aLastSubmission = Nothing
                    }

        it "Filtering works" $
            sqliteProperty $ \
              ( delayedGen listUnique -> preAssignments
              , courseIdF
              , docTypeF
              , isFinalF
              ) -> do
                let courseIds = ordNub $ map Core._aCourseId preAssignments
                _ <- CoreDB.createStudent student1
                forM_ courseIds $ \courseId -> do
                    void $ CoreDB.createCourse courseId Nothing []
                    void $ CoreDB.enrollStudentToCourse student1 courseId
                forM_ preAssignments $ \assignment -> do
                    void $ CoreDB.createAssignment assignment
                    let assignH = getId assignment
                    void $ CoreDB.setStudentAssignment student1 assignH

                assignments <- sqlTx $
                    DB.getAssignments student1 courseIdF docTypeF isFinalF

                let assignments' =
                        applyFilterOn aCourseId courseIdF $
                        applyFilterOn aDocumentType docTypeF $
                        applyFilterOn (IsFinal . aIsFinal) isFinalF $
                        map (\a -> liftAssignment a Nothing)
                        preAssignments

                return $ sortOn aHash assignments === sortOn aHash assignments'

        it "Last submission is actually the last" $
            sqliteProperty $
              \( delayedGen
                 (genStudentSignedSubmissions (pure submission1))
                 -> (student, sigSubmissions)
               ) -> do
                prepareAndCreateSubmissions sigSubmissions

                let lastSigSubmission = last sigSubmissions
                    -- submission was changed
                let lastSubmission = Core._ssSubmission lastSigSubmission
                let assignment = Core._sAssignment lastSubmission
                assignment' <-
                    sqlTx $ DB.getAssignment student (hash assignment)
                let lastSubmission' = aLastSubmission assignment'
                return $
                    lastSubmission'
                    ===
                    Just (liftSubmission lastSubmission Nothing)

  describe "Submissions" $ do
    describe "getSubmission" $ do
        let mkSomeSubmission :: Hash Raw -> Core.Submission
            mkSomeSubmission _sContentsHash =
                Core.Submission { _sStudentId = student1
                                , _sAssignment = assignment1
                                , .. }

        it "Fails on request of non-existent submission" $
            sqliteProperty $ \(mkSomeSubmission -> submission) ->
                throwsPrism _SubmissionDoesNotExist $ do
                    let student = Core._sStudentId submission
                    _ <- CoreDB.createStudent student
                    sqlTx $ DB.getSubmission student (getId submission)

        it "Returns existing submission properly" $
            sqliteProperty $ \sigSubmission -> do
                let submission = Core._ssSubmission sigSubmission
                    assignment = Core._sAssignment submission
                    course = Core._aCourseId assignment
                    owner = Core._sStudentId submission
                _ <- CoreDB.createStudent owner
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment
                _ <- CoreDB.enrollStudentToCourse owner course
                _ <- CoreDB.setStudentAssignment owner (getId assignment)
                _ <- CoreDB.submitAssignment sigSubmission
                res <- sqlTx $
                    DB.getSubmission owner (getId submission)
                return $ res === Submission
                    { sHash = hash submission
                    , sContentsHash = Core._sContentsHash submission
                    , sAssignmentHash = hash assignment
                    , sGrade = Nothing
                    }

        it "Fails when student is not submission owner" $
            sqliteProperty $ \sigSubmission -> do
                let submission = Core._ssSubmission sigSubmission
                    user = student1
                    owner = Core._sStudentId submission
                    assignment = Core._sAssignment submission
                    course = Core._aCourseId assignment
                if user == owner
                then return $ property rejected
                else do
                    _ <- CoreDB.createStudent owner
                    _ <- CoreDB.createStudent user
                    _ <- CoreDB.createCourse course Nothing []
                    _ <- CoreDB.createAssignment assignment
                    _ <- CoreDB.enrollStudentToCourse owner course
                    _ <- CoreDB.setStudentAssignment owner (getId assignment)
                    _ <- CoreDB.submitAssignment sigSubmission
                    fmap property $ throwsPrism _SubmissionDoesNotExist $
                        sqlTx $ DB.getSubmission user (getId submission)

        it "Returns grade when present" $
            sqliteProperty $ \(sigSubmission, grade) -> do
                let submission = Core._ssSubmission sigSubmission
                    student = Core._sStudentId $ submission
                prepareAndCreateSubmissions [sigSubmission]
                _ <- CoreDB.createTransaction $
                     PrivateTx sigSubmission grade someTime

                submission' <-
                    sqlTx $ DB.getSubmission student (getId submission)
                let Just grade' = sGrade submission'
                return $ (gGrade grade', gHasProof grade') === (grade, False)

    describe "getSubmissions" $ do
        it "Student has no last submissions initially" $
            sqliteProperty $ \() -> do
                let course = Core._aCourseId assignment1
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment1
                _ <- CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.setStudentAssignment student1 (getId assignment1)
                -- even after these steps there should be no submissions

                submissions <- getAllSubmissions student1
                return $ submissions === []

        it "Returns existing submission properly and only related to student" $
            sqliteProperty $
              \(delayedGen (vectorUnique 2) -> sigSubmissions) -> do
                let submissions@[someSubmission, _] =
                        map Core._ssSubmission sigSubmissions
                    [owner1, owner2] = map Core._sStudentId submissions
                if owner1 == owner2
                then return $ property rejected
                else do
                    prepareAndCreateSubmissions sigSubmissions

                    res <- getAllSubmissions owner1
                    return $ res === one Submission
                        { sHash = hash someSubmission
                        , sContentsHash = Core._sContentsHash someSubmission
                        , sAssignmentHash =
                            hash (Core._sAssignment someSubmission)
                        , sGrade = Nothing
                        }

        it "Returns grade when present" $
            sqliteProperty $ \
              ( delayedGen (genStudentSignedSubmissions arbitrary)
                -> (student, sigSubmissions)
              , delayedGen infiniteList
                -> grades
              ) -> do
                prepareAndCreateSubmissions sigSubmissions
                let sigSubmissionsAndGrades =
                        zip (nub (toList sigSubmissions)) grades

                forM_ sigSubmissionsAndGrades $ \(sigSubmission, grade) ->
                    CoreDB.createTransaction $
                    PrivateTx sigSubmission grade someTime

                submissions' <- getAllSubmissions student
                let submissionsAndGrades' =
                        map (sHash &&& fmap gGrade . sGrade) submissions'
                let submissionsAndGrades = sigSubmissionsAndGrades
                        <&> \(sigSub, grade) ->
                            (hash $ Core._ssSubmission sigSub, Just grade)
                return $
                    sortOn fst submissionsAndGrades
                    ===
                    sortOn fst submissionsAndGrades'

        it "Filtering works" $
            sqliteProperty $ \
              ( delayedGen (genStudentSignedSubmissions arbitrary)
                -> (student, sigSubmissions)
              , courseIdF
              , assignHF
              , docTypeF
              ) -> do
                  prepareAndCreateSubmissions sigSubmissions

                  submissions <-
                      sqlTx $
                      DB.getSubmissions student courseIdF assignHF docTypeF

                  let subCourseId = Core._aCourseId . Core._sAssignment
                  let submissions' =
                        map (\s -> liftSubmission s Nothing) $
                        applyFilterOn subCourseId courseIdF $
                        applyFilterOn (hash . Core._sAssignment) assignHF $
                        applyFilterOn Core._sDocumentType docTypeF $
                        map Core._ssSubmission $
                        toList sigSubmissions

                  return $ sortOn sHash submissions == sortOn sHash submissions'

    describe "deleteSubmission" $ do
        it "Deletion of non-existing submission throws" $
            sqliteProperty $ \submission -> do
                _ <- CoreDB.createStudent student1
                throwsPrism _SubmissionDoesNotExist $
                    sqlTx $ DB.deleteSubmission student1 (hash submission)

        it "Delete works" $
            sqliteProperty $
              \( delayedGen
                 (genStudentSignedSubmissions arbitrary)
                 -> (student, sigSubmissions@(sigSubmissiontoDel :| _))
               ) -> do
                  prepareAndCreateSubmissions sigSubmissions
                  subBefore <- getAllSubmissions student

                  let submissionToDel = Core._ssSubmission sigSubmissiontoDel
                  let submissionToDelH = hash submissionToDel
                  sqlTx $ DB.deleteSubmission student submissionToDelH
                  subAfter <- getAllSubmissions student
                  let expected =
                          filter (\s -> sHash s /= submissionToDelH) subBefore

                  return $ sortOn sHash subAfter === sortOn sHash expected

        it "Can not delete graded submission" $
            sqliteProperty $ \sigSubmission -> do
                let submission = Core._ssSubmission sigSubmission
                    student = Core._sStudentId $ submission
                prepareAndCreateSubmissions [sigSubmission]
                _ <- CoreDB.createTransaction $
                     PrivateTx sigSubmission Core.gA someTime

                throwsPrism _DeletingGradedSubmission $ do
                     sqlTx $ DB.deleteSubmission student (hash submission)

        it "Can not delete other student's submission" $
            sqliteProperty $ \sigSubmission -> do
                let submission = Core._ssSubmission sigSubmission
                    student = Core._sStudentId $ submission
                    Just otherStudent = find (/= student) allStudents
                prepareAndCreateSubmissions [sigSubmission]

                throwsPrism _SubmissionDoesNotExist $
                    sqlTx $ DB.deleteSubmission otherStudent (hash submission)

    describe "makeSubmission" $ do
        it "Making same submission twice throws" $
            sqliteProperty $ \sigSubmission -> do
                prepareAndCreateSubmissions [sigSubmission]
                throwsPrism (_EntityAlreadyPresent . _SubmissionAlreadyExists) $
                    DB.makeSubmission sigSubmission

        it "Pretending to be another student is bad" $
            sqliteProperty $ \(sigSubmission, badStudent) -> do
                if Core._sStudentId (Core._ssSubmission sigSubmission) == badStudent
                then return $ property rejected
                else do
                    prepareForSubmissions [sigSubmission]
                    fmap property $ throwsPrism (_BadSubmissionSignature . _FakeSubmissionSignature) $
                        Logic.makeSubmissionVerified badStudent sigSubmission

  describe "Transactions" $ do
    describe "getProofs" $ do
         let getAllProofs student = sqlTx $ DB.getProofs student Nothing

         it "Returns nothing initially" $
             sqliteProperty $ \() -> do
                 _ <- CoreDB.createStudent student1
                 proofs <- getAllProofs student1
                 return $ proofs === []

         -- TODO: more tests
