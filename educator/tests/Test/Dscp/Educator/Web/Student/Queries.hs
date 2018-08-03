module Test.Dscp.Educator.Web.Student.Queries where

import Data.List (nub, (!!))
import Data.Time.Clock (UTCTime (..))

import Dscp.Core
import Dscp.Core.Arbitrary (genStudentSignedSubmissions)
import Dscp.Crypto (Hash, Raw, hash)
import Dscp.DB.SQLite (MonadSQLiteDB, WithinSQLTransaction, sqlTransaction, _AssignmentDoesNotExist,
                       _SubmissionDoesNotExist)
import qualified Dscp.DB.SQLite as CoreDB
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
import Dscp.Util (Id (..))
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Common (sqliteProperty)
import Test.Dscp.Educator.Web.Instances ()

allStudents :: [Student]
student1, student2 :: Student
allStudents@(student1 : student2 : _) = detGen 3432 $ vectorUnique 100

allCourses :: [Id Course]
courseId1, courseId2 :: Id Course
allCourses@(courseId1 : courseId2 : _) = detGen 2234 $ vectorUnique 100

allAssignments :: [Assignment]
assignment1, assignment2 :: Assignment
allAssignments@(assignment1 : assignment2 : _) = detGen 23423 $ do
    contentsHashes <- vectorUnique 100
    return $
        zip3 [1::Int ..]
             (cycle [Regular, CourseFinal])
             contentsHashes
        <&> \(i, assignType, chash) ->
            Assignment
                { _aCourseId = courseId1
                , _aContentsHash = chash
                , _aType = assignType
                , _aDesc = "assignment " <> show i
                }

submission1 :: Submission
submission1 : _ = detGen 23423 $ do
    contentsHashes <- vectorUnique 100
    return $
        zip3 allStudents
             contentsHashes
             allAssignments
        <&> \(_sStudentId, _sContentsHash, _sAssignment) -> Submission{..}

createCourseSimple :: CoreDB.DBM m => Int -> m Course
createCourseSimple i =
    CoreDB.createCourse
        (allCourses !! (i - 1))
        (Just $ "course " <> pretty i)
        []

getAllSubmissions
    :: (MonadStudentAPIQuery m)
    => Student -> m [SubmissionInfo]
getAllSubmissions student =
    sqlTx $ studentGetSubmissions student Nothing Nothing Nothing

-- | For advanced queries. Puts SignedSubmissions in db, tolerates repeating
-- entities.
prepareForSubmissions
    :: (MonadSQLiteDB m, Container l, Element l ~ SignedSubmission)
    => l -> m ()
prepareForSubmissions (toList -> sigSubmissions) = do
    let submissions = map _ssSubmission sigSubmissions
        assignments = map _sAssignment submissions
        courses = map _aCourseId assignments
        owners = map _sStudentId submissions
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
    :: (MonadSQLiteDB m, Container l, Element l ~ SignedSubmission)
    => l -> m ()
prepareAndCreateSubmissions (toList -> sigSubmissions) = do
    prepareForSubmissions sigSubmissions
    sqlTx $ mapM_ CoreDB.submitAssignment (nub sigSubmissions)

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

                course <- studentGetCourse student1 courseId1
                return (not $ ciIsEnrolled course)

        it "Student gets enrolled when she asks to" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1
                CoreDB.enrollStudentToCourse student1 courseId1

                course <- studentGetCourse student1 courseId1
                return (ciIsEnrolled course)

        it "'Course' datatype is filled correctly" $
            sqliteProperty $ \
              ( courseId
              , desc
              , delayedGen listUnique -> subjects
              ) -> do
                _ <- CoreDB.createCourse courseId desc subjects
                course <- studentGetCourse student1 courseId
                return $ course === CourseInfo
                    { ciId = courseId
                    , ciDesc = fromMaybe "" desc
                    , ciSubjects = subjects
                    , ciIsEnrolled = False
                    }

        it "Student has vision proper for him" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createStudent student2
                _ <- createCourseSimple 1
                _ <- createCourseSimple 2
                CoreDB.enrollStudentToCourse student1 courseId1

                course <- studentGetCourse student2 courseId1
                return (not $ ciIsEnrolled course)

    describe "getCourses" $ do
        it "Student is not enrolled initially" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1

                courses <- sqlTx $ studentGetCourses student1 Nothing
                return (all (not . ciIsEnrolled) courses)

        it "Student gets enrolled when he asks to" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1
                CoreDB.enrollStudentToCourse student1 courseId1

                courses <- sqlTx $ studentGetCourses student1 Nothing
                let Just course1 = find (\c -> ciId c == courseId1) courses
                return (ciIsEnrolled course1)

        it "'Course' datatype is filled correctly" $
            sqliteProperty $ \
              ( courseId
              , desc
              , delayedGen listUnique -> subjects
              ) -> do
                _ <- CoreDB.createCourse courseId desc subjects
                courses <- sqlTx $ studentGetCourses student1 Nothing
                return $ courses === one CourseInfo
                    { ciId = courseId
                    , ciDesc = fromMaybe "" desc
                    , ciSubjects = subjects
                    , ciIsEnrolled = False
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

                courses <- sqlTx $ studentGetCourses student1 Nothing
                               <&> sortOn ciDesc
                return (map ciIsEnrolled courses === [True, False])

        it "Filtering on IsEnrolled works" $
            sqliteProperty $ \() -> do
                _ <- CoreDB.createStudent student1
                _ <- createCourseSimple 1
                _ <- createCourseSimple 2
                CoreDB.enrollStudentToCourse student1 courseId2

                [notEnrolled, enrolled] <-
                    forM [False, True] $ \isEnrolled ->
                        sqlTx $
                        studentGetCourses student1 (Just $ IsEnrolled isEnrolled)

                return $
                    map ciDesc enrolled === ["course 2"]
                  .&&.
                    map ciDesc notEnrolled === ["course 1"]

  describe "Assignments" $ do
    describe "getAssignment" $ do
        it "Fails on request of non-existent assignment" $
            sqliteProperty $ \() ->
                throwsPrism _AssignmentDoesNotExist $ do
                    _ <- CoreDB.createStudent student1
                    sqlTx $ studentGetAssignment student1 (getId assignment1)

        it "Fails when student is not assigned to submission" $
            -- Student is required to just take his recent submission,
            -- but we have to fail on all unautharized actions.
            sqliteProperty $ \assignment -> do
                let course = _aCourseId assignment
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment
                throwsPrism _AssignmentDoesNotExist $
                    sqlTx $ studentGetAssignment student1 (getId assignment1)

        it "Returns existing assignment properly" $
            sqliteProperty $ \assignment -> do
                let assignmentH = getId assignment
                let course = _aCourseId assignment
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment
                _ <- CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.setStudentAssignment student1 assignmentH

                assignment' <- sqlTx $ studentGetAssignment student1 assignmentH
                return $ assignment' === AssignmentInfo
                    { aiHash = assignmentH
                    , aiCourseId = _aCourseId assignment
                    , aiContentsHash = _aContentsHash assignment
                    , aiIsFinal = _aType assignment ^. assignmentTypeRaw . _IsFinal
                    , aiDesc = _aDesc assignment
                    , aiLastSubmission = Nothing
                    }

        it "Student sees only his assignment" $
            sqliteProperty $ \
              ( delayedGen (vectorUnique 2) -> [assignment, needlessAssignment]
              ) -> do
                let course = _aCourseId assignment
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment
                _ <- CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.setStudentAssignment student1 (getId assignment)
                throwsPrism _AssignmentDoesNotExist $
                    sqlTx $ studentGetAssignment student1 (getId needlessAssignment)

    describe "getAssignments" $ do
        let getAssignmentsSimple student =
                sqlTx $ studentGetAssignments student Nothing Nothing Nothing

        it "Student has no last submission initially" $
            sqliteProperty $ \() -> do
                let course = _aCourseId assignment1
                _ <- CoreDB.createStudent student1
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment1
                _ <- CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.setStudentAssignment student1 (getId assignment1)
                assignments <- getAssignmentsSimple student1
                return $ all (isNothing . aiLastSubmission) assignments

        it "Returns existing assignment properly and only related to student" $
            sqliteProperty $
              \(delayedGen (vectorUnique 2)
                 -> assignments@[assignment, needlessAssignment]) -> do
                let assignmentH = getId assignment
                _ <- CoreDB.createStudent student1
                forM_ (ordNub $ map _aCourseId assignments) $ \course -> do
                    void $ CoreDB.createCourse course Nothing []
                    void $ CoreDB.enrollStudentToCourse student1 course
                _ <- CoreDB.createAssignment assignment
                _ <- CoreDB.createAssignment needlessAssignment
                _ <- CoreDB.setStudentAssignment student1 assignmentH

                res <- getAssignmentsSimple student1
                return $ res === one AssignmentInfo
                    { aiHash = getId assignment
                    , aiCourseId = _aCourseId assignment
                    , aiContentsHash = _aContentsHash assignment
                    , aiIsFinal =
                        _aType assignment ^. assignmentTypeRaw . _IsFinal
                    , aiDesc = _aDesc assignment
                    , aiLastSubmission = Nothing
                    }

        it "Filtering works" $
            sqliteProperty $ \
              ( delayedGen listUnique -> preAssignments
              , courseIdF
              , docTypeF
              , isFinalF
              ) -> do
                let courseIds = ordNub $ map _aCourseId preAssignments
                _ <- CoreDB.createStudent student1
                forM_ courseIds $ \courseId -> do
                    void $ CoreDB.createCourse courseId Nothing []
                    void $ CoreDB.enrollStudentToCourse student1 courseId
                forM_ preAssignments $ \assignment -> do
                    void $ CoreDB.createAssignment assignment
                    let assignH = getId assignment
                    void $ CoreDB.setStudentAssignment student1 assignH

                assignments <- sqlTx $
                    studentGetAssignments student1 courseIdF docTypeF isFinalF

                let assignments' =
                        applyFilterOn aiCourseId courseIdF $
                        applyFilterOn aiDocumentType docTypeF $
                        applyFilterOn (IsFinal . aiIsFinal) isFinalF $
                        map (\a -> liftAssignment a Nothing)
                        preAssignments

                return $ sortOn aiHash assignments === sortOn aiHash assignments'

        it "Last submission is actually the last" $
            sqliteProperty $
              \( delayedGen
                 (genStudentSignedSubmissions arbitrary (pure submission1))
                 -> (student, sigSubmissions)
               ) -> do
                prepareAndCreateSubmissions sigSubmissions

                let lastSigSubmission = last sigSubmissions
                    -- submission was changed
                let lastSubmission = _ssSubmission lastSigSubmission
                let assignment = _sAssignment lastSubmission
                assignment' <-
                    sqlTx $ studentGetAssignment student (hash assignment)
                let lastSubmission' = aiLastSubmission assignment'
                return $
                    lastSubmission'
                    ===
                    Just (liftSubmission lastSubmission Nothing)

  describe "Submissions" $ do
    describe "getSubmission" $ do
        let mkSomeSubmission :: Hash Raw -> Submission
            mkSomeSubmission _sContentsHash =
                Submission { _sStudentId = student1
                                , _sAssignment = assignment1
                                , .. }

        it "Fails on request of non-existent submission" $
            sqliteProperty $ \(mkSomeSubmission -> submission) ->
                throwsPrism _SubmissionDoesNotExist $ do
                    let student = _sStudentId submission
                    _ <- CoreDB.createStudent student
                    sqlTx $ studentGetSubmission student (getId submission)

        it "Returns existing submission properly" $
            sqliteProperty $ \sigSubmission -> do
                let submission = _ssSubmission sigSubmission
                    assignment = _sAssignment submission
                    course = _aCourseId assignment
                    owner = _sStudentId submission
                _ <- CoreDB.createStudent owner
                _ <- CoreDB.createCourse course Nothing []
                _ <- CoreDB.createAssignment assignment
                _ <- CoreDB.enrollStudentToCourse owner course
                _ <- CoreDB.setStudentAssignment owner (getId assignment)
                _ <- sqlTx $ CoreDB.submitAssignment sigSubmission
                res <- sqlTx $
                    studentGetSubmission owner (getId submission)
                return $ res === SubmissionInfo
                    { siHash = hash submission
                    , siContentsHash = _sContentsHash submission
                    , siAssignmentHash = hash assignment
                    , siGrade = Nothing
                    }

        it "Fails when student is not submission owner" $
            sqliteProperty $ \sigSubmission -> do
                let submission = _ssSubmission sigSubmission
                    user = student1
                    owner = _sStudentId submission
                    assignment = _sAssignment submission
                    course = _aCourseId assignment
                if user == owner
                then return $ property rejected
                else do
                    _ <- CoreDB.createStudent owner
                    _ <- CoreDB.createStudent user
                    _ <- CoreDB.createCourse course Nothing []
                    _ <- CoreDB.createAssignment assignment
                    _ <- CoreDB.enrollStudentToCourse owner course
                    _ <- CoreDB.setStudentAssignment owner (getId assignment)
                    _ <- sqlTx $ CoreDB.submitAssignment sigSubmission
                    fmap property $ throwsPrism _SubmissionDoesNotExist $
                        sqlTx $ studentGetSubmission user (getId submission)

        it "Returns grade when present" $
            sqliteProperty $ \(sigSubmission, grade) -> do
                let submission = _ssSubmission sigSubmission
                    student = _sStudentId $ submission
                prepareAndCreateSubmissions [sigSubmission]
                _ <- CoreDB.createTransaction $
                     PrivateTx sigSubmission grade someTime

                submission' <-
                    sqlTx $ studentGetSubmission student (getId submission)
                let Just grade' = siGrade submission'
                return $
                    (giGrade grade', giHasProof grade', giSubmissionHash grade')
                    ===
                    (grade, False, hash submission)

    describe "getSubmissions" $ do
        it "Student has no last submissions initially" $
            sqliteProperty $ \() -> do
                let course = _aCourseId assignment1
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
                        map _ssSubmission sigSubmissions
                    [owner1, owner2] = map _sStudentId submissions
                if owner1 == owner2
                then return $ property rejected
                else do
                    prepareAndCreateSubmissions sigSubmissions

                    res <- getAllSubmissions owner1
                    return $ res === one SubmissionInfo
                        { siHash = hash someSubmission
                        , siContentsHash = _sContentsHash someSubmission
                        , siAssignmentHash =
                            hash (_sAssignment someSubmission)
                        , siGrade = Nothing
                        }

        it "Returns grade when present" $
            sqliteProperty $ \
              ( delayedGen (genStudentSignedSubmissions arbitrary arbitrary)
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
                        map (siHash &&& fmap giGrade . siGrade) submissions'
                let submissionsAndGrades = sigSubmissionsAndGrades
                        <&> \(sigSub, grade) ->
                            (hash $ _ssSubmission sigSub, Just grade)
                return $
                    sortOn fst submissionsAndGrades
                    ===
                    sortOn fst submissionsAndGrades'

        it "Filtering works" $
            sqliteProperty $ \
              ( delayedGen (genStudentSignedSubmissions arbitrary arbitrary)
                -> (student, sigSubmissions)
              , courseIdF
              , assignHF
              , docTypeF
              ) -> do
                  prepareAndCreateSubmissions sigSubmissions

                  submissions <-
                      sqlTx $
                      studentGetSubmissions student courseIdF assignHF docTypeF

                  let subCourseId = _aCourseId . _sAssignment
                  let submissions' =
                        map (\s -> liftSubmission s Nothing) $
                        applyFilterOn subCourseId courseIdF $
                        applyFilterOn (hash . _sAssignment) assignHF $
                        applyFilterOn _sDocumentType docTypeF $
                        map _ssSubmission $
                        toList sigSubmissions

                  return $ sortOn siHash submissions == sortOn siHash submissions'

    describe "deleteSubmission" $ do
        it "Deletion of non-existing submission throws" $
            sqliteProperty $ \submission -> do
                _ <- CoreDB.createStudent student1
                throwsPrism _SubmissionDoesNotExist $
                    sqlTx $ studentDeleteSubmission student1 (hash submission)

        it "Delete works" $
            sqliteProperty $
              \( delayedGen
                 (genStudentSignedSubmissions arbitrary arbitrary)
                 -> (student, sigSubmissions@(sigSubmissiontoDel :| _))
               ) -> do
                  prepareAndCreateSubmissions sigSubmissions
                  subBefore <- getAllSubmissions student

                  let submissionToDel = _ssSubmission sigSubmissiontoDel
                  let submissionToDelH = hash submissionToDel
                  sqlTx $ studentDeleteSubmission student submissionToDelH
                  subAfter <- getAllSubmissions student
                  let expected =
                          filter (\s -> siHash s /= submissionToDelH) subBefore

                  return $ sortOn siHash subAfter === sortOn siHash expected

        it "Can not delete graded submission" $
            sqliteProperty $ \sigSubmission -> do
                let submission = _ssSubmission sigSubmission
                    student = _sStudentId $ submission
                prepareAndCreateSubmissions [sigSubmission]
                _ <- CoreDB.createTransaction $
                     PrivateTx sigSubmission gA someTime

                throwsPrism _DeletingGradedSubmission $ do
                     sqlTx $ studentDeleteSubmission student (hash submission)

        it "Can not delete other student's submission" $
            sqliteProperty $ \sigSubmission -> do
                let submission = _ssSubmission sigSubmission
                    student = _sStudentId $ submission
                    Just otherStudent = find (/= student) allStudents
                prepareAndCreateSubmissions [sigSubmission]

                throwsPrism _SubmissionDoesNotExist $
                    sqlTx $ studentDeleteSubmission otherStudent (hash submission)

    describe "makeSubmission" $ do
        it "Making same submission twice throws" $
            sqliteProperty $ \sigSubmission -> do
                prepareAndCreateSubmissions [sigSubmission]
                throwsPrism (_EntityAlreadyPresent . _SubmissionAlreadyExists) $
                    sqlTx $ studentMakeSubmission sigSubmission

        it "Making submission works" $
            sqliteProperty $ \sigSubmission -> do
                let student = _sStudentId (_ssSubmission sigSubmission)
                let submissionReq = signedSubmissionToRequest sigSubmission
                prepareForSubmissions [sigSubmission]
                void $ studentMakeSubmissionVerified student submissionReq

                res <- getAllSubmissions student
                let submission = _ssSubmission sigSubmission
                return $ res === [liftSubmission submission Nothing]

        it "Pretending to be another student is bad" $
            sqliteProperty $ \(sigSubmission, badStudent) -> do
                if _sStudentId (_ssSubmission sigSubmission) == badStudent
                then return $ property rejected
                else do
                    let newSubmission = signedSubmissionToRequest sigSubmission
                    prepareForSubmissions [sigSubmission]
                    fmap property $ throwsPrism (_BadSubmissionSignature . _FakeSubmissionSignature) $
                        studentMakeSubmissionVerified badStudent newSubmission

  describe "Transactions" $ do
    describe "getProofs" $ do
         let getAllProofs student = sqlTx $ studentGetProofs student Nothing

         it "Returns nothing initially" $
             sqliteProperty $ \() -> do
                 _ <- CoreDB.createStudent student1
                 proofs <- getAllProofs student1
                 return $ proofs === []

         -- TODO: more tests
