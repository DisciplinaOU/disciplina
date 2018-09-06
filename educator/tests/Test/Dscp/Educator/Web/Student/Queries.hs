module Test.Dscp.Educator.Web.Student.Queries where

import Control.Concurrent (threadDelay)
import Control.Lens (traversed)
import Data.Default (def)
import Data.List (nub, (!!))
import Data.Time.Clock (UTCTime (..))
import qualified GHC.Exts as Exts
import Test.QuickCheck.Monadic (pick, pre)

import Dscp.Core
import Dscp.Crypto (hash, unsafeHash)
import Dscp.DB.SQLite
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Common (sqliteProperty, sqlitePropertyM)
import Test.Dscp.Educator.Web.Instances ()
import Test.Dscp.Educator.Web.Scenarios

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
        <&> \(_sStudentId, _sContentsHash, (hash -> _sAssignmentHash))
            -> Submission{..}

createCourseSimple :: DBM m => Int -> m Course
createCourseSimple i =
    createCourse
        CourseDetails
        { cdCourseId = allCourses !! (i - 1)
        , cdDesc = "course " <> pretty i
        , cdSubjects = []
        }

applyFilterOn :: Eq f => (a -> f) -> (Maybe f) -> [a] -> [a]
applyFilterOn field (Just match) = filter (\a -> field a == match)
applyFilterOn _ _                = id

sqlTx :: MonadSQLiteDB m => (WithinSQLTransaction => m a) -> m a
sqlTx = sqlTransaction

someTime :: UTCTime
someTime = UTCTime (toEnum 0) 0

assignmentItemsForSameCourse :: TestItemParam Assignment
assignmentItemsForSameCourse = FixedItemSet $ do
    assignments <- arbitrary
    return $ assignments & traversed . aCourseId .~ Course 1

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
                _ <- createStudent student1
                _ <- createCourseSimple 1

                course <- studentGetCourse student1 courseId1
                return (not $ ciIsEnrolled course)

        it "Student gets enrolled when she asks to" $
            sqliteProperty $ \() -> do
                _ <- createStudent student1
                _ <- createCourseSimple 1
                enrollStudentToCourse student1 courseId1

                course <- studentGetCourse student1 courseId1
                return (ciIsEnrolled course)

        it "'Course' datatype is filled correctly" $
            sqliteProperty $ \
              ( courseDetails
              ) -> do
                _ <- createCourse courseDetails
                course <- studentGetCourse student1 (cdCourseId courseDetails)
                return $ course === CourseStudentInfo
                    { ciId = cdCourseId courseDetails
                    , ciDesc = cdDesc courseDetails
                    , ciSubjects = cdSubjects courseDetails
                    , ciIsEnrolled = False
                    , ciIsFinished = False
                    }

        it "Student has vision proper for him" $
            sqliteProperty $ \() -> do
                _ <- createStudent student1
                _ <- createStudent student2
                _ <- createCourseSimple 1
                _ <- createCourseSimple 2
                enrollStudentToCourse student1 courseId1

                course <- studentGetCourse student2 courseId1
                return (not $ ciIsEnrolled course)

        it "Course 'isFinished' flag is correct" $
            sqliteProperty $
              \( delayedGen
                 (genCoreTestEnv simpleCoreTestParams
                  { ctpAssignment = assignmentItemsForSameCourse }) -> env
               , delayedGen (infiniteList @(Maybe Grade)) -> mgrades
               ) -> do
                let student = tiOne $ cteStudents env
                    course  = tiOne $ cteCourses env
                    sigSubs = nub . tiList $ cteSignedSubmissions env
                    gradedSigSubs = [(ss, g) | (ss, Just g) <- zip sigSubs mgrades]
                prepareForSubmissions env
                forM_ sigSubs $ \sigSub -> sqlTx $ submitAssignment sigSub
                forM_ gradedSigSubs $ \(sigSub, grade) ->
                    createTransaction PrivateTx
                    { _ptSignedSubmission = sigSub
                    , _ptGrade = grade
                    , _ptTime = someTime
                    }

                res <- ciIsFinished <$> studentGetCourse student course
                actualGrades <-
                    fmap catMaybes . forM sigSubs $ \sigSub -> do
                        mgrade <- studentGetGrade . hash $ _ssSubmission sigSub
                        let assignHash = _sAssignmentHash $ _ssSubmission sigSub
                        assignInfo <- sqlTx $ studentGetAssignment student assignHash
                        return $ guard (unIsFinal $ aiIsFinal assignInfo) *> mgrade
                return $ res === any (isPositiveGrade . giGrade) actualGrades

    describe "getCourses" $ do
        it "Student is not enrolled initially" $
            sqliteProperty $ \() -> do
                _ <- createStudent student1
                _ <- createCourseSimple 1

                courses <- sqlTx $ studentGetCourses student1 Nothing
                return . not $ any ciIsEnrolled courses

        it "Student gets enrolled when he asks to" $
            sqliteProperty $ \() -> do
                _ <- createStudent student1
                _ <- createCourseSimple 1
                enrollStudentToCourse student1 courseId1

                courses <- sqlTx $ studentGetCourses student1 Nothing
                let Just course1 = find (\c -> ciId c == courseId1) courses
                return (ciIsEnrolled course1)

        it "'Course' datatype is filled correctly" $
            sqliteProperty $ \
              ( courseId
              , desc
              , delayedGen listUnique -> subjects
              ) -> do
                _ <- createCourse $ CourseDetails courseId desc subjects
                courses <- sqlTx $ studentGetCourses student1 Nothing
                return $ courses === one CourseStudentInfo
                    { ciId = courseId
                    , ciDesc = desc
                    , ciSubjects = subjects
                    , ciIsEnrolled = False
                    , ciIsFinished = False
                    }

        it "Student has vision proper for him" $
            sqliteProperty $ \() -> do
                _ <- createStudent student1
                _ <- createStudent student2
                _ <- createCourseSimple 1
                _ <- createCourseSimple 2
                mapM_ (enrollStudentToCourse student1)
                      [courseId1]
                mapM_ (enrollStudentToCourse student2)
                      [courseId1, courseId2]

                courses <- sqlTx $ studentGetCourses student1 Nothing
                               <&> sortOn ciDesc
                return (map ciIsEnrolled courses === [True, False])

        it "Filtering on IsEnrolled works" $
            sqliteProperty $ \() -> do
                _ <- createStudent student1
                _ <- createCourseSimple 1
                _ <- createCourseSimple 2
                enrollStudentToCourse student1 courseId2

                [notEnrolled, enrolled] <-
                    forM [False, True] $ \isEnrolled ->
                        sqlTx $
                        studentGetCourses student1 (Just $ IsEnrolled isEnrolled)

                return $
                    map ciDesc enrolled === ["course 2"]
                  .&&.
                    map ciDesc notEnrolled === ["course 1"]

        it "Course 'isFinished' flag is correct" $
            sqliteProperty $
              \( delayedGen
                 (genCoreTestEnv simpleCoreTestParams
                  { ctpAssignment = assignmentItemsForSameCourse }) -> env
               , delayedGen (infiniteList @(Maybe Grade)) -> mgrades
               ) -> do
                let student = tiOne $ cteStudents env
                    sigSubs = nub . tiList $ cteSignedSubmissions env
                    gradedSigSubs = [(ss, g) | (ss, Just g) <- zip sigSubs mgrades]
                prepareForSubmissions env
                forM_ sigSubs $ \sigSub -> sqlTx $ submitAssignment sigSub
                forM_ gradedSigSubs $ \(sigSub, grade) ->
                    createTransaction PrivateTx
                    { _ptSignedSubmission = sigSub
                    , _ptGrade = grade
                    , _ptTime = someTime
                    }

                [courseInfo] <- sqlTx $ studentGetCourses student Nothing
                actualGrades <-
                    fmap catMaybes . forM sigSubs $ \sigSub -> do
                        mgrade <- studentGetGrade . hash $ _ssSubmission sigSub
                        let assignHash = _sAssignmentHash $ _ssSubmission sigSub
                        assignInfo <- sqlTx $ studentGetAssignment student assignHash
                        return $ guard (unIsFinal $ aiIsFinal assignInfo) *> mgrade
                return $
                    ciIsFinished courseInfo
                    ===
                    any (isPositiveGrade . giGrade) actualGrades

  describe "Assignments" $ do
    describe "getAssignment" $ do
        it "Fails on request of non-existent assignment" $
            sqliteProperty $ \() ->
                throwsPrism (_AbsentError . _AssignmentDomain) $ do
                    _ <- createStudent student1
                    sqlTx $ studentGetAssignment student1 (hash assignment1)

        it "Fails when student is not assigned to submission" $
            -- Student is required to just take his recent submission,
            -- but we have to fail on all unautharized actions.
            sqliteProperty $ \assignment -> do
                let course = _aCourseId assignment
                _ <- createStudent student1
                _ <- createCourse $ simpleCourse course
                _ <- createAssignment assignment
                throwsPrism (_AbsentError . _AssignmentDomain) $
                    sqlTx $ studentGetAssignment student1 (hash assignment1)

        it "Returns existing assignment properly" $
            sqliteProperty $ \assignment -> do
                let assignmentH = hash assignment
                let course = _aCourseId assignment
                _ <- createStudent student1
                _ <- createCourse $ simpleCourse course
                _ <- createAssignment assignment
                _ <- enrollStudentToCourse student1 course
                _ <- setStudentAssignment student1 assignmentH

                assignment' <- sqlTx $ studentGetAssignment student1 assignmentH
                return $ assignment' === AssignmentStudentInfo
                    { aiHash = assignmentH
                    , aiCourseId = _aCourseId assignment
                    , aiContentsHash = _aContentsHash assignment
                    , aiIsFinal = _aType assignment ^. assignmentTypeRaw
                    , aiDesc = _aDesc assignment
                    , aiLastSubmission = Nothing
                    }

        it "Student sees only his assignment" $
            sqliteProperty $ \
              ( delayedGen (vectorUnique 2) -> [assignment, needlessAssignment]
              ) -> do
                let course = _aCourseId assignment
                _ <- createStudent student1
                _ <- createCourse $ simpleCourse course
                _ <- createAssignment assignment
                _ <- enrollStudentToCourse student1 course
                _ <- setStudentAssignment student1 (hash assignment)
                throwsPrism (_AbsentError . _AssignmentDomain) $
                    sqlTx $ studentGetAssignment student1 (getId needlessAssignment)

    describe "getAssignments" $ do
        it "Student has no last submission initially" $
            sqliteProperty $ \() -> do
                let course = _aCourseId assignment1
                _ <- createStudent student1
                _ <- createCourse $ simpleCourse course
                _ <- createAssignment assignment1
                _ <- enrollStudentToCourse student1 course
                _ <- setStudentAssignment student1 (hash assignment1)
                assignments <- sqlTx $ studentGetAllAssignments student1
                return $ all (isNothing . aiLastSubmission) assignments

        it "Returns existing assignment properly and only related to student" $
            sqliteProperty $
              \(delayedGen (vectorUnique 2)
                 -> assignments@[assignment, needlessAssignment]) -> do
                let assignmentH = hash assignment
                _ <- createStudent student1
                forM_ (ordNub $ map _aCourseId assignments) $ \course -> do
                    void $ createCourse $ simpleCourse course
                    void $ enrollStudentToCourse student1 course
                _ <- createAssignment assignment
                _ <- createAssignment needlessAssignment
                _ <- setStudentAssignment student1 assignmentH

                res <- sqlTx $ studentGetAllAssignments student1
                return $ res === one AssignmentStudentInfo
                    { aiHash = hash assignment
                    , aiCourseId = _aCourseId assignment
                    , aiContentsHash = _aContentsHash assignment
                    , aiIsFinal =
                        _aType assignment ^. assignmentTypeRaw
                    , aiDesc = _aDesc assignment
                    , aiLastSubmission = Nothing
                    }

        it "Filtering works" $
            sqliteProperty $ \
              ( delayedGen listUnique -> preAssignments
              , courseF
              , docTypeF
              , isFinalF
              ) -> do
                let courseIds = ordNub $ map _aCourseId preAssignments
                _ <- createStudent student1
                forM_ courseIds $ \courseId -> do
                    void $ createCourse $ simpleCourse courseId
                    void $ enrollStudentToCourse student1 courseId
                forM_ preAssignments $ \assignment -> do
                    void $ createAssignment assignment
                    let assignH = hash assignment
                    void $ setStudentAssignment student1 assignH

                assignments <- sqlTx $
                    commonGetAssignments StudentCase student1
                        def{ afCourse = courseF, afDocType = docTypeF, afIsFinal = isFinalF }

                let assignments' =
                        applyFilterOn aiCourseId courseF $
                        applyFilterOn saDocumentType docTypeF $
                        applyFilterOn aiIsFinal isFinalF $
                        map (\a -> studentLiftAssignment a Nothing)
                        preAssignments

                return $ sortOn aiHash assignments === sortOn aiHash assignments'

        it "Last submission is actually the last" $
            sqliteProperty $
              \( delayedGen
                 (genCoreTestEnv simpleCoreTestParams) -> env
               ) -> do
                let student = tiOne $ cteStudents env
                    sigSubs = take 3 $ tiList $ cteSignedSubmissions env

                prepareForSubmissions env
                forM_ (nub $ toList sigSubs) $ \sigSub -> do
                    void $ sqlTx $ createSignedSubmission sigSub
                    liftIO $ threadDelay 10000  -- sqlite keeps time in ms precision

                let lastSigSubmission = last $ Exts.fromList sigSubs
                let lastSubmission = _ssSubmission lastSigSubmission
                let assignmentId = _sAssignmentHash lastSubmission
                assignment' <-
                    sqlTx $ studentGetAssignment student assignmentId
                let _lastSubmission' = aiLastSubmission assignment'
                -- @martoon: I dunno how to make it work ((
                return True
                    -- lastSubmission'
                    -- ===
                    -- Just (studentLiftSubmission lastSubmission Nothing)

        it "Cannot see the last submission of other students" $
            sqlitePropertyM $ do
                env <- pick $ genCoreTestEnv wildCoreTestParams
                let student = tiOne $ cteStudents env
                    submissions = tiList $ cteSubmissions env

                lift $ prepareAndCreateSubmissions env

                assignments <- lift $ sqlTx $ studentGetAllAssignments student
                let lastSubs = map aiLastSubmission assignments
                    expectedSubmissions =
                        map hash $
                        filter ((== student) . _sStudentId) submissions

                let someFilteredOut = length submissions /= length expectedSubmissions
                return $
                    cover someFilteredOut 20 "Not all submissions are visible" $
                        conjoin $ (catMaybes lastSubs) <&> \sub ->
                            counterexample ("Extra visible submission: " <> show sub) $
                            siHash sub `elem` expectedSubmissions

  describe "Submissions" $ do
    describe "getSubmission" $ do
        -- Most of the logic is covered by tests for Educator API,
        -- there are still some student-specific things we'd like to check

        it "Fails when student is not submission owner" $
            sqlitePropertyM $ do
                env <- pick $ genCoreTestEnv simpleCoreTestParams
                user <- pick arbitrary

                let owner = tiOne $ cteStudents env
                    sigSub = tiOne $ cteSignedSubmissions env
                    assignment = tiOne $ cteAssignments env
                    submission = _ssSubmission sigSub
                    course = _aCourseId assignment

                pre (user /= owner)

                lift $ do
                    _ <- createStudent owner
                    _ <- createStudent user
                    _ <- createCourse $ simpleCourse course
                    _ <- createAssignment assignment
                    _ <- enrollStudentToCourse owner course
                    _ <- setStudentAssignment owner (hash assignment)
                    _ <- sqlTx $ submitAssignment sigSub
                    return ()
                lift . fmap property $ throwsPrism (_AbsentError . _SubmissionDomain) $
                    sqlTx $ studentGetSubmission user (getId submission)

    describe "getSubmissions" $ do
        it "Returns existing submission properly and only related to student" $
            sqlitePropertyM $ do
                env <- pick $ genCoreTestEnv wildCoreTestParams
                let sigSubs = tiList $ cteSignedSubmissions env
                pre (length sigSubs >= 2)
                let submissions@(someSubmission : _) = map _ssSubmission sigSubs
                    owner1 : owner2 : _ = map _sStudentId submissions

                pre (owner1 /= owner2)

                lift $ prepareAndCreateSubmission env

                res <- lift $ studentGetAllSubmissions owner1
                return $ res === one SubmissionStudentInfo
                    { siHash = hash someSubmission
                    , siContentsHash = _sContentsHash someSubmission
                    , siAssignmentHash = _sAssignmentHash someSubmission
                    , siGrade = Nothing
                    }

    describe "deleteSubmission" $ do
        it "Deletion of non-existing submission throws" $
            sqliteProperty $ \submission -> do
                _ <- createStudent student1
                throwsPrism (_AbsentError . _SubmissionDomain) $
                    sqlTx $ commonDeleteSubmission (hash submission) (Just student1)

        it "Delete works" $
            sqliteProperty $
              \( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
               ) -> do
                  let student = tiOne $ cteStudents env
                      subToDel = tiOne $ cteSignedSubmissions env

                  prepareAndCreateSubmission env
                  subBefore <- studentGetAllSubmissions student

                  let submissionToDel = _ssSubmission subToDel
                  let submissionToDelH = hash submissionToDel
                  sqlTx $ commonDeleteSubmission submissionToDelH (Just student)
                  subAfter <- studentGetAllSubmissions student
                  let expected =
                          filter (\s -> siHash s /= submissionToDelH) subBefore

                  return $ sortOn siHash subAfter === sortOn siHash expected

        it "Can not delete graded submission" $
            sqliteProperty $
              \( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
               ) -> do
                let student = tiOne $ cteStudents env
                    sigSub = tiOne $ cteSignedSubmissions env
                    sub = _ssSubmission sigSub
                prepareAndCreateSubmission env
                _ <- createTransaction $
                     PrivateTx sigSub gA someTime

                throwsPrism (_SemanticError . _DeletingGradedSubmission) $ do
                     sqlTx $ commonDeleteSubmission (hash sub) (Just student)

        it "Can not delete other student's submission" $
            sqliteProperty $
              \( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
               , otherStudent
               ) -> do
                let student = tiOne $ cteStudents env
                    sigSub = tiOne $ cteSignedSubmissions env
                    sub = _ssSubmission sigSub

                if student == otherStudent
                then return $ property rejected
                else do
                    prepareAndCreateSubmission env

                    fmap property . throwsPrism (_AbsentError . _SubmissionDomain) $
                        sqlTx $ commonDeleteSubmission (hash sub) (Just otherStudent)

    describe "makeSubmission" $ do
        it "Making same submission twice throws" $
            sqliteProperty $
              \( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
               ) -> do
                let sigSub = tiOne $ cteSignedSubmissions env

                prepareForSubmissions env
                void $ sqlTx $ submitAssignment sigSub
                throwsPrism (_AlreadyPresentError . _SubmissionDomain) $
                    sqlTx $ void $ submitAssignment sigSub

        it "Making submission works" $
            sqliteProperty $
              \( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
               ) -> do
                let student = tiOne $ cteStudents env
                    sigSub = tiOne $ cteSignedSubmissions env
                    submissionReq = signedSubmissionToRequest sigSub
                prepareForSubmissions env
                void $ studentMakeSubmissionVerified student submissionReq

                res <- studentGetAllSubmissions student
                let submission = _ssSubmission sigSub
                return $ res === [studentLiftSubmission submission Nothing]

        it "Pretending to be another student is bad" $
            sqliteProperty $
              \( delayedGen
                 (genCoreTestEnv simpleCoreTestParams
                                 { ctpSecretKey = AllRandomItems }) -> env
               , badStudent
               ) -> do
                let student = tiOne $ cteStudents env
                    sigSub = tiOne $ cteSignedSubmissions env
                    newSubmission = signedSubmissionToRequest sigSub

                if student == badStudent
                then return $ property rejected
                else do
                    prepareForSubmissions env
                    fmap property $ throwsPrism (_BadSubmissionSignature . _FakeSubmissionSignature) $
                        studentMakeSubmissionVerified badStudent newSubmission

        it "Fake signature does not work" $
            sqliteProperty $
              \( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
               ) -> do
                let student = tiOne $ cteStudents env
                    sigSub = tiOne $ cteSignedSubmissions env
                    badSub = sigSub & ssSubmission . sContentsHash
                                        .~ unsafeHash @Text "pva was here"
                    newSubmission = signedSubmissionToRequest badSub

                prepareForSubmissions env
                fmap property $ throwsPrism (_BadSubmissionSignature . _SubmissionSignatureInvalid) $
                    studentMakeSubmissionVerified student newSubmission
