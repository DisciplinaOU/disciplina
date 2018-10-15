module Test.Dscp.Educator.Web.Educator.Queries where

import Control.Lens (to)
import Data.Default (def)
import Data.List (nubBy)
import Dscp.DB.SQLite
import Dscp.Util.Test
import Test.QuickCheck (cover)
import Test.QuickCheck.Monadic (pick, pre)

import Dscp.Educator.Web.Educator
import Dscp.Educator.Web.Logic
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types
import Dscp.Util
import Test.Dscp.Educator.Mode
import Test.Dscp.Educator.Web.Instances ()
import Test.Dscp.Educator.Web.Scenarios

applyFilterOn :: Eq f => (a -> f) -> (Maybe f) -> [a] -> [a]
applyFilterOn field (Just match) = filter (\a -> field a == match)
applyFilterOn _ _                = id

spec_EducatorApiQueries :: Spec
spec_EducatorApiQueries = describe "Basic database operations" $ do
  describe "Students" $ do
    describe "getStudents" $ do
        it "Returns previously added students" $ sqlitePropertyM $ do
            students <- pickSmall listUnique
            lift $ forM_ students createStudent

            students' <- lift $ educatorGetStudents Nothing
            return $ sort students' === sort (map StudentInfo students)

        it "Filtering works" $ sqlitePropertyM $ do
            students@[student1, student2] <- pick $ vectorUnique 2
            courses@[course1, course2] <- pick $ vectorUnique 2
            lift $ do
                forM_ students createStudent
                forM_ courses $ createCourse . simpleCourse
                enrollStudentToCourse student1 course1
                enrollStudentToCourse student2 course1
                enrollStudentToCourse student1 course2

            res1 <- lift $ educatorGetStudents (Just course1)
            res2 <- lift $ educatorGetStudents (Just course2)
            return $ sort res1 === sort (map StudentInfo students)
                .&&. res2 === one (StudentInfo student1)

  describe "Courses" $ do
    describe "getCourses" $ do
        it "Returns previously added courses" $ sqlitePropertyM $ do
            coursesDetails <- nubBy ((==) `on` cdCourseId) <$> pickSmall arbitrary
            lift $ forM_ coursesDetails createCourse

            courses' <- lift $ educatorGetCourses Nothing
            let coursesBone = coursesDetails <&>
                              \(CourseDetails courseId desc subjs) ->
                                  (courseId, desc, subjs)
            let coursesBone' = courses' <&>
                              \(CourseEducatorInfo courseId desc subjs) ->
                                  (Just courseId, desc, subjs)
            return $
                cover (length coursesDetails > 1) 50 "enough courses" $
                sort coursesBone === sort coursesBone'

    describe "getCourses" $ do
        it "Filtering works" $ sqlitePropertyM $ do
            students@[student1, student2] <- pick $ vectorUnique 2
            courses@[course1, course2] <- pick $ vectorUnique 2
            lift $ do
                forM_ students createStudent
                forM_ courses $ createCourse . simpleCourse
                enrollStudentToCourse student1 course1
                enrollStudentToCourse student2 course1
                enrollStudentToCourse student1 course2

            res1 <- lift $ educatorGetCourses (Just student1)
            res2 <- lift $ educatorGetCourses (Just student2)
            return $ sort (map ciId res1) === sort courses
                .&&. map ciId res2 === one (course1)

    describe "getCourse" $ do
        it "Fails on request of non-existent course" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            let course = tiOne $ cteCourses env

            lift . throwsPrism (_AbsentError . _CourseDomain) $
                educatorGetCourse (getId course)

        it "Returns existing course properly" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            course <- lift $ createCourse . simpleCourse . tiOne . cteCourses $ env

            res <- lift $ educatorGetCourse (getId course)
            return $ res === CourseEducatorInfo
                { ciId = getId course
                , ciDesc = ""
                , ciSubjects = []
                }

    describe "getAssignments" $ do
        -- This endpoint is fully covered by tests for Student API,
        -- so just checking it at least works.

        it "Returns existing assignment properly" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            let assignment = tiOne $ cteAssignments env
            let student = tiOne $ cteStudents env

            lift $ do
                prepareForAssignments env
                void $ createAssignment assignment
                setStudentAssignment student (hash assignment)

            res <- lift $ commonGetAssignments EducatorCase def{ afStudent = Just student }
            return $ res === one AssignmentEducatorInfo
                { aiHash = hash assignment
                , aiCourseId = _aCourseId assignment
                , aiContentsHash = _aContentsHash assignment
                , aiIsFinal =
                    _aType assignment ^. assignmentTypeRaw
                , aiDesc = _aDesc assignment
                }

  describe "Submissions" $ do
    describe "getSubmission" $ do
        it "Fails on request of non-existent submission" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            let submission = tiOne $ cteSubmissions env
            let student = _sStudentId submission

            _ <- lift $ createStudent student

            lift . throwsPrism (_AbsentError . _SubmissionDomain) $
                educatorGetSubmission (getId submission)

        it "Returns existing submission properly" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            let assignment = tiOne $ cteAssignments env
                submission = tiOne $ cteSubmissions env
                signedSubmission = tiOne $ cteSignedSubmissions env

            lift $ prepareAndCreateSubmission env

            res <- lift $ educatorGetSubmission (getId submission)
            return $ res === SubmissionEducatorInfo
                { siHash = hash submission
                , siContentsHash = _sContentsHash submission
                , siAssignmentHash = hash assignment
                , siGrade = Nothing
                , siWitness = _ssWitness signedSubmission
                }

    describe "getSubmissions" $ do
        it "Student has no last submissions initially" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            lift $ prepareForAssignments env
            -- even after this ^ there should be no submissions

            submissions <- lift educatorGetAllSubmissions
            return $ submissions === []

        it "Returns existing submission properly" $
          sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv wildCoreTestParams
            let submission = tiOne $ cteSubmissions env
                signedSubmission = tiOne $ cteSignedSubmissions env

            lift $ prepareAndCreateSubmission env

            res <- lift educatorGetAllSubmissions
            return $ res === one SubmissionEducatorInfo
                { siHash = hash submission
                , siContentsHash = _sContentsHash submission
                , siAssignmentHash = _sAssignmentHash submission
                , siGrade = Nothing
                , siWitness = _ssWitness signedSubmission
                }

        it "Returns grade when present" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            let txs     = tiList $ ctePrivateTxs env
                sigSubs = tiList $ cteSignedSubmissions env

            pre (length sigSubs == length (ordNub sigSubs))

            lift $ do
                prepareForSubmissions env
                mapM_ createSignedSubmission sigSubs
                mapM_ createTransaction txs

            submissions' <- lift educatorGetAllSubmissions
            let submissionsAndGrades' =
                    map (siHash &&& fmap giGrade . siGrade) submissions'
            let submissionsAndGrades = txs <&> \tx ->
                        ( tx ^. ptSignedSubmission . ssSubmission . to hash
                        , Just (_ptGrade tx))
            return $
                sortOn fst submissionsAndGrades
                ===
                sortOn fst submissionsAndGrades'

        it "Filtering works" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
                                         { ctpAssignment = variousItems }
            courseIdF <- pick arbitrary
            assignHF <- pick arbitrary
            docTypeF <- pick arbitrary

            let student = tiOne $ cteStudents env
                sigSubs = ordNub . tiList $ cteSignedSubmissions env
                courses = map _aCourseId . tiList $ cteAssignments env

            lift $ prepareForSubmissions env
            lift $ mapM_ createSignedSubmission sigSubs

            submissions <- lift $ commonGetSubmissions EducatorCase
                def{ sfStudent = Just student, sfCourse = courseIdF
                    , sfAssignmentHash = assignHF, sfDocType = docTypeF }

            let submissions' =
                  map (\(s, _) -> educatorLiftSubmission s Nothing) $
                  applyFilterOn snd courseIdF $
                  applyFilterOn (_sAssignmentHash . _ssSubmission . fst) assignHF $
                  applyFilterOn (_sDocumentType . _ssSubmission . fst) docTypeF $
                  sigSubs `zip` cycle courses

            return $ sortOn siHash submissions === sortOn siHash submissions'


  describe "Grades" $ do
    describe "getGrades" $ pass  -- TODO [DSCP-176]

    describe "postGrade" $ pass  -- TODO [DSCP-176]


  describe "Proofs" $ do
    describe "getProofs" $ do
        it "No proofs initially" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            let student = tiOne $ cteStudents env
            lift $ prepareAndCreateSubmissions env

            proofs <- lift $ commonGetProofs def{ pfStudent = Just student }

            return $ proofs === []

        it "Returns existing proof properly" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            let student = tiOne $ cteStudents env
                ptx = tiOne $ ctePrivateTxs env

            lift $ do
                prepareAndCreateSubmissions env
                void $ createTransaction ptx
                mblock <- createPrivateBlock Nothing
                let !_ = mblock ?: error "No private block created"
                return ()

            proofs <- lift $ commonGetProofs def{ pfStudent = Just student }
            let proof = expectOne "block proofs" proofs

            return $ bpiTxs proof === [ptx]

        it "Proofs are grouped properly" $ sqlitePropertyM $ do
            env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
            let student = tiOne $ cteStudents env
                ptxs = tiList $ ctePrivateTxs env

            pre (length ptxs >= 3)
            let ptx1 : ptx2 : ptx3 : _ = ptxs

            lift $ do
                prepareAndCreateSubmissions env
                void $ createTransaction ptx1
                void $ createTransaction ptx2
                mblock1 <- createPrivateBlock Nothing
                void $ createTransaction ptx3
                mblock2 <- createPrivateBlock Nothing
                let !_ = (mblock1 >> mblock2) ?: error "No private blocks created"
                return ()

            proofs <- lift $ commonGetProofs def{ pfStudent = Just student }
            let resTxs = map bpiTxs proofs

            return $ map sort resTxs === [sort [ptx1, ptx2], [ptx3]]

        -- Content of MerkleProof is tested in "Retrieval of proven
        -- transactions" test.
