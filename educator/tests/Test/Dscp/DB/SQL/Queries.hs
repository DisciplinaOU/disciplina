{-# LANGUAGE ViewPatterns #-}

module Test.Dscp.DB.SQL.Queries where

import Prelude

import Control.Lens (mapped)
-- import Data.Default (Default (..))
import Test.QuickCheck.Monadic (pre)

import Dscp.Core.Arbitrary
-- import qualified Dscp.Crypto.MerkleTree as MerkleTree
import Dscp.Educator.DB as DB
import Dscp.Util

import Test.Dscp.DB.SQL.Mode
import Test.Dscp.Educator.Mode

spec_Instances :: Spec
spec_Instances = specWithTempPostgresServer $ do
    describe "Basic database operations" $ do
        describe "Courses" $ do
            it "Course does not exist before it is created" $
                sqlProperty $ \courseId -> do
                    isThere <- DB.existsCourse courseId
                    return (not isThere)

            it "Course does exist after it is created" $
                sqlProperty $ \courseId -> do
                    _       <- DB.createCourse (simpleCourse courseId)
                    isThere <- DB.existsCourse courseId

                    return isThere

            it "Can create unique courses relying on autoincrement" $
                sqlProperty $ \n -> do
                    ids <- replicateM n $ DB.createCourse nullCourse
                    return $ allUniqueOrd ids

        describe "Students" $ do
            it "Student does not exist before she is created" $
                sqlProperty $ \student -> do
                    isThere <- DB.existsStudent student
                    return (not isThere)

            it "Student does exist after she is created" $
                sqlProperty $ \student -> do
                    _       <- DB.createStudent student
                    isThere <- DB.existsStudent student

                    return isThere

        describe "Assignments" $ do
            it "Assignment is created and retrieved by hash" $
                sqlProperty $ \assignment -> do

                    _              <- DB.createCourse    (simpleCourse $ assignment^.aCourseId)
                    assignmentHash <- DB.createAssignment assignment
                    assignment'    <- DB.getAssignment    assignmentHash

                    return (assignment' === Just assignment)

            it "Assignment is not created if course does not exist" $
                sqlProperty $ \(assignment) -> do
                    throws @DomainError $ do
                        _ <- DB.createAssignment assignment
                        return ()

        describe "Submissions" $ do
            it "Submission is not created unless Assignment exist" $
                sqlProperty $ \submission -> do
                    throws @DomainError $ do
                        _ <- DB.createSignedSubmission submission
                        return ()

            it "Submission is not created unless Student exist" $
                sqlProperty $ \
                    ( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
                    ) -> do
                    let assignment    = tiOne $ cteAssignments env
                        sigSubmission = tiOne $ cteSignedSubmissions env

                    let course     = assignment   ^.aCourseId

                    _ <- DB.createCourse           (simpleCourse course)
                    _ <- DB.createAssignment       assignment

                    throws @DomainError $ do
                        _ <- DB.createSignedSubmission sigSubmission
                        return ()

            it "Submission is not created unless StudentAssignment exist" $
                sqlProperty $ \
                    ( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
                    ) -> do
                    let assignment    = tiOne $ cteAssignments env
                        sigSubmission = tiOne $ cteSignedSubmissions env

                    throws @DomainError $ do
                        let submission = sigSubmission^.ssSubmission
                            course     = assignment^.aCourseId
                            student    = submission^.sStudentId

                        _ <- DB.createCourse           (simpleCourse course)
                        _ <- DB.createStudent          student
                        _ <- DB.createAssignment       assignment
                        _ <- DB.createSignedSubmission sigSubmission

                        return ()

        describe "Transactions" $ do
            it "Transaction is created if all deps exist" $
                sqlProperty $ \
                    ( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
                    ) -> do
                    let assignment    = tiOne $ cteAssignments env
                        trans         = tiOne $ ctePrivateTxs env

                    let sigSubmission = trans        ^.ptSignedSubmission
                        submission    = sigSubmission^.ssSubmission
                        course        = assignment   ^.aCourseId
                        student       = submission   ^.sStudentId

                    courseId  <- DB.createCourse           (simpleCourse course)
                    studentId <- DB.createStudent          student
                    _         <- DB.enrollStudentToCourse  studentId courseId
                    aHash     <- DB.createAssignment       assignment
                    _         <- DB.setStudentAssignment   student aHash
                    _         <- DB.createSignedSubmission sigSubmission
                    transHash <- DB.createTransaction      trans

                    trans'    <- DB.getTransaction transHash

                    return (trans' == Just trans && (getId <$> trans') == (getId <$> Just trans))

    describe "Concrete operations from domain" $ do
        it "getStudentCourses/enrollStudentToCourse" $ do
            sqlProperty $ \
                (student,
                 delayedGen (vectorUnique 3)
                    -> [course1, course2, course3]
                ) -> do
                let courses = [course1, course2, course3]

                studentId <- DB.createStudent student
                courseIds <- DB.getStudentCourses studentId

                null courseIds `assertThat`
                    "Student should be enrolled to no courses initially"

                for_ courses $ \course -> do
                    courseId <- DB.createCourse (simpleCourse course)
                    DB.enrollStudentToCourse studentId courseId

                courseIds' <- DB.getStudentCourses student

                return (sort (map getId courses) == sort courseIds')

        it "getStudentAssignments" $ do
            sqlProperty $ \
                ( student
                , (delayedGen (vectorUnique 2)
                    -> [course1, course2])
                , (delayedGen (vectorUniqueOf 4 $ listOf1 @Assignment arbitrary)
                    -> [toHerCourse1, toHerCourse2,
                        notToHerCourse1, notToHerCourse2])
                ) -> do

                    studentId <- DB.createStudent student

                    courseId1 <- DB.createCourse $ simpleCourse course1
                    courseId2 <- DB.createCourse $ simpleCourse course2

                    DB.enrollStudentToCourse studentId courseId1
                    DB.enrollStudentToCourse studentId courseId2

                    let toHer = (toHerCourse1 & mapped.aCourseId .~ courseId1)
                             <> (toHerCourse2 & mapped.aCourseId .~ courseId2)

                    let notToHer = (notToHerCourse1 & mapped.aCourseId .~ courseId1)
                                <> (notToHerCourse2 & mapped.aCourseId .~ courseId2)

                    -- TODO: use PropertyM in base to write proper preconditions?
                    if not $ allUniqueOrd @(Id Assignment) $
                         map getId $ toHer <> notToHer
                    then return $ property rejected
                    else do
                        --_ <- error "stahp!"
                        for_ toHer $ \assignment -> do
                            assignmentId <- DB.createAssignment assignment
                            DB.setStudentAssignment studentId assignmentId

                        for_ notToHer $ \assignment -> do
                            _ <- DB.createAssignment assignment
                            return ()

                        --_ <- error "stahp!"
                        assignments1 <-
                            DB.getStudentAssignments studentId courseId1
                        assignments2 <-
                            DB.getStudentAssignments studentId courseId2

                        let equal = (==) `on` sortBy (comparing getId)

                        return $ property $
                            (assignments1 <> assignments2) `equal` toHer

        it "submitAssignment" $
            sqlProperty $ \
                ( delayedGen (genCoreTestEnv simpleCoreTestParams) -> env
                ) -> do
                let assignment    = tiOne $ cteAssignments env
                    sigSubmission = tiOne $ cteSignedSubmissions env

                let submission = sigSubmission^.ssSubmission
                    course     = assignment   ^.aCourseId
                    student    = submission   ^.sStudentId

                courseId  <- DB.createCourse           (simpleCourse course)
                studentId <- DB.createStudent          student
                _         <- DB.enrollStudentToCourse  studentId courseId
                aHash     <- DB.createAssignment       assignment
                _         <- DB.setStudentAssignment   student aHash
                subHash   <- DB.submitAssignment       sigSubmission

                sub'      <- DB.getSignedSubmission    subHash

                return (sub' == Just sigSubmission)

        it "getGradesForCourseAssignments" $
            sqlPropertyM $ do
                env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
                course2 <- pick arbitrary

                let assignment    = tiOne $ cteAssignments env
                    trans         = tiOne $ ctePrivateTxs env

                let sigSubmission = trans        ^.ptSignedSubmission
                    submission    = sigSubmission^.ssSubmission
                    course        = assignment   ^.aCourseId
                    student       = submission   ^.sStudentId

                    sigSubmission2 = sigSubmission & ssSubmission
                                                   . sAssignmentHash    .~ getId assignment2
                    assignment2    = assignment    & aCourseId          .~ getId course2
                    trans2         = trans         & ptSignedSubmission .~ sigSubmission2

                pre (assignment^.idOf /= assignment2^.idOf)

                lift $ do
                    _studentId <- DB.createStudent          student

                    courseId   <- DB.createCourse           (simpleCourse course)
                    courseId2  <- DB.createCourse           (simpleCourse course2)

                    _          <- DB.enrollStudentToCourse  student courseId
                    _          <- DB.enrollStudentToCourse  student courseId2

                    aHash      <- DB.createAssignment       assignment
                    aHash2     <- DB.createAssignment       assignment2

                    _          <- DB.setStudentAssignment   student aHash
                    _          <- DB.setStudentAssignment   student aHash2

                    _          <- DB.createSignedSubmission sigSubmission
                    _          <- DB.createSignedSubmission sigSubmission2

                    _          <- DB.createTransaction      trans
                    _          <- DB.createTransaction      trans2

                    transs2    <- DB.getGradesForCourseAssignments student courseId2
                    transs1    <- DB.getGradesForCourseAssignments student courseId

                    return (transs2 === [trans2] .&&. transs1 === [trans])

    -- describe "Retrieval of proven transactions" $ do
    --     it "getProvenStudentTransactions" $
    --         sqlProperty $ \
    --             ( delayedGen (genCoreTestEnv simpleCoreTestParams
    --                           `suchThat` ((>= 3) . tiNum . ctePrivateTxs)
    --                          ) -> env
    --             ) -> do
    --                 let student      = tiOne $ cteStudents env
    --                     assignment   = tiOne $ cteAssignments env
    --                     transactions = take 3 $ tiList $ ctePrivateTxs env

    --                 studentId <- DB.createStudent student

    --                 let (_ : rest@ (next : _)) = sortWith _ptTime transactions
    --                     pointSince             = next^.ptTime

    --                 -- Check that transactions aren't simultaneous.
    --                 for_ transactions $ \trans -> do
    --                     let sigSubmission = trans        ^.ptSignedSubmission
    --                         course        = assignment   ^.aCourseId

    --                     cId <- DB.createCourse           (simpleCourse course) `orIfItFails` getId course
    --                     _   <- DB.enrollStudentToCourse  studentId cId     `orIfItFails` ()
    --                     aId <- DB.createAssignment       assignment        `orIfItFails` getId assignment
    --                     _   <- DB.setStudentAssignment   studentId aId     `orIfItFails` ()
    --                     _   <- DB.createSignedSubmission sigSubmission     `orIfItFails` getId sigSubmission

    --                     ptId <- DB.createTransaction trans
    --                     return ptId

    --                 mblock <- DB.createPrivateBlock Nothing
    --                 let !_ = mblock ?: error "No private block created"

    --                 transPacksSince <- DB.getProvenStudentTransactions
    --                     def{ pfStudent = Just studentId, pfSince = Just pointSince }

    --                 let transSince = join $ map (map snd . snd) _transPacksSince

    --                 let equal = (==) `on` sortWith getId

    --                 (transSince `equal` rest) `assertThat`
    --                     ("Incorrect set of transactions is returned: "
    --                     <> show (length transSince) <> " vs "
    --                     <> show (length rest)
    --                     )

    --                 return $ conjoin $ transPacksSince <&> \(proof, txSet) ->
    --                     conjoin $ txSet <&> \(idx, tx) ->
    --                         counterexample ("Tx not present in tree: " <> show tx) $
    --                         MerkleTree.validateElementExistAt (fromIntegral idx) tx proof
