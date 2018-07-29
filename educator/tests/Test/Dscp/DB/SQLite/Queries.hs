{-# LANGUAGE ViewPatterns #-}

module Test.Dscp.DB.SQLite.Queries where

import Prelude hiding (toList)

import Control.Lens (mapped)

import qualified Dscp.Crypto.MerkleTree as MerkleTree
import Dscp.DB.SQLite as DB
import Dscp.Util (Id (..), allUniqueOrd)

import Test.Dscp.DB.SQLite.Common

spec_Instances :: Spec
spec_Instances = do
    describe "Basic database operations" $ do
        describe "Courses" $ do
            it "Course does not exist before it is created" $
                sqliteProperty $ \courseId -> do
                    isThere <- DB.existsCourse courseId
                    return (not isThere)

            it "Course does exist after it is created" $
                sqliteProperty $ \courseId -> do
                    _       <- DB.createCourse courseId (Just "foo") []
                    isThere <- DB.existsCourse courseId

                    return isThere

        describe "Students" $ do
            it "Student does not exist before she is created" $
                sqliteProperty $ \student -> do
                    isThere <- DB.existsStudent student
                    return (not isThere)

            it "Student does exist after she is created" $
                sqliteProperty $ \student -> do
                    _       <- DB.createStudent student
                    isThere <- DB.existsStudent student

                    return isThere

        describe "Assignments" $ do
            it "Assignment is created and retrieved by hash" $
                sqliteProperty $ \assignment -> do

                    _              <- DB.createCourse    (assignment^.aCourseId) Nothing []
                    assignmentHash <- DB.createAssignment assignment
                    assignment'    <- DB.getAssignment    assignmentHash

                    return (assignment' == Just assignment)

            it "Assignment is not created if course does not exist" $
                sqliteProperty $ \(assignment) -> do
                    throws @DomainError $ do
                        _ <- DB.createAssignment assignment
                        return ()

        describe "Submissions" $ do
            it "Submission is not created unless Assignment exist" $
                sqliteProperty $ \submission -> do
                    throws @DomainError $ do
                        _ <- DB.sqlTransaction $ DB.createSignedSubmission submission
                        return ()

            it "Submission is not created unless Student exist" $
                sqliteProperty $ \sigSubmission -> do

                    let submission = sigSubmission^.ssSubmission
                        assignment = submission   ^.sAssignment
                        course     = assignment   ^.aCourseId

                    _ <- DB.createCourse           course Nothing []
                    _ <- DB.createAssignment       assignment

                    throws @DomainError $ do
                        _ <- DB.sqlTransaction $ DB.createSignedSubmission sigSubmission
                        return ()

            it "Submission is not created unless StudentAssignment exist" $
                sqliteProperty $ \sigSubmission -> do

                    throws @DomainError $ do
                        let submission = sigSubmission^.ssSubmission
                            assignment = submission^.sAssignment
                            course     = assignment^.aCourseId
                            student    = submission^.sStudentId

                        _ <- DB.createCourse           course Nothing []
                        _ <- DB.createStudent          student
                        _ <- DB.createAssignment       assignment
                        _ <- sqlTransaction $
                             DB.createSignedSubmission sigSubmission

                        return ()

        describe "Transactions" $ do
            it "Transaction is created if all deps exist" $
                sqliteProperty $ \trans -> do

                    let sigSubmission = trans        ^.ptSignedSubmission
                        submission    = sigSubmission^.ssSubmission
                        assignment    = submission   ^.sAssignment
                        course        = assignment   ^.aCourseId
                        student       = submission   ^.sStudentId

                    courseId  <- DB.createCourse           course Nothing []
                    studentId <- DB.createStudent          student
                    _         <- DB.enrollStudentToCourse  studentId courseId
                    aHash     <- DB.createAssignment       assignment
                    _         <- DB.setStudentAssignment   student aHash
                    _         <- sqlTransaction $
                                 DB.createSignedSubmission sigSubmission
                    transHash <- DB.createTransaction      trans

                    trans'    <- DB.getTransaction transHash

                    return (trans' == Just trans && (getId <$> trans') == (getId <$> Just trans))

    describe "Concrete operations from domain" $ do
        it "getStudentCourses/enrollStudentToCourse" $ do
            sqliteProperty $ \
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
                    courseId <- DB.createCourse course (Just "foo") []
                    DB.enrollStudentToCourse studentId courseId

                courseIds' <- DB.getStudentCourses student

                return (sort (map getId courses) == sort courseIds')

        it "getStudentAssignments" $ do
            sqliteProperty $ \
                ( student
                , (delayedGen (vectorUnique 2)
                    -> [course1, course2])
                , (delayedGen (vectorUniqueOf 4 $ listOf1 @Assignment arbitrary)
                    -> [toHerCourse1, toHerCourse2,
                        notToHerCourse1, notToHerCourse2])
                ) -> do

                    studentId <- DB.createStudent student

                    courseId1 <- DB.createCourse course1 Nothing []
                    courseId2 <- DB.createCourse course2 Nothing []

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
            sqliteProperty $ \sigSubmission -> do

                let submission = sigSubmission^.ssSubmission
                    assignment = submission   ^.sAssignment
                    course     = assignment   ^.aCourseId
                    student    = submission   ^.sStudentId

                courseId  <- DB.createCourse           course Nothing []
                studentId <- DB.createStudent          student
                _         <- DB.enrollStudentToCourse  studentId courseId
                aHash     <- DB.createAssignment       assignment
                _         <- DB.setStudentAssignment   student aHash
                subHash   <- sqlTransaction $
                             DB.submitAssignment       sigSubmission

                sub'      <- DB.getSignedSubmission    subHash

                return (sub' == Just sigSubmission)

        it "getGradesForCourseAssignments" $
            sqliteProperty $ \(trans, course2) -> do

                let sigSubmission = trans        ^.ptSignedSubmission
                    submission    = sigSubmission^.ssSubmission
                    assignment    = submission   ^.sAssignment
                    course        = assignment   ^.aCourseId
                    student       = submission   ^.sStudentId

                    sigSubmission2 = sigSubmission & ssSubmission.sAssignment .~ assignment2
                    assignment2    = assignment    & aCourseId                .~ getId course2
                    trans2         = trans         & ptSignedSubmission       .~ sigSubmission2

                if  (assignment^.idOf /= assignment2^.idOf)
                then do
                    _studentId <- DB.createStudent          student

                    courseId   <- DB.createCourse           course Nothing []
                    courseId2  <- DB.createCourse           course2 Nothing []

                    _          <- DB.enrollStudentToCourse  student courseId
                    _          <- DB.enrollStudentToCourse  student courseId2

                    aHash      <- DB.createAssignment       assignment
                    aHash2     <- DB.createAssignment       assignment2

                    _          <- DB.setStudentAssignment   student aHash
                    _          <- DB.setStudentAssignment   student aHash2

                    _          <- sqlTransaction $
                                  DB.createSignedSubmission sigSubmission
                    _          <- sqlTransaction $
                                  DB.createSignedSubmission sigSubmission2

                    _          <- DB.createTransaction      trans
                    _          <- DB.createTransaction      trans2

                    transs2    <- DB.getGradesForCourseAssignments student courseId2
                    transs1    <- DB.getGradesForCourseAssignments student courseId

                    return (transs2 == [trans2] && transs1 == [trans])
                else do
                    return True

    describe "Retrieval of proven transactions" $ do
        it "getProvenStudentTransactionsSince" $
            sqliteProperty $ \
                ( trans1
                , trans2
                , trans3
                , student
                ) -> do
                    studentId <- DB.createStudent student

                    let transactions'          = [trans1, trans2, trans3]
                    let transactions           = transactions' & mapped.ptSignedSubmission.ssSubmission.sStudentId .~ studentId
                    let (_ : rest@ (next : _)) = sortWith _ptTime transactions
                        pointSince             = next^.ptTime

                    for_ transactions $ \trans -> do
                        let sigSubmission = trans        ^.ptSignedSubmission
                            submission    = sigSubmission^.ssSubmission
                            assignment    = submission   ^.sAssignment
                            course        = assignment   ^.aCourseId

                        cId <- DB.createCourse           course Nothing [] `orIfItFails` getId course
                        _   <- DB.enrollStudentToCourse  studentId cId     `orIfItFails` ()
                        aId <- DB.createAssignment       assignment        `orIfItFails` getId assignment
                        _   <- DB.setStudentAssignment   studentId aId     `orIfItFails` ()
                        _   <- sqlTransaction $
                               DB.createSignedSubmission sigSubmission     `orIfItFails` getId sigSubmission

                        ptId <- DB.createTransaction trans
                        return ptId

                    DB.createBlock Nothing

                    transPacksSince <- DB.getProvenStudentTransactionsSince studentId pointSince

                    let transSince = join . map (map snd . snd) $ transPacksSince

                    let equal = (==) `on` sortWith getId

                    (transSince `equal` rest) `assertThat`
                        "Incorrect set of transactions is returned"

                    return $ flip all transPacksSince $ \(proof, txSet) ->
                        flip all txSet $ \(idx, tx) ->
                            MerkleTree.validateElementExistAt idx tx proof
