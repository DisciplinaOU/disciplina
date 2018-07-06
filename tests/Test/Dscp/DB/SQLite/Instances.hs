
module Test.Dscp.DB.SQLite.Instances where

import Dscp.DB.SQLite as DB

import Test.Dscp.DB.SQLite.Common

spec_Instances :: Spec
spec_Instances = do
    describe "Database operations" $ do
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
                        _ <- DB.createSignedSubmission submission
                        return ()

            it "Submission is not created unless Student exist" $
                sqliteProperty $ \sigSubmission -> do

                    let submission = sigSubmission^.ssSubmission
                        assignment = submission   ^.sAssignment
                        course     = assignment   ^.aCourseId

                    _ <- DB.createCourse           course Nothing []
                    _ <- DB.createAssignment       assignment

                    throws @DomainError $ do
                        _ <- DB.createSignedSubmission sigSubmission
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
                        _ <- DB.createSignedSubmission sigSubmission

                        return ()

            it "Submission is created if all deps exist" $
                sqliteProperty $ \sigSubmission -> do

                    let submission = sigSubmission^.ssSubmission
                        assignment = submission   ^.sAssignment
                        course     = assignment   ^.aCourseId
                        student    = submission   ^.sStudentId

                    _       <- DB.createCourse           course Nothing []
                    _       <- DB.createStudent          student
                    aHash   <- DB.createAssignment       assignment
                    _       <- DB.setStudentAssignment   student aHash
                    subHash <- DB.createSignedSubmission sigSubmission

                    sub'    <- DB.getSignedSubmission    subHash

                    return (sub' == Just sigSubmission)

        describe "Transactions" $ do
            it "Submission is created if all deps exist" $
                sqliteProperty $ \trans -> do

                    let sigSubmission = trans        ^.ptSignedSubmission
                        submission    = sigSubmission^.ssSubmission
                        assignment    = submission   ^.sAssignment
                        course        = assignment   ^.aCourseId
                        student       = submission   ^.sStudentId

                    _         <- DB.createCourse           course Nothing []
                    _         <- DB.createStudent          student
                    aHash     <- DB.createAssignment       assignment
                    _         <- DB.setStudentAssignment   student aHash
                    _         <- DB.createSignedSubmission sigSubmission
                    transHash <- DB.createTransaction      trans

                    trans'    <- DB.getTransaction transHash

                    return (trans' == Just trans)

