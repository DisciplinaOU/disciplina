{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Dscp.DB.SQLite.Common
       ( module Test.Common
       , module Test.Dscp.DB.SQLite.Common
       , module Dscp.Core.Types
       , hash
       ) where

import Prelude hiding (fold)

import Database.SQLite.Simple

import System.Directory (removeFile)
import System.IO.Error (IOError, isDoesNotExistError)

import Test.QuickCheck.Gen (generate)

import Dscp.Core.Types (
    Address (..),
    Assignment (..),
    AssignmentType (..),
    CourseId (..),
    --Grade (..),
    SignedSubmission (..),
    --StudentId (..),
    --SubjectId,
    Submission (..),
    SubmissionType (..),
    SubmissionSig,
    SubmissionWitness (..),
    --aAssignment,
    aCourseId,
    --aType,
    sAssignment,
    sStudentId,
    --sType,
    ssSubmission,
    ssWitness,
    swKey
    --swSig
  )
import qualified Dscp.DB.SQLite.Class as Adapter
import Dscp.DB.SQLite.Schema (ensureSchemaIsSetUp)
import Dscp.Crypto (
    Hash,
    PublicKey,
    Signature,
    HasSignature,
    HasHash,
    sign,
    hash
  )

import Test.Common

newtype TestSQLiteM a
    = TestSQLiteM { getTestSQLiteM :: ReaderT Connection IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

runTestSQLiteM :: TestSQLiteM a -> IO a
runTestSQLiteM action = do
    let filename = "test.db"

    removeFile filename `catch` \(e :: IOError) -> do
        if isDoesNotExistError e
        then return ()
        else throwM e

    withConnection filename $ \conn -> do
        ensureSchemaIsSetUp conn
        setTrace conn (Just print)
        getTestSQLiteM action `runReaderT` conn

sqliteProperty
    :: Testable prop
    => Arbitrary a
    => (a -> TestSQLiteM prop)
    -> Property
sqliteProperty action =
    ioProperty $ do
        input <- generate arbitrary
        runTestSQLiteM (action input)

throws :: forall e m . (MonadCatch m, Exception e) => m () -> m Bool
throws action = do
    (action >> return False) `catch` \(_ :: e) ->
        return True

instance Adapter.MonadSQLiteDB TestSQLiteM where
    query   theQuery   args = TestSQLiteM $ ReaderT $ \conn -> query   conn theQuery   args
    execute theRequest args = TestSQLiteM $ ReaderT $ \conn -> execute conn theRequest args

    queryStreamed theQuery args seed op =
        TestSQLiteM $ ReaderT $ \conn ->
            fold conn theQuery args seed $ \a b ->
                getTestSQLiteM (op a b) `runReaderT` conn

instance Arbitrary CourseId where
    arbitrary = CourseId <$> arbitrary

instance Arbitrary Address where
    arbitrary = (Address . hash . mkPubKey) <$> arbitrary

instance Arbitrary Assignment where
    arbitrary = Assignment <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AssignmentType where
    arbitrary = elements [Regular, CourseFinal]

instance Arbitrary SubmissionType where
    arbitrary = elements [Digital, Offline]

instance Arbitrary Submission where
    arbitrary = Submission <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SubmissionWitness where
    arbitrary = SubmissionWitness <$> arbitrary <*> arbitrary

instance Arbitrary SignedSubmission where
    arbitrary = SignedSubmission <$> arbitrary <*> arbitrary

instance Arbitrary PublicKey where
    arbitrary = mkPubKey <$> arbitrary

instance (Arbitrary a, HasSignature a) => Arbitrary (Signature a) where
    arbitrary = sign <$> (mkPrivKey <$> arbitrary) <*> arbitrary

instance (Arbitrary a, HasHash a) => Arbitrary (Hash a) where
    arbitrary = hash <$> arbitrary