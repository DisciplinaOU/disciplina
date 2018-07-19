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
       , module Dscp.Educator.Txs
       , module Dscp.Util
       , hash
       ) where

import Prelude hiding (fold)

import Database.SQLite.Simple (Connection, execute, fold, query, setTrace, setTrace, withConnection,
                               withTransaction)


import Test.QuickCheck.Gen (generate)

import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType (..), Course (..), Grade (..),
                        SignedSubmission (..), Submission (..), SubmissionSig,
                        SubmissionWitness (..), aCourseId, mkGrade, sAssignment, sStudentId,
                        ssSubmission, ssWitness, swKey)
import Dscp.Crypto (hash)
import qualified Dscp.DB.SQLite.Class as Adapter
import Dscp.DB.SQLite.Schema (ensureSchemaIsSetUp)
import Dscp.Educator.Txs (PrivateTx (..), ptSignedSubmission, ptTime)
import Dscp.Util (idOf)

import qualified Loot.Log as Adapter

import Test.Common
import Test.Dscp.Core.Instances ()
import Test.Dscp.Crypto.Instances ()
import Test.Dscp.Educator.Instances ()

-- import System.Directory (removeFile)
-- import System.IO.Error (IOError, isDoesNotExistError)

newtype TestSQLiteM a
    = TestSQLiteM { getTestSQLiteM :: ReaderT Connection IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch,
              MonadFail, MonadReader Connection)

runTestSQLiteM :: TestSQLiteM a -> IO a
runTestSQLiteM action = do
    let filename = ":memory:"

    -- removeFile filename `catch` \(e :: IOError) -> do
    --    if isDoesNotExistError e
    --    then return ()
    --    else throwM e

    withConnection filename $ \conn -> do
        ensureSchemaIsSetUp conn
        getTestSQLiteM action `runReaderT` conn

sqliteProperty
    :: (Testable prop, Show a)
    => Arbitrary a
    => (a -> TestSQLiteM prop)
    -> Property
sqliteProperty action =
    property $ \input ->
    ioProperty $ do
        runTestSQLiteM (action input)

instance Adapter.MonadSQLiteDB TestSQLiteM where
    query   theQuery   args = TestSQLiteM $ ReaderT $ \conn -> query   conn theQuery   args
    execute theRequest args = TestSQLiteM $ ReaderT $ \conn -> execute conn theRequest args

    queryStreamed theQuery args seed op =
        TestSQLiteM $ ReaderT $ \conn ->
            fold conn theQuery args seed $ \a b ->
                getTestSQLiteM (op a b) `runReaderT` conn

    transaction (TestSQLiteM (ReaderT actor)) = do
        TestSQLiteM $ ReaderT $ \conn ->
            conn `withTransaction` do
                actor conn

    traced (TestSQLiteM (ReaderT actor)) = do
        TestSQLiteM $ ReaderT $ \conn -> do
            setTrace conn (Just putStrLn)
            res <- actor conn
            setTrace conn (Nothing)
            return res

instance Adapter.MonadLogging TestSQLiteM where
    log _ _ _ = pass
    logName = return $ error "Logger name requested in test"

instance Arbitrary AssignmentType    where arbitrary = elements [Regular, CourseFinal]
instance Arbitrary Grade             where arbitrary = elements [A, B, C, D, F]
instance Arbitrary Address           where arbitrary = (Address . hash . mkPubKey) <$> arbitrary
instance Arbitrary Course            where arbitrary = Course     <$> arbitrary
instance Arbitrary Assignment        where arbitrary = Assignment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary Submission        where arbitrary = Submission <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary PublicKey         where arbitrary = mkPubKey   <$> arbitrary
instance Arbitrary PrivateTx         where arbitrary = PrivateTx  <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary SubmissionWitness where arbitrary = SubmissionWitness <$> arbitrary <*> arbitrary
instance Arbitrary SignedSubmission  where arbitrary = SignedSubmission  <$> arbitrary <*> arbitrary

instance (Arbitrary a, HasSignature a) => Arbitrary (Signature a) where
    arbitrary = sign <$> (mkPrivKey <$> arbitrary) <*> arbitrary

instance (Arbitrary a, HasHash a) => Arbitrary (Hash a) where
    arbitrary = hash <$> arbitrary

orIfItFails :: MonadCatch m => m a -> a -> m a
orIfItFails action instead = do
    action `catch` \(_e :: SomeException) -> do
        return instead
