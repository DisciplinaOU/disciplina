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

import Database.SQLite.Simple (Connection, execute, fold, query, setTrace, withConnection,
                               withTransaction)

import Test.QuickCheck.Gen (generate)

import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType (..), Course (..),
                        Grade (..), SignedSubmission (..), Submission (..), SubmissionSig,
                        SubmissionWitness (..), aCourseId, sAssignment,
                        sStudentId, ssSubmission, ssWitness, swKey)
import Dscp.Crypto (HasHash, HasSignature, Hash, PublicKey, Signature, hash, sign)
import qualified Dscp.DB.SQLite.Class as Adapter
import Dscp.DB.SQLite.Schema (ensureSchemaIsSetUp)
import Dscp.Educator.Txs (PrivateTx (..), ptSignedSubmission)
import Dscp.Util (idOf)

import Test.Common

--import System.Directory (removeFile)
--import System.IO.Error (IOError, isDoesNotExistError)

newtype TestSQLiteM a
    = TestSQLiteM { getTestSQLiteM :: ReaderT Connection IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

runTestSQLiteM :: TestSQLiteM a -> IO a
runTestSQLiteM action = do
    let filename = ":memory:"

    --removeFile filename `catch` \(e :: IOError) -> do
    --    if isDoesNotExistError e
    --    then return ()
    --    else throwM e

    withConnection filename $ \conn -> do
        ensureSchemaIsSetUp conn
        -- It stays here, so next time something breaks I don't have to
        -- add it to imports again.
        setTrace conn (Just putStrLn)
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

    transaction (TestSQLiteM (ReaderT actor)) = do
        TestSQLiteM $ ReaderT $ \conn ->
            conn `withTransaction` do
                actor conn

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

