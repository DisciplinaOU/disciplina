-- | Common logic for educator and student API's.

module Dscp.Educator.Web.Logic
    ( commonGetProofs
    , getEducatorStatus
    , checkFairCV
    , checkFairCVPDF
    ) where

import Universum

import Control.Lens (each, to)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (MonadLogging, logDebug)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL
import Dscp.Educator.DB
import Dscp.Educator.Logic.Certificates
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types
import Dscp.Util.Aeson
import Dscp.Web
import qualified Pdf.Scanner as Pdf

commonGetProofs
    :: MonadEducatorWebQuery m
    => GetProvenStudentTransactionsFilters
    -> DBT 'WithinTx m [BlkProofInfo]
commonGetProofs filters = do
    rawProofs <- getProvenStudentTransactions filters
    return
        [ BlkProofInfo
          { bpiBlockHash = bHash
          , bpiMtreeSerialized = EncodeSerialised mtree
          , bpiTxs = txs
          }
        | (bHash, mtree, txs) <- rawProofs
        ]

getEducatorStatus
    :: MonadEducatorWeb ctx m
    => m EducatorInfo
getEducatorStatus = do
    eiAddress <- view $ lensOf @PubAddress
    return EducatorInfo {..}


-- | Check that a @'FairCV'@ is fully valid internally.
checkFairCV
    :: FairCV -> FairCVCheckResult
checkFairCV =
    buildResults . checkCV . readyFairCV
  where
    checkCV (FairCV sAddr _ cv) =
        M.mapWithKey (M.mapWithKey . checkProofPure sAddr) cv

    -- checkProofAgainstDB sAddr eAddr h proof =
    --     maybe False (checkProofPure sAddr eAddr proof) <$>
    --         runSdMempoolLocked (getPublicationByHeaderHash h)

    checkProofPure sAddr _ _ (TxIdAnnotated mTxId proof) =
        -- let root = ptw ^. ptwTxL.ptHeaderL.pbhBodyProof
            -- pubAuthor = ptw ^. ptwTxL.ptAuthorL
        -- in verifyPubTxWitnessed ptw &&
           -- eAddr == pubAuthor &&
           -- root == mprRoot proof &&
        TxIdAnnotated mTxId $
            all (checkTxSubmission sAddr) (mprProof proof) &&
            isJust mTxId

    checkTxSubmission sAddr (PrivateTx sSub _ _) =
        isRight $ verifyStudentSubmission sAddr sSub

    buildResults results =
        FairCVCheckResult
        { fairCVCheckResults = results
        , fairCVFullyValid = all (and . fmap tiaVal) results
        }

checkFairCVPDF
    :: (MonadThrow m, MonadLogging m)
    => Pdf.PDFBody -> m FairCVAndCheckResult
checkFairCVPDF pdf = do
    (fairCV, Pdf.PDFBody source) <- maybe (throwM InvalidFormat) pure $ extractFairCVFromCert pdf

    let checkRes       = checkFairCV fairCV
        pdfHash        = fairCV ^? fcCVL.each.each.tiaValL.to(toList).each.ptSignedSubmission.ssSubmission.sContentsHash
        sourceHash     = hash source
        pdfHashIsValid = pdfHash == Just (coerce sourceHash)

    logDebug $ "Original fairCV check result: "+|checkRes|+";"
    logDebug $ "PDF hash inside FairCV: "+|pdfHash|+";"
    logDebug $ "Calculated source hash: "+|sourceHash|+";"
    logDebug $ "Is hash valid? "+|pdfHashIsValid|+"."

    return $ FairCVAndCheckResult (readyFairCV fairCV) checkRes
        { fairCVFullyValid = fairCVFullyValid checkRes && pdfHashIsValid
        }
