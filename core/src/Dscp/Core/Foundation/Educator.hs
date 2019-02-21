-- | Private-chain (educator) datatypes definition.

{-# LANGUAGE NumDecimals #-}

module Dscp.Core.Foundation.Educator
    (

    -- * Common types
      ItemDesc (..)
    , isValidItemDesc
    , toItemDesc
    , toItemDescUnsafe
    , Timestamp (..)
    , toTimestamp
    , toTimestampUnsafe
    , Course (..)
    , Subject (..)
    , Student
    , Grade (..)
    , mkGrade
    , Language (..)
    , gradeToNum
    , Assignment (..)
    , AssignmentType (..)
    , Submission (..)
    , offlineHash
    , DocumentType (..)
    , SubmissionSig
    , SignedSubmission (..)
    , SubmissionWitness (..)
    , CertificateFullInfo (..)
    , CertificateMeta (..)
    , CertificateGrade (..)
    , CertificateIssuerInfo (..)
    , SignedCertificateGrade (..)
    , Certificate (..)
    , CertificateName (..)
    , StudentInfo (..)
    , GradeInfo (..)
    , EducationForm (..)
    , GradingScale (..)
    , documentType
    , _aDocumentType
    , aDocumentType
    , aCourseId
    , aContentsHash
    , aType
    , aDesc
    , _sDocumentType
    , sDocumentType
    , sStudentId
    , sContentsHash
    , sAssignmentHash
    , swKey
    , swSig
    , ssSubmission
    , ssWitness

    -- * Private transactions
    , PrivateTx (..)
    , PrivateGrade (..)
    , PrivateCertification (..)
    , PrivateTxWitness (..)
    , PrivateTxAux (..)
    , _PrivateTxGrade
    , _PrivateTxCertification
    , getPrivateTxType
    , ptxTypeGrade
    , ptxTypeCertification

    -- * Lenses
    , ptaTx
    , ptaWitness
    , ptGrade
    , ptSignedSubmission
    , ptTime
    , ptwKey
    , ptwSig
    , pcGrade
    , pcStudent
    , scgKey
    , scgSig
    , scgCertificateGrade

    -- * Basic types
    , PrivateHeaderHash
    , PrivateBlockHeader (..)
    , pbhPrevBlock
    , pbhBodyProof
    , pbhAtgDelta
    , PrivateBlockBody (..)
    , pbbTxs
    , PrivateBlock (..)
    , pbHeader
    , pbBody

      -- * Constants
    , genesisHeaderHash

      -- * Helpers
    , getPrevBlockRefMaybe

    -- * Activity Type Graph
    , ATGSubjectChange (..)
    , ATGDelta (..)
    , isEmptyATGDelta
    , ATGNode (..)
    , atgnSubjectId
    , atgnChildren
    , ATGEdge (..)
    , atgeWeight
    , atgeChild
    , ATG (..)
    ) where

import Dscp.Core.Foundation.Educator.ATG as M
import Dscp.Core.Foundation.Educator.Assignment as M
import Dscp.Core.Foundation.Educator.ATGDelta as M
import Dscp.Core.Foundation.Educator.Certificate as M
import Dscp.Core.Foundation.Educator.Course as M
import Dscp.Core.Foundation.Educator.DocumentType as M
import Dscp.Core.Foundation.Educator.Educator as M
import Dscp.Core.Foundation.Educator.Grade as M
import Dscp.Core.Foundation.Educator.GradeInfo as M
import Dscp.Core.Foundation.Educator.ItemDesc as M
import Dscp.Core.Foundation.Educator.Orphans ()
import Dscp.Core.Foundation.Educator.Student as M
import Dscp.Core.Foundation.Educator.Subject as M
import Dscp.Core.Foundation.Educator.Submission as M
import Dscp.Core.Foundation.Educator.Timestamp as M
import Dscp.Core.Foundation.Educator.PrivateTx as M
import Dscp.Core.Foundation.Educator.PrivateBlock as M
