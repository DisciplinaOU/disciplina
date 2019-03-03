#!/usr/bin/env bash

# This script runs ghci and prepares environment for common client testing operations
# on all our APIs.
#
# If you need authenticated requests, set AUTH_SEED environmental variable to
# secret key seed.

set -e -o pipefail

prepare_ghci=$(mktemp /tmp/educator-test.XXXXXX.ghci)

cat >$prepare_ghci <<EOL
import Dscp.Core
import Data.Default
import Servant.Client
import GHC.IO.Unsafe
import Test.QuickCheck (Arbitrary, generate, resize)

import Dscp.Crypto
import Dscp.Util.Test (arbitrary, detGen)
import Dscp.Witness.Web.Client


let authSeed = "$AUTH_SEED" :: ByteString
let authSk = guard (not $ null authSeed) $> mkSecretKeyData (secretFromSeed authSeed)

let eBaseUrl = BaseUrl Http "127.0.0.1" 8090 ""
let wBaseUrl = BaseUrl Http "127.0.0.1" 8091 ""

eCli' <- createEducatorApiClient eBaseUrl
let eCli@EducatorApiEndpoints{..} = eCli' authSk

sCli' <- createStudentApiClient eBaseUrl
let sCli@StudentApiEndpoints{..} = sCli' authSk

wCli@WitnessEndpoints{..} <- createWitnessClient wBaseUrl

putTextLn ""
putTextLn "Useful functions memo:"

-- | Generator of small values (affects list-like structures)
let small = resize 1 arbitrary

-- | Get an example object of a specified type
let seed s = detGen s small
:t seed

-- | Get an example object of a specified type (always the same)
let ex = seed (-1)
:t ex

-- | Generate a unique instance of a specified type.
let gen = unsafePerformIO $ generate small
:t gen

putTextLn ""
putTextLn "Try \`wPing\` or \`eAddCertificate ex\`"
putTextLn ""

EOL

cd educator || :
stack ghci disciplina-educator --ghci-options "-ghci-script $prepare_ghci"

rm $prepare_ghci
