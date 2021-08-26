module Test.Dscp.Witness.Serialise where

import Dscp.Core
import Dscp.Util.Test
import Dscp.Witness

spec_Serialise :: Spec
spec_Serialise = do
    describe "Witness datatypes JSON serialisation" $ do
        aesonRoundtripProp @(BlocksOrMempool ())
        aesonRoundtripProp @(Detailed Tx)
        aesonRoundtripProp @(Detailed PublicationTx)
        aesonRoundtripProp @(Detailed GTx)
        aesonRoundtripProp @TxInfo
        aesonRoundtripProp @(PaginatedList "mem" ())
        aesonRoundtripProp @TxList
        aesonRoundtripProp @PublicationList
        aesonRoundtripProp @HashIs
        aesonRoundtripProp @BlockList
        aesonRoundtripProp @BlockInfo
        aesonRoundtripProp @AccountInfo
